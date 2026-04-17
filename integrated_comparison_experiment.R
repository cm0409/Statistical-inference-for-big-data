# ============================================================================
# 三类大数据统计方法横向对比与综合使用实验
# 覆盖：分布式（SAE/One-step）、子抽样（SRS/Stratified/BLB）、小批次（SGD）
# 主线：模拟数据 + 实证数据（NYC Taxi）
# ============================================================================

rm(list = ls())

# ----------------------------------------------------------------------------
# 1) 依赖与环境
# ----------------------------------------------------------------------------
packages <- c("MASS", "dplyr", "ggplot2", "arrow", "scales", "viridis", "tidyr")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))

theme_set(
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
)

# ----------------------------------------------------------------------------
# 2) 通用工具
# ----------------------------------------------------------------------------
safe_solve <- function(mat, ridge = 1e-6) {
  tryCatch(
    solve(mat),
    error = function(e) solve(mat + diag(ridge, nrow(mat)))
  )
}

safe_coef <- function(model, coef_name, fallback = NA_real_) {
  cf <- coef(model)
  if (coef_name %in% names(cf)) {
    return(unname(cf[[coef_name]]))
  }
  fallback
}

build_design <- function(df, feature_cols) {
  as.matrix(cbind(1, df[, feature_cols, drop = FALSE]))
}

clip01 <- function(x) pmin(1, pmax(0, x))

summarize_method_results <- function(results_df) {
  results_df |>
    dplyr::group_by(track, category, method, config_id) |>
    dplyr::summarise(
      runs = dplyr::n(),
      mean_runtime_sec = mean(runtime_sec, na.rm = TRUE),
      sd_runtime_sec = sd(runtime_sec, na.rm = TRUE),
      mean_sample_access = mean(sample_accesses, na.rm = TRUE),
      mean_memory_mb = mean(memory_mb, na.rm = TRUE),
      mean_abs_error_mean = mean(mean_abs_error, na.rm = TRUE),
      mean_abs_error_coef = mean(coef_abs_error, na.rm = TRUE),
      mean_ci_cover_mean = mean(mean_ci_cover, na.rm = TRUE),
      mean_ci_cover_coef = mean(coef_ci_cover, na.rm = TRUE),
      mean_ci_width_mean = mean(mean_ci_width, na.rm = TRUE),
      mean_ci_width_coef = mean(coef_ci_width, na.rm = TRUE),
      mean_mse = mean(mse, na.rm = TRUE),
      mean_r2 = mean(r2, na.rm = TRUE),
      stability_mean_sd = sd(mean_estimate, na.rm = TRUE),
      stability_coef_sd = sd(coef_estimate, na.rm = TRUE),
      stability_mse_sd = sd(mse, na.rm = TRUE),
      .groups = "drop"
    )
}

compute_budget_views <- function(summary_df) {
  score_df <- summary_df |>
    dplyr::mutate(
      time_eff = scales::rescale(-mean_runtime_sec, to = c(0, 1)),
      memory_eff = scales::rescale(-mean_memory_mb, to = c(0, 1)),
      access_eff = scales::rescale(-mean_sample_access, to = c(0, 1)),
      acc_score = scales::rescale(-mean_abs_error_coef, to = c(0, 1)),
      stability_score = scales::rescale(-stability_coef_sd, to = c(0, 1)),
      cov_score = clip01(mean_ci_cover_coef),
      score_time_budget = 0.45 * time_eff + 0.35 * acc_score + 0.20 * cov_score,
      score_memory_budget = 0.45 * memory_eff + 0.35 * acc_score + 0.20 * stability_score,
      score_access_budget = 0.45 * access_eff + 0.35 * acc_score + 0.20 * cov_score
    )

  score_df |>
    dplyr::arrange(track, dplyr::desc(score_time_budget))
}

get_reference_best <- function(summary_df) {
  summary_df |>
    dplyr::filter(category != "Hybrid") |>
    dplyr::group_by(track) |>
    dplyr::slice_min(order_by = mean_abs_error_coef, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(
      track,
      ref_method = method,
      ref_config = config_id,
      ref_error = mean_abs_error_coef,
      ref_runtime = mean_runtime_sec,
      ref_ci_cover = mean_ci_cover_coef
    )
}

# ----------------------------------------------------------------------------
# 3) 数据生成与加载
# ----------------------------------------------------------------------------
generate_ar1_cov <- function(p, rho = 0.5) {
  Sigma <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      Sigma[i, j] <- rho^abs(i - j)
    }
  }
  Sigma
}

generate_simulation_data <- function(
    n = 10000,
    p = 8,
    rho = 0.5,
    sigma = 1.0,
    nonlinear_strength = 0.2,
    seed = 42
) {
  set.seed(seed)
  Sigma <- generate_ar1_cov(p, rho)
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
  beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)
  Xd <- cbind(1, X)
  nonlinear_part <- nonlinear_strength * (0.8 * X[, "x1"]^2 - 0.6 * abs(X[, "x2"]))
  y <- as.vector(Xd %*% beta_true + nonlinear_part + rnorm(n, 0, sigma))

  data.frame(y = y, X, check.names = FALSE) |>
    dplyr::mutate(track = "Simulation", .before = 1)
}

load_empirical_data <- function(path, n_max = 150000, seed = 42) {
  if (!file.exists(path)) {
    stop("实证数据文件不存在: ", path)
  }

  set.seed(seed)
  taxi_raw <- arrow::read_parquet(path)
  taxi_clean <- taxi_raw |>
    dplyr::filter(
      fare_amount > 0, fare_amount < 500,
      trip_distance > 0, trip_distance < 100,
      tip_amount >= 0, tip_amount < 200,
      passenger_count > 0, passenger_count <= 6
    ) |>
    dplyr::select(fare_amount, trip_distance, passenger_count, tip_amount, payment_type)

  if (nrow(taxi_clean) > n_max) {
    taxi_clean <- dplyr::slice_sample(taxi_clean, n = n_max)
  }

  taxi_clean |>
    dplyr::transmute(
      track = "Empirical",
      y = fare_amount,
      x1 = trip_distance,
      x2 = passenger_count,
      x3 = tip_amount,
      payment_type = as.character(payment_type)
    )
}

# ----------------------------------------------------------------------------
# 4) 方法实现
# ----------------------------------------------------------------------------
fit_linear_and_ci <- function(train_df, eval_df, feature_cols, coef_name, true_mean, true_coef) {
  t0 <- Sys.time()
  formula_obj <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))
  fit <- lm(formula_obj, data = train_df)
  runtime <- as.numeric(Sys.time() - t0, units = "secs")

  pred <- predict(fit, newdata = eval_df)
  mse <- mean((eval_df$y - pred)^2)
  r2 <- 1 - sum((eval_df$y - pred)^2) / sum((eval_df$y - mean(eval_df$y))^2)

  est_mean <- mean(train_df$y)
  se_mean <- sd(train_df$y) / sqrt(nrow(train_df))
  ci_mean <- c(est_mean - 1.96 * se_mean, est_mean + 1.96 * se_mean)

  coef_est <- safe_coef(fit, coef_name)
  coef_se <- tryCatch(summary(fit)$coefficients[coef_name, "Std. Error"], error = function(e) NA_real_)
  ci_coef <- c(coef_est - 1.96 * coef_se, coef_est + 1.96 * coef_se)

  list(
    runtime_sec = runtime,
    mean_estimate = est_mean,
    mean_abs_error = abs(est_mean - true_mean),
    mean_ci_cover = as.numeric(true_mean >= ci_mean[1] && true_mean <= ci_mean[2]),
    mean_ci_width = diff(ci_mean),
    coef_estimate = coef_est,
    coef_abs_error = abs(coef_est - true_coef),
    coef_ci_cover = as.numeric(true_coef >= ci_coef[1] && true_coef <= ci_coef[2]),
    coef_ci_width = diff(ci_coef),
    mse = mse,
    r2 = r2,
    sample_accesses = nrow(train_df),
    memory_mb = as.numeric(object.size(train_df) / 1024^2),
    loss_history = NULL
  )
}

run_distributed_sae <- function(df, feature_cols, coef_name, true_mean, true_coef, K = 10) {
  n <- nrow(df)
  idx <- sample(rep(1:K, length.out = n))
  local_coef <- list()
  local_means <- numeric(K)
  t0 <- Sys.time()
  formula_obj <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))

  for (k in 1:K) {
    part <- df[idx == k, , drop = FALSE]
    local_fit <- lm(formula_obj, data = part)
    local_coef[[k]] <- coef(local_fit)
    local_means[k] <- mean(part$y)
  }

  coef_names <- unique(unlist(lapply(local_coef, names)))
  aligned <- matrix(0, nrow = K, ncol = length(coef_names))
  colnames(aligned) <- coef_names
  for (k in 1:K) {
    aligned[k, names(local_coef[[k]])] <- local_coef[[k]]
  }
  beta <- colMeans(aligned)
  runtime <- as.numeric(Sys.time() - t0, units = "secs")

  Xd <- build_design(df, feature_cols)
  beta_full <- beta[c("(Intercept)", feature_cols)]
  pred <- as.vector(Xd %*% beta_full)

  coef_local <- aligned[, coef_name]
  coef_est <- mean(coef_local, na.rm = TRUE)
  coef_se <- sd(coef_local, na.rm = TRUE) / sqrt(K)
  ci_coef <- c(coef_est - 1.96 * coef_se, coef_est + 1.96 * coef_se)

  mean_est <- mean(local_means)
  mean_se <- sd(local_means) / sqrt(K)
  ci_mean <- c(mean_est - 1.96 * mean_se, mean_est + 1.96 * mean_se)

  list(
    runtime_sec = runtime,
    mean_estimate = mean_est,
    mean_abs_error = abs(mean_est - true_mean),
    mean_ci_cover = as.numeric(true_mean >= ci_mean[1] && true_mean <= ci_mean[2]),
    mean_ci_width = diff(ci_mean),
    coef_estimate = coef_est,
    coef_abs_error = abs(coef_est - true_coef),
    coef_ci_cover = as.numeric(true_coef >= ci_coef[1] && true_coef <= ci_coef[2]),
    coef_ci_width = diff(ci_coef),
    mse = mean((df$y - pred)^2),
    r2 = 1 - sum((df$y - pred)^2) / sum((df$y - mean(df$y))^2),
    sample_accesses = n,
    memory_mb = as.numeric(object.size(aligned) / 1024^2),
    communication_proxy = K * length(feature_cols)
  )
}

run_distributed_onestep <- function(df, feature_cols, coef_name, true_mean, true_coef, K = 10, pilot_n = 1500, beta_init = NULL) {
  n <- nrow(df)
  p <- length(feature_cols)
  t0 <- Sys.time()
  formula_obj <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))

  if (is.null(beta_init)) {
    pilot_idx <- sample(seq_len(n), min(pilot_n, n))
    pilot_fit <- lm(formula_obj, data = df[pilot_idx, , drop = FALSE])
    beta0 <- coef(pilot_fit)[c("(Intercept)", feature_cols)]
  } else {
    beta0 <- beta_init[c("(Intercept)", feature_cols)]
  }

  idx <- sample(rep(1:K, length.out = n))
  grads <- matrix(0, nrow = K, ncol = p + 1)
  colnames(grads) <- c("(Intercept)", feature_cols)

  for (k in 1:K) {
    part <- df[idx == k, , drop = FALSE]
    Xd <- build_design(part, feature_cols)
    resid <- part$y - Xd %*% beta0
    grads[k, ] <- as.vector(-2 * t(Xd) %*% resid / nrow(part))
  }

  avg_grad <- colMeans(grads)
  Xd_all <- build_design(df, feature_cols)
  fisher <- 2 * t(Xd_all) %*% Xd_all / n
  beta1 <- beta0 - safe_solve(fisher) %*% avg_grad
  beta1 <- as.vector(beta1)
  names(beta1) <- c("(Intercept)", feature_cols)

  pred <- as.vector(Xd_all %*% beta1)
  runtime <- as.numeric(Sys.time() - t0, units = "secs")

  rss <- sum((df$y - pred)^2)
  sigma2 <- rss / max(1, n - (p + 1))
  vcov_beta <- sigma2 * safe_solve(t(Xd_all) %*% Xd_all)
  coef_idx <- which(colnames(Xd_all) == coef_name)
  coef_est <- beta1[coef_name]
  coef_se <- sqrt(vcov_beta[coef_idx, coef_idx])
  ci_coef <- c(coef_est - 1.96 * coef_se, coef_est + 1.96 * coef_se)

  mean_est <- mean(df$y)
  mean_se <- sd(df$y) / sqrt(n)
  ci_mean <- c(mean_est - 1.96 * mean_se, mean_est + 1.96 * mean_se)

  list(
    runtime_sec = runtime,
    mean_estimate = mean_est,
    mean_abs_error = abs(mean_est - true_mean),
    mean_ci_cover = as.numeric(true_mean >= ci_mean[1] && true_mean <= ci_mean[2]),
    mean_ci_width = diff(ci_mean),
    coef_estimate = coef_est,
    coef_abs_error = abs(coef_est - true_coef),
    coef_ci_cover = as.numeric(true_coef >= ci_coef[1] && true_coef <= ci_coef[2]),
    coef_ci_width = diff(ci_coef),
    mse = mean((df$y - pred)^2),
    r2 = 1 - sum((df$y - pred)^2) / sum((df$y - mean(df$y))^2),
    sample_accesses = n + min(pilot_n, n),
    memory_mb = as.numeric(object.size(grads) / 1024^2),
    beta = beta1
  )
}

run_subsample_stratified <- function(df, feature_cols, coef_name, true_mean, true_coef, sample_frac = 0.1, strata_n = 4) {
  strata <- cut(df[[feature_cols[1]]], breaks = quantile(df[[feature_cols[1]]], probs = seq(0, 1, length.out = strata_n + 1)), include.lowest = TRUE)
  n_take <- max(200, floor(nrow(df) * sample_frac))
  n_per_stratum <- max(10, floor(n_take / strata_n))
  sample_df <- df |>
    dplyr::mutate(.strata = strata) |>
    dplyr::group_by(.strata) |>
    dplyr::slice_sample(n = min(n_per_stratum, dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::select(-.strata)

  fit_linear_and_ci(sample_df, df, feature_cols, coef_name, true_mean, true_coef)
}

run_blb <- function(df, feature_cols, coef_name, true_mean, true_coef, gamma = 0.7, s = 10, B = 80) {
  n <- nrow(df)
  m <- floor(n^gamma)
  formula_obj <- as.formula(paste("y ~", paste(feature_cols, collapse = " + ")))
  t0 <- Sys.time()

  coef_store <- matrix(NA_real_, nrow = s, ncol = B)
  mean_store <- matrix(NA_real_, nrow = s, ncol = B)
  for (i in 1:s) {
    sub_idx <- sample(seq_len(n), m, replace = FALSE)
    sub <- df[sub_idx, , drop = FALSE]
    for (b in 1:B) {
      boot_idx <- sample(seq_len(m), m, replace = TRUE)
      boot <- sub[boot_idx, , drop = FALSE]
      fit <- lm(formula_obj, data = boot)
      coef_store[i, b] <- safe_coef(fit, coef_name)
      mean_store[i, b] <- mean(boot$y)
    }
  }

  coef_vec <- as.vector(coef_store)
  mean_vec <- as.vector(mean_store)
  coef_est <- mean(coef_vec, na.rm = TRUE)
  coef_se <- sd(coef_vec, na.rm = TRUE)
  mean_est <- mean(mean_vec, na.rm = TRUE)
  mean_se <- sd(mean_vec, na.rm = TRUE)
  runtime <- as.numeric(Sys.time() - t0, units = "secs")

  formula_fit <- lm(formula_obj, data = df)
  pred <- predict(formula_fit, newdata = df)

  ci_coef <- c(coef_est - 1.96 * coef_se, coef_est + 1.96 * coef_se)
  ci_mean <- c(mean_est - 1.96 * mean_se, mean_est + 1.96 * mean_se)

  list(
    runtime_sec = runtime,
    mean_estimate = mean_est,
    mean_abs_error = abs(mean_est - true_mean),
    mean_ci_cover = as.numeric(true_mean >= ci_mean[1] && true_mean <= ci_mean[2]),
    mean_ci_width = diff(ci_mean),
    coef_estimate = coef_est,
    coef_abs_error = abs(coef_est - true_coef),
    coef_ci_cover = as.numeric(true_coef >= ci_coef[1] && true_coef <= ci_coef[2]),
    coef_ci_width = diff(ci_coef),
    mse = mean((df$y - pred)^2),
    r2 = 1 - sum((df$y - pred)^2) / sum((df$y - mean(df$y))^2),
    sample_accesses = s * B * m,
    memory_mb = as.numeric(object.size(coef_store) / 1024^2),
    beta_init = c("(Intercept)" = coef(formula_fit)[1], setNames(rep(0, length(feature_cols)), feature_cols))
  )
}

run_sgd <- function(df, feature_cols, coef_name, true_mean, true_coef, batch_size = 128, epochs = 25, lr = 0.01, decay = 0.99, beta_init = NULL) {
  n <- nrow(df)
  p <- length(feature_cols)
  Xd <- build_design(df, feature_cols)
  y <- df$y

  if (is.null(beta_init)) {
    beta <- rnorm(p + 1, 0, 0.1)
    names(beta) <- c("(Intercept)", feature_cols)
  } else {
    beta <- beta_init[c("(Intercept)", feature_cols)]
  }

  t0 <- Sys.time()
  iter <- 1
  max_iter <- epochs * ceiling(n / batch_size)
  loss_history <- numeric(max_iter)
  lr_now <- lr

  for (ep in 1:epochs) {
    ord <- sample(seq_len(n))
    Xs <- Xd[ord, , drop = FALSE]
    ys <- y[ord]
    n_batches <- ceiling(n / batch_size)

    for (b in 1:n_batches) {
      i1 <- (b - 1) * batch_size + 1
      i2 <- min(b * batch_size, n)
      Xb <- Xs[i1:i2, , drop = FALSE]
      yb <- ys[i1:i2]
      resid <- yb - Xb %*% beta
      grad <- -2 * t(Xb) %*% resid / length(yb)
      beta <- beta - lr_now * as.vector(grad)

      loss_history[iter] <- mean((y - Xd %*% beta)^2)
      iter <- iter + 1
    }
    lr_now <- lr_now * decay
  }

  runtime <- as.numeric(Sys.time() - t0, units = "secs")
  loss_history <- loss_history[1:(iter - 1)]
  pred <- as.vector(Xd %*% beta)
  mse <- mean((y - pred)^2)
  r2 <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)

  rss <- sum((y - pred)^2)
  sigma2 <- rss / max(1, n - (p + 1))
  vcov_beta <- sigma2 * safe_solve(t(Xd) %*% Xd)
  coef_idx <- which(colnames(Xd) == coef_name)
  coef_est <- beta[coef_name]
  coef_se <- sqrt(vcov_beta[coef_idx, coef_idx])
  ci_coef <- c(coef_est - 1.96 * coef_se, coef_est + 1.96 * coef_se)

  mean_est <- mean(y)
  mean_se <- sd(y) / sqrt(n)
  ci_mean <- c(mean_est - 1.96 * mean_se, mean_est + 1.96 * mean_se)

  list(
    runtime_sec = runtime,
    mean_estimate = mean_est,
    mean_abs_error = abs(mean_est - true_mean),
    mean_ci_cover = as.numeric(true_mean >= ci_mean[1] && true_mean <= ci_mean[2]),
    mean_ci_width = diff(ci_mean),
    coef_estimate = coef_est,
    coef_abs_error = abs(coef_est - true_coef),
    coef_ci_cover = as.numeric(true_coef >= ci_coef[1] && true_coef <= ci_coef[2]),
    coef_ci_width = diff(ci_coef),
    mse = mse,
    r2 = r2,
    sample_accesses = epochs * n,
    memory_mb = as.numeric(object.size(Xd) / 1024^2),
    loss_history = loss_history,
    beta = beta
  )
}

# ----------------------------------------------------------------------------
# 5) 实验主循环
# ----------------------------------------------------------------------------
run_track_experiment <- function(track_name, df, feature_cols, coef_name, true_mean, true_coef, cfg, seed_base = 2026) {
  methods <- list(
    list(category = "Distributed", method = "SAE", config_id = "K5", fn = function(d) run_distributed_sae(d, feature_cols, coef_name, true_mean, true_coef, K = 5)),
    list(category = "Distributed", method = "SAE", config_id = "K20", fn = function(d) run_distributed_sae(d, feature_cols, coef_name, true_mean, true_coef, K = 20)),
    list(category = "Distributed", method = "One-step", config_id = "K10", fn = function(d) run_distributed_onestep(d, feature_cols, coef_name, true_mean, true_coef, K = 10, pilot_n = cfg$pilot_n)),
    list(category = "Subsampling", method = "SRS", config_id = "frac0.05", fn = function(d) {
      m <- max(300, floor(nrow(d) * 0.05))
      sub <- dplyr::slice_sample(d, n = m)
      fit_linear_and_ci(sub, d, feature_cols, coef_name, true_mean, true_coef)
    }),
    list(category = "Subsampling", method = "Stratified", config_id = "frac0.10", fn = function(d) run_subsample_stratified(d, feature_cols, coef_name, true_mean, true_coef, sample_frac = 0.10)),
    list(category = "Subsampling", method = "BLB", config_id = sprintf("g%.1f_s%d_B%d", cfg$blb_gamma, cfg$blb_s, cfg$blb_B), fn = function(d) run_blb(d, feature_cols, coef_name, true_mean, true_coef, gamma = cfg$blb_gamma, s = cfg$blb_s, B = cfg$blb_B)),
    list(category = "Mini-batch", method = "SGD", config_id = "bs64", fn = function(d) run_sgd(d, feature_cols, coef_name, true_mean, true_coef, batch_size = 64, epochs = cfg$sgd_epochs, lr = 0.01)),
    list(category = "Mini-batch", method = "SGD", config_id = "bs256", fn = function(d) run_sgd(d, feature_cols, coef_name, true_mean, true_coef, batch_size = 256, epochs = cfg$sgd_epochs, lr = 0.01))
  )

  rows <- list()
  convergence_rows <- list()
  row_id <- 1

  for (mtd in methods) {
    for (r in seq_len(cfg$reps)) {
      set.seed(seed_base + row_id + r)
      res <- mtd$fn(df)

      rows[[length(rows) + 1]] <- data.frame(
        track = track_name,
        category = mtd$category,
        method = mtd$method,
        config_id = mtd$config_id,
        rep = r,
        runtime_sec = res$runtime_sec,
        sample_accesses = res$sample_accesses,
        memory_mb = res$memory_mb,
        mean_estimate = res$mean_estimate,
        mean_abs_error = res$mean_abs_error,
        mean_ci_cover = res$mean_ci_cover,
        mean_ci_width = res$mean_ci_width,
        coef_estimate = res$coef_estimate,
        coef_abs_error = res$coef_abs_error,
        coef_ci_cover = res$coef_ci_cover,
        coef_ci_width = res$coef_ci_width,
        mse = res$mse,
        r2 = res$r2
      )

      if (!is.null(res$loss_history)) {
        keep <- round(seq(1, length(res$loss_history), length.out = min(180, length(res$loss_history))))
        convergence_rows[[length(convergence_rows) + 1]] <- data.frame(
          track = track_name,
          method = paste0(mtd$method, "_", mtd$config_id),
          iteration = keep,
          loss = res$loss_history[keep]
        )
      }
    }
    row_id <- row_id + 1
  }

  # 综合方案A：BLB -> One-step
  for (r in seq_len(cfg$reps)) {
    set.seed(seed_base + 700 + r)
    blb_res <- run_blb(df, feature_cols, coef_name, true_mean, true_coef, gamma = cfg$blb_gamma, s = cfg$blb_s, B = cfg$blb_B)
    beta_seed <- c("(Intercept)" = mean(df$y), setNames(rep(0, length(feature_cols)), feature_cols))
    beta_seed[coef_name] <- blb_res$coef_estimate
    one_res <- run_distributed_onestep(df, feature_cols, coef_name, true_mean, true_coef, K = 10, pilot_n = cfg$pilot_n, beta_init = beta_seed)

    rows[[length(rows) + 1]] <- data.frame(
      track = track_name,
      category = "Hybrid",
      method = "BLB->One-step",
      config_id = "A",
      rep = r,
      runtime_sec = blb_res$runtime_sec + one_res$runtime_sec,
      sample_accesses = blb_res$sample_accesses + one_res$sample_accesses,
      memory_mb = max(blb_res$memory_mb, one_res$memory_mb),
      mean_estimate = one_res$mean_estimate,
      mean_abs_error = one_res$mean_abs_error,
      mean_ci_cover = one_res$mean_ci_cover,
      mean_ci_width = one_res$mean_ci_width,
      coef_estimate = one_res$coef_estimate,
      coef_abs_error = one_res$coef_abs_error,
      coef_ci_cover = one_res$coef_ci_cover,
      coef_ci_width = one_res$coef_ci_width,
      mse = one_res$mse,
      r2 = one_res$r2
    )
  }

  # 综合方案B：One-step 初始化 -> SGD 在线更新
  for (r in seq_len(cfg$reps)) {
    set.seed(seed_base + 900 + r)
    one_res <- run_distributed_onestep(df, feature_cols, coef_name, true_mean, true_coef, K = 10, pilot_n = cfg$pilot_n)
    sgd_res <- run_sgd(df, feature_cols, coef_name, true_mean, true_coef, batch_size = 128, epochs = cfg$hybrid_sgd_epochs, lr = 0.008, beta_init = one_res$beta)

    rows[[length(rows) + 1]] <- data.frame(
      track = track_name,
      category = "Hybrid",
      method = "One-step->SGD",
      config_id = "B",
      rep = r,
      runtime_sec = one_res$runtime_sec + sgd_res$runtime_sec,
      sample_accesses = one_res$sample_accesses + sgd_res$sample_accesses,
      memory_mb = max(one_res$memory_mb, sgd_res$memory_mb),
      mean_estimate = sgd_res$mean_estimate,
      mean_abs_error = sgd_res$mean_abs_error,
      mean_ci_cover = sgd_res$mean_ci_cover,
      mean_ci_width = sgd_res$mean_ci_width,
      coef_estimate = sgd_res$coef_estimate,
      coef_abs_error = sgd_res$coef_abs_error,
      coef_ci_cover = sgd_res$coef_ci_cover,
      coef_ci_width = sgd_res$coef_ci_width,
      mse = sgd_res$mse,
      r2 = sgd_res$r2
    )

    keep <- round(seq(1, length(sgd_res$loss_history), length.out = min(180, length(sgd_res$loss_history))))
    convergence_rows[[length(convergence_rows) + 1]] <- data.frame(
      track = track_name,
      method = "Hybrid_One-step->SGD",
      iteration = keep,
      loss = sgd_res$loss_history[keep]
    )
  }

  # 综合方案C：分层小批次 -> 聚合
  for (r in seq_len(cfg$reps)) {
    set.seed(seed_base + 1100 + r)
    strata <- cut(df[[feature_cols[1]]], breaks = quantile(df[[feature_cols[1]]], probs = seq(0, 1, length.out = 5)), include.lowest = TRUE)
    strata_df <- split(df, strata)
    local_beta <- list()
    t0 <- Sys.time()
    local_runtime <- 0
    for (sdf in strata_df) {
      sgd_local <- run_sgd(sdf, feature_cols, coef_name, true_mean, true_coef, batch_size = 64, epochs = max(8, floor(cfg$sgd_epochs / 2)), lr = 0.01)
      local_beta[[length(local_beta) + 1]] <- sgd_local$beta
      local_runtime <- local_runtime + sgd_local$runtime_sec
    }
    beta_mat <- do.call(rbind, local_beta)
    beta_avg <- colMeans(beta_mat)
    runtime <- as.numeric(Sys.time() - t0, units = "secs") + local_runtime

    Xd <- build_design(df, feature_cols)
    pred <- as.vector(Xd %*% beta_avg[c("(Intercept)", feature_cols)])
    coef_est <- beta_avg[coef_name]

    rows[[length(rows) + 1]] <- data.frame(
      track = track_name,
      category = "Hybrid",
      method = "Stratified-SGD->Aggregate",
      config_id = "C",
      rep = r,
      runtime_sec = runtime,
      sample_accesses = cfg$sgd_epochs * nrow(df),
      memory_mb = as.numeric(object.size(beta_mat) / 1024^2),
      mean_estimate = mean(df$y),
      mean_abs_error = abs(mean(df$y) - true_mean),
      mean_ci_cover = as.numeric(true_mean >= (mean(df$y) - 1.96 * sd(df$y) / sqrt(nrow(df))) &&
                                   true_mean <= (mean(df$y) + 1.96 * sd(df$y) / sqrt(nrow(df)))),
      mean_ci_width = 2 * 1.96 * sd(df$y) / sqrt(nrow(df)),
      coef_estimate = coef_est,
      coef_abs_error = abs(coef_est - true_coef),
      coef_ci_cover = as.numeric(abs(coef_est - true_coef) < sd(beta_mat[, coef_name, drop = TRUE]) * 1.96),
      coef_ci_width = 2 * 1.96 * sd(beta_mat[, coef_name, drop = TRUE]),
      mse = mean((df$y - pred)^2),
      r2 = 1 - sum((df$y - pred)^2) / sum((df$y - mean(df$y))^2)
    )
  }

  list(
    results = dplyr::bind_rows(rows),
    convergence = dplyr::bind_rows(convergence_rows)
  )
}

# ----------------------------------------------------------------------------
# 6) 图表输出
# ----------------------------------------------------------------------------
plot_pareto <- function(summary_df, out_file) {
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = mean_runtime_sec, y = mean_abs_error_coef, color = category, shape = method)) +
    ggplot2::geom_point(size = 3.2, alpha = 0.9) +
    ggplot2::facet_wrap(~track, scales = "free_x") +
    ggplot2::scale_color_viridis_d(option = "C") +
    ggplot2::labs(title = "速度-精度帕累托对比", x = "平均计算时间（秒）", y = "系数绝对误差（越低越好）", color = "类别", shape = "方法")
  ggplot2::ggsave(out_file, p, width = 11, height = 6, dpi = 300)
}

plot_coverage_width <- function(summary_df, out_file) {
  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = reorder(paste(method, config_id, sep = "_"), mean_ci_cover_coef), y = mean_ci_cover_coef, fill = category)) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(y = pmin(1, mean_ci_width_coef / max(mean_ci_width_coef, na.rm = TRUE))), color = "black", size = 1.8) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~track) +
    ggplot2::scale_fill_viridis_d(option = "D") +
    ggplot2::labs(title = "覆盖率与区间宽度对比", x = "方法配置", y = "覆盖率（黑点为归一化区间宽度）", fill = "类别")
  ggplot2::ggsave(out_file, p, width = 12, height = 7, dpi = 300)
}

plot_convergence <- function(convergence_df, out_file) {
  if (nrow(convergence_df) == 0) return(invisible(NULL))
  p <- ggplot2::ggplot(convergence_df, ggplot2::aes(x = iteration, y = loss, color = method)) +
    ggplot2::geom_line(alpha = 0.85, linewidth = 0.8) +
    ggplot2::facet_wrap(~track, scales = "free") +
    ggplot2::scale_y_log10() +
    ggplot2::scale_color_viridis_d(option = "B") +
    ggplot2::labs(title = "小批次/综合方案收敛曲线", x = "迭代次数", y = "损失（MSE，对数）", color = "方法")
  ggplot2::ggsave(out_file, p, width = 11, height = 6, dpi = 300)
}

plot_radar <- function(summary_df, out_file) {
  top_methods <- summary_df |>
    dplyr::group_by(track, category) |>
    dplyr::slice_min(order_by = mean_abs_error_coef, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      精度 = scales::rescale(-mean_abs_error_coef, to = c(0, 1)),
      效率 = scales::rescale(-mean_runtime_sec, to = c(0, 1)),
      稳定性 = scales::rescale(-stability_coef_sd, to = c(0, 1)),
      覆盖率 = clip01(mean_ci_cover_coef)
    ) |>
    dplyr::select(track, category, method, config_id, 精度, 效率, 稳定性, 覆盖率)

  radar_long <- top_methods |>
    tidyr::pivot_longer(cols = c("精度", "效率", "稳定性", "覆盖率"), names_to = "metric", values_to = "score") |>
    dplyr::mutate(label = paste(track, category, method, config_id, sep = " | "))

  p <- ggplot2::ggplot(radar_long, ggplot2::aes(x = metric, y = score, group = label, color = label)) +
    ggplot2::geom_polygon(fill = NA, linewidth = 0.9, alpha = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::coord_polar() +
    ggplot2::scale_color_viridis_d(option = "A") +
    ggplot2::labs(title = "方法雷达图（各类别最优配置）", x = NULL, y = NULL, color = "方法配置") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
  ggplot2::ggsave(out_file, p, width = 10, height = 8, dpi = 300)
}

# ----------------------------------------------------------------------------
# 7) 主入口：快速/完整模式
# ----------------------------------------------------------------------------
run_integrated_comparison <- function(
    mode = c("quick", "full"),
    output_dir = file.path(getwd(), "output", "unified_comparison"),
    empirical_path = file.path(getwd(), "data", "yellow_tripdata_2023-01.parquet"),
    seed = 20260417
) {
  mode <- match.arg(mode)
  set.seed(seed)

  cfg <- switch(
    mode,
    quick = list(reps = 3, sim_n = 6000, sim_sigma = 1.2, sim_rho = 0.5, sim_nonlinear = 0.2, empirical_n_max = 120000, pilot_n = 1200, blb_gamma = 0.7, blb_s = 6, blb_B = 40, sgd_epochs = 20, hybrid_sgd_epochs = 14),
    full = list(reps = 8, sim_n = 40000, sim_sigma = 1.0, sim_rho = 0.6, sim_nonlinear = 0.3, empirical_n_max = 600000, pilot_n = 2500, blb_gamma = 0.7, blb_s = 10, blb_B = 120, sgd_epochs = 40, hybrid_sgd_epochs = 28)
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_tag <- paste0("integrated_", mode, "_", timestamp)

  # 模拟主线
  sim_df <- generate_simulation_data(
    n = cfg$sim_n,
    rho = cfg$sim_rho,
    sigma = cfg$sim_sigma,
    nonlinear_strength = cfg$sim_nonlinear,
    seed = seed + 1
  )
  sim_features <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
  sim_true_mean <- mean(sim_df$y)
  sim_true_coef <- 0.8
  sim_track <- run_track_experiment("Simulation", sim_df, sim_features, "x1", sim_true_mean, sim_true_coef, cfg, seed_base = seed + 100)

  # 实证主线（全量估计作为基准真值代理）
  emp_df <- load_empirical_data(empirical_path, n_max = cfg$empirical_n_max, seed = seed + 2)
  emp_features <- c("x1", "x2", "x3")
  emp_true_mean <- mean(emp_df$y)
  full_emp_fit <- lm(y ~ x1 + x2 + x3, data = emp_df)
  emp_true_coef <- unname(coef(full_emp_fit)[["x1"]])
  emp_track <- run_track_experiment("Empirical", emp_df, emp_features, "x1", emp_true_mean, emp_true_coef, cfg, seed_base = seed + 200)

  # 合并与汇总
  all_results <- dplyr::bind_rows(sim_track$results, emp_track$results)
  all_convergence <- dplyr::bind_rows(sim_track$convergence, emp_track$convergence)
  summary_table <- summarize_method_results(all_results)
  budget_table <- compute_budget_views(summary_table)

  # 参数敏感性：聚焦可调参数方法
  sensitivity_table <- summary_table |>
    dplyr::filter(grepl("K|frac|g|bs", config_id)) |>
    dplyr::select(track, category, method, config_id, mean_abs_error_coef, mean_ci_cover_coef, mean_runtime_sec, stability_coef_sd)

  # 综合收益：Hybrid 对比同 track 最优非Hybrid
  reference <- get_reference_best(summary_table)
  hybrid_gain <- summary_table |>
    dplyr::filter(category == "Hybrid") |>
    dplyr::left_join(reference, by = "track") |>
    dplyr::mutate(
      error_gain_pct = (ref_error - mean_abs_error_coef) / pmax(ref_error, 1e-8) * 100,
      runtime_gain_pct = (ref_runtime - mean_runtime_sec) / pmax(ref_runtime, 1e-8) * 100,
      cover_gain = mean_ci_cover_coef - ref_ci_cover
    ) |>
    dplyr::select(track, method, config_id, error_gain_pct, runtime_gain_pct, cover_gain, ref_method, ref_config)

  # 输出表格
  write.csv(all_results, file.path(output_dir, paste0(run_tag, "_raw_results.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(summary_table, file.path(output_dir, paste0(run_tag, "_table_overall_accuracy_efficiency_stability.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(sensitivity_table, file.path(output_dir, paste0(run_tag, "_table_parameter_sensitivity.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(hybrid_gain, file.path(output_dir, paste0(run_tag, "_table_hybrid_gain.csv")), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(budget_table, file.path(output_dir, paste0(run_tag, "_table_budget_views.csv")), row.names = FALSE, fileEncoding = "UTF-8")

  # 输出图形
  plot_pareto(summary_table, file.path(output_dir, paste0(run_tag, "_fig_speed_accuracy_pareto.png")))
  plot_coverage_width(summary_table, file.path(output_dir, paste0(run_tag, "_fig_coverage_width.png")))
  plot_convergence(all_convergence, file.path(output_dir, paste0(run_tag, "_fig_convergence.png")))
  plot_radar(summary_table, file.path(output_dir, paste0(run_tag, "_fig_radar.png")))

  # 日志
  log_lines <- c(
    "统一横向对比实验日志",
    paste("run_tag:", run_tag),
    paste("mode:", mode),
    paste("timestamp:", as.character(Sys.time())),
    paste("seed:", seed),
    paste("simulation_n:", cfg$sim_n),
    paste("empirical_n_max:", cfg$empirical_n_max),
    paste("reps:", cfg$reps),
    paste("methods_count:", length(unique(paste(all_results$category, all_results$method, all_results$config_id)))),
    paste("output_dir:", output_dir)
  )
  writeLines(log_lines, con = file.path(output_dir, paste0(run_tag, "_run.log")))

  message("实验完成，输出目录：", output_dir)
  invisible(list(
    run_tag = run_tag,
    raw = all_results,
    summary = summary_table,
    sensitivity = sensitivity_table,
    hybrid_gain = hybrid_gain,
    budget = budget_table
  ))
}

if (interactive()) {
  message("可用入口：")
  message("  run_integrated_comparison(mode = 'quick')")
  message("  run_integrated_comparison(mode = 'full')")
}
