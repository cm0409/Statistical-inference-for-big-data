# ============================================================================
# 数值模拟：海量数据统计计算方法（精简 + 加速版）
# ============================================================================

rm(list = ls())
set.seed(42)

# ----------------------------------------------------------------------------
# 1) 依赖与通用设置
# ----------------------------------------------------------------------------
required_pkgs <- c(
  "MASS", "glmnet", "mgcv", "np", "ggplot2", "dplyr", "scales",
  "viridis", "officer", "rvg"
)
is_pkg_available <- function(pkg) {
  tryCatch(nzchar(find.package(pkg, quiet = TRUE)), error = function(e) FALSE)
}
missing_pkgs <- required_pkgs[!vapply(required_pkgs, is_pkg_available, logical(1))]
if (length(missing_pkgs) > 0) {
  stop("缺少依赖包，请先安装：", paste(missing_pkgs, collapse = ", "))
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

font_family <- if (.Platform$OS.type == "windows") {
  windowsFonts(YaHei = windowsFont("微软雅黑"))
  "YaHei"
} else {
  "sans"
}

theme_cn <- function(base_size = 12) {
  theme_bw(base_size = base_size, base_family = font_family) +
    theme(
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "#333333", linewidth = 0.4),
      text = element_text(color = "#333333"),
      plot.title = element_text(face = "bold", color = "#1A1A1A", size = base_size + 2),
      plot.subtitle = element_text(color = "#555555", size = base_size),
      strip.text = element_text(face = "bold", color = "#1A1A1A"),
      strip.background = element_rect(fill = "#F5F5F5", color = NA)
    )
}

palette_main <- c(
  OLS = "#4472C4", Polynomial = "#ED7D31", Ridge = "#70AD47",
  GAM = "#FFC000", Kernel = "#5B9BD5", Partial_Linear = "#A5A5A5"
)
palette_blb <- c(
  "γ=0.6, s=5" = "#305496", "γ=0.6, s=10" = "#4472C4", "γ=0.6, s=20" = "#8EAADB",
  "γ=0.7, s=5" = "#C55A11", "γ=0.7, s=10" = "#ED7D31", "γ=0.7, s=20" = "#F4B183",
  "全数据Bootstrap" = "#A5A5A5"
)
palette_batch <- c("32" = "#4472C4", "64" = "#70AD47", "128" = "#ED7D31", "256" = "#FFC000", "512" = "#5B9BD5")
palette_dist <- c("SAE" = "#4472C4", "One-step" = "#ED7D31", "Ideal" = "#A5A5A5")

save_to_ppt <- function(plot_list, outfile) {
  doc <- read_pptx()
  for (nm in names(plot_list)) {
    doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
    doc <- ph_with(
      doc,
      value = rvg::dml(ggobj = plot_list[[nm]], fonts = list(sans = "Microsoft YaHei")),
      location = ph_location_fullsize()
    )
  }
  print(doc, target = outfile)
  message("可编辑 PPT 已保存: ", outfile)
}

write_csv_bom <- function(df, path) {
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8-BOM")
  message("表格已保存: ", path)
}

detected_cores <- parallel::detectCores(logical = FALSE)
n_cores <- if (is.na(detected_cores)) 1L else max(1L, detected_cores - 1L)
map_reps <- function(X, FUN, ...) {
  if (.Platform$OS.type == "unix" && n_cores > 1L) {
    parallel::mclapply(X, FUN, ..., mc.cores = n_cores)
  } else {
    lapply(X, FUN, ...)
  }
}

replace_na_coef <- function(b) {
  if (anyNA(b)) warning("检测到不可估系数，已用 0 填充。")
  b[is.na(b)] <- 0
  b
}

fast_lm_coef <- function(X, y) {
  fit <- lm.fit(x = X, y = y)
  replace_na_coef(fit$coefficients)
}

fast_wls_coef <- function(X, y, w) {
  fit <- lm.wfit(x = X, y = y, w = w)
  replace_na_coef(fit$coefficients)
}

# ----------------------------------------------------------------------------
# 2) 数据生成参数
# ----------------------------------------------------------------------------
beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)
p <- 8
rho <- 0.5
sigma <- 1
Sigma <- toeplitz(rho^(0:(p - 1)))
sample_sizes <- c(1000, 10000, 100000)
n_large <- max(sample_sizes)
n_medium <- median(sample_sizes)
feature_names <- c("income", "age", "rooms", "distance", "crime_rate", "school_rating", "tax_rate", "employment")

# ----------------------------------------------------------------------------
# 实验1：不同样本量模型比较
# ----------------------------------------------------------------------------
message("\n=== 实验1：不同样本量模型比较 ===")

run_single_n <- function(n) {
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- feature_names
  y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))
  df <- data.frame(y = y, X)

  t0 <- Sys.time(); fit_ols <- lm(y ~ ., data = df); t_ols <- as.numeric(Sys.time() - t0, units = "secs")
  pred_ols <- fitted(fit_ols)

  t0 <- Sys.time()
  fit_poly <- lm(y ~ poly(income, 2) + poly(age, 2) + rooms + distance + crime_rate + school_rating + tax_rate + employment, data = df)
  t_poly <- as.numeric(Sys.time() - t0, units = "secs")
  pred_poly <- fitted(fit_poly)

  t0 <- Sys.time()
  cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 5, standardize = TRUE)
  fit_ridge <- glmnet(X, y, alpha = 0, lambda = cv_ridge$lambda.min, standardize = TRUE)
  t_ridge <- as.numeric(Sys.time() - t0, units = "secs")
  pred_ridge <- as.vector(predict(fit_ridge, newx = X))

  t0 <- Sys.time()
  fit_gam <- mgcv::gam(y ~ s(income) + s(age) + s(rooms) + distance + crime_rate + school_rating + tax_rate + employment, data = df, method = "REML")
  t_gam <- as.numeric(Sys.time() - t0, units = "secs")
  pred_gam <- predict(fit_gam)

  t0 <- Sys.time()
  bw <- np::npregbw(xdat = X[, "income"], ydat = y, regtype = "ll")
  fit_kernel <- np::npreg(bw)
  t_kernel <- as.numeric(Sys.time() - t0, units = "secs")
  pred_kernel <- fitted(fit_kernel)

  t0 <- Sys.time()
  fit_pl <- mgcv::gam(y ~ rooms + distance + crime_rate + school_rating + tax_rate + employment + s(income), data = df, method = "REML")
  t_pl <- as.numeric(Sys.time() - t0, units = "secs")
  pred_pl <- predict(fit_pl)

  ss_tot <- sum((y - mean(y))^2)
  list(
    OLS = list(time = t_ols, mse = mean((y - pred_ols)^2), r2 = summary(fit_ols)$r.squared),
    Polynomial = list(time = t_poly, mse = mean((y - pred_poly)^2), r2 = summary(fit_poly)$r.squared),
    Ridge = list(time = t_ridge, mse = mean((y - pred_ridge)^2), r2 = 1 - sum((y - pred_ridge)^2) / ss_tot, lambda = cv_ridge$lambda.min),
    GAM = list(time = t_gam, mse = mean((y - pred_gam)^2), r2 = summary(fit_gam)$r.sq),
    Kernel = list(time = t_kernel, mse = mean((y - pred_kernel)^2), r2 = 1 - sum((y - pred_kernel)^2) / ss_tot, bandwidth = fit_kernel$bw),
    Partial_Linear = list(time = t_pl, mse = mean((y - pred_pl)^2), r2 = summary(fit_pl)$r.sq)
  )
}

all_results <- setNames(map_reps(sample_sizes, run_single_n), as.character(sample_sizes))

# ----------------------------------------------------------------------------
# 实验2：分布式计算模拟
# ----------------------------------------------------------------------------
message("\n=== 实验2：分布式计算模拟 ===")

X <- MASS::mvrnorm(n_large, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- feature_names
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n_large, 0, sigma))
X_design <- cbind(1, X)

# 基准 OLS（快速线性代数实现）
t0 <- Sys.time()
base_coef <- fast_lm_coef(X_design, y)
base_time <- as.numeric(Sys.time() - t0, units = "secs")

K_values <- c(5, 10, 20, 50)
dist_rows <- lapply(K_values, function(K) {
  idx <- sample(rep(seq_len(K), length.out = n_large))
  split_idx <- split(seq_len(n_large), idx)

  t0 <- Sys.time()
  local_est <- vapply(split_idx, function(ik) fast_lm_coef(X_design[ik, , drop = FALSE], y[ik]), numeric(p + 1))
  sae_est <- rowMeans(local_est)
  sae_time <- as.numeric(Sys.time() - t0, units = "secs")

  t0 <- Sys.time()
  pilot_idx <- sample.int(n_large, min(1000, n_large))
  beta0 <- fast_lm_coef(X_design[pilot_idx, , drop = FALSE], y[pilot_idx])
  grad_mat <- vapply(split_idx, function(ik) {
    Xk <- X_design[ik, , drop = FALSE]
    resid <- y[ik] - Xk %*% beta0
    as.vector(-2 * crossprod(Xk, resid) / length(ik))
  }, numeric(p + 1))
  avg_grad <- rowMeans(grad_mat)
  fisher <- 2 * crossprod(X_design) / n_large
  onestep_est <- beta0 - solve(fisher, avg_grad)
  onestep_time <- as.numeric(Sys.time() - t0, units = "secs")

  data.frame(
    K = K,
    Method = c("SAE", "One-step"),
    Time = c(sae_time, onestep_time),
    Speedup = c(base_time / sae_time, base_time / onestep_time),
    EstBiasL2 = c(sqrt(sum((sae_est - base_coef)^2)), sqrt(sum((onestep_est - base_coef)^2)))
  )
})
dist_df <- bind_rows(dist_rows, data.frame(K = K_values, Method = "Ideal", Time = base_time / K_values, Speedup = K_values, EstBiasL2 = NA_real_))

# ----------------------------------------------------------------------------
# 实验3：BLB 子抽样
# ----------------------------------------------------------------------------
message("\n=== 实验3：BLB 子抽样 ===")

n <- n_medium
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- feature_names
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))
X_design <- cbind(1, X)

gamma_values <- c(0.6, 0.7)
s_values <- c(5, 10, 20)

# 基准：全数据 bootstrap
B_full <- 500
full_boot <- replicate(B_full, {
  w <- as.numeric(rmultinom(1, size = n, prob = rep.int(1 / n, n)))
  fast_wls_coef(X_design, y, w)
})
full_boot <- t(full_boot)
full_boot_se <- apply(full_boot, 2, sd)
full_boot_mean <- colMeans(full_boot)
full_cov <- mean(beta_true >= (full_boot_mean - 1.96 * full_boot_se) & beta_true <= (full_boot_mean + 1.96 * full_boot_se))

blb_rows <- list()
row_id <- 1L
for (gamma in gamma_values) {
  for (s in s_values) {
    m <- floor(n^gamma)
    B <- 100
    subset_stat <- map_reps(seq_len(s), function(i) {
      sub_idx <- sample.int(n, m, replace = FALSE)
      X_sub <- X_design[sub_idx, , drop = FALSE]
      y_sub <- y[sub_idx]
      boot_coef <- replicate(B, {
        w <- as.numeric(rmultinom(1, size = m, prob = rep.int(1 / m, m)))
        fast_wls_coef(X_sub, y_sub, w)
      })
      boot_coef <- t(boot_coef)
      list(mu = colMeans(boot_coef), vr = apply(boot_coef, 2, var))
    })

    subset_means <- do.call(rbind, lapply(subset_stat, `[[`, "mu"))
    subset_vars <- do.call(rbind, lapply(subset_stat, `[[`, "vr"))
    blb_est <- colMeans(subset_means)
    blb_se <- sqrt(colMeans(subset_vars))
    ci_lower <- blb_est - 1.96 * blb_se
    ci_upper <- blb_est + 1.96 * blb_se

    blb_rows[[row_id]] <- data.frame(
      Gamma = gamma,
      S = s,
      Coverage = mean(beta_true >= ci_lower & beta_true <= ci_upper),
      CI_Width = mean(ci_upper - ci_lower),
      Method = sprintf("γ=%.1f, s=%d", gamma, s)
    )
    row_id <- row_id + 1L
  }
}
blb_df <- bind_rows(blb_rows, data.frame(Gamma = NA, S = NA, Coverage = full_cov, CI_Width = mean(2 * 1.96 * full_boot_se), Method = "全数据Bootstrap"))

# ----------------------------------------------------------------------------
# 实验4：SGD 小批次优化
# ----------------------------------------------------------------------------
message("\n=== 实验4：SGD 小批次优化 ===")

batch_sizes <- c(32, 64, 128, 256, 512)
sgd_loss_df <- list()
sgd_summary_df <- list()

for (i in seq_along(batch_sizes)) {
  bs <- batch_sizes[i]
  beta <- rnorm(p + 1, 0, 0.1)
  lr <- 0.01
  epochs <- 50
  loss_hist <- numeric(epochs * ceiling(n / bs))
  iter <- 1L

  for (epoch in seq_len(epochs)) {
    shuffle <- sample.int(n)
    Xs <- X_design[shuffle, , drop = FALSE]
    ys <- y[shuffle]
    for (b in seq_len(ceiling(n / bs))) {
      start_idx <- (b - 1) * bs + 1
      end_idx <- min(b * bs, n)
      Xb <- Xs[start_idx:end_idx, , drop = FALSE]
      yb <- ys[start_idx:end_idx]
      resid <- yb - Xb %*% beta
      beta <- beta - lr * as.vector(-2 * crossprod(Xb, resid) / length(yb))
      loss_hist[iter] <- mean((y - X_design %*% beta)^2)
      iter <- iter + 1L
    }
    lr <- lr * 0.99
  }

  loss_hist <- loss_hist[seq_len(iter - 1L)]
  pred <- as.vector(X_design %*% beta)
  mse <- mean((y - pred)^2)
  r2 <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)

  sample_idx <- round(seq(1, length(loss_hist), length.out = min(200, length(loss_hist))))
  sgd_loss_df[[i]] <- data.frame(Iteration = sample_idx, Loss = loss_hist[sample_idx], BatchSize = factor(bs, levels = batch_sizes))
  sgd_summary_df[[i]] <- data.frame(BatchSize = bs, Epochs = epochs, 初始学习率 = 0.01, 最终MSE = round(mse, 6), R2 = round(r2, 4), 收敛迭代次数 = length(loss_hist))
}
sgd_loss_df <- bind_rows(sgd_loss_df)
sgd_summary_df <- bind_rows(sgd_summary_df)

# ----------------------------------------------------------------------------
# 可视化（高质量 + PPT）
# ----------------------------------------------------------------------------
message("\n=== 生成可视化图表 ===")
plot_list <- list()

plot_compute <- bind_rows(lapply(names(all_results), function(size) {
  bind_rows(lapply(names(all_results[[size]]), function(model) {
    r <- all_results[[size]][[model]]
    data.frame(SampleSize = factor(size, levels = as.character(sample_sizes)), Model = model, Time = r$time, MSE = r$mse, R2 = r$r2)
  }))
}))

model_labels <- c(OLS = "OLS", Polynomial = "多项式回归", Ridge = "岭回归", GAM = "GAM", Kernel = "核回归", Partial_Linear = "部分线性模型")

plot_list[["图1_计算时间对比"]] <- ggplot(plot_compute, aes(x = SampleSize, y = Time, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white", linewidth = 0.3) +
  scale_y_log10(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "不同样本量下模型计算时间", x = "样本量", y = "计算时间（秒，对数坐标）", fill = "模型") +
  theme_cn() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = palette_main, labels = model_labels)

res_large <- all_results[[as.character(max(sample_sizes))]]
plot_mse <- bind_rows(lapply(names(res_large), function(model) data.frame(Model = model, MSE = res_large[[model]]$mse)))
plot_mse <- plot_mse |> arrange(MSE)
sorted_models <- plot_mse$Model
plot_mse <- plot_mse |> mutate(Model = factor(Model, levels = sorted_models))

plot_list[["图2_MSE对比"]] <- ggplot(plot_mse, aes(x = Model, y = MSE, fill = Model)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.4f", MSE)), vjust = -0.5, size = 3.5, family = font_family) +
  labs(title = "模型精度对比", x = "模型", y = "均方误差（MSE）") +
  theme_cn() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_manual(values = palette_main, labels = model_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

plot_list[["图3_加速比曲线"]] <- ggplot(dist_df, aes(x = K, y = Speedup, color = Method, linetype = Method)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(fill = Method), size = 3.5, shape = 21, color = "white", stroke = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(title = "分布式计算加速表现", x = "节点数量 K", y = "加速比", color = "方法", linetype = "方法") +
  theme_cn() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = palette_dist) +
  scale_fill_manual(values = palette_dist, guide = "none") +
  scale_x_continuous(breaks = K_values)

plot_list[["图4_BLB覆盖率"]] <- ggplot(blb_df, aes(x = Method, y = Coverage, fill = Method)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#C44E52", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f%%", Coverage * 100)), hjust = -0.1, size = 3.2, family = font_family) +
  labs(title = "BLB 与全数据 Bootstrap 覆盖率", x = "方法", y = "覆盖率") +
  theme_cn() +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette_blb) +
  scale_x_discrete(limits = rev(levels(factor(blb_df$Method)))) +
  scale_y_continuous(labels = percent, limits = c(0, 1.05), expand = c(0, 0)) +
  coord_flip()

plot_list[["图5_SGD收敛曲线"]] <- ggplot(sgd_loss_df, aes(x = Iteration, y = Loss, color = BatchSize)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  labs(title = "SGD 在不同 batch size 下的收敛", x = "迭代次数", y = "损失值（MSE，对数坐标）", color = "批大小") +
  theme_cn() +
  scale_color_manual(values = palette_batch) +
  scale_y_log10()

complexity <- c(OLS = 1, Polynomial = 2, Ridge = 2, GAM = 4, Kernel = 5, Partial_Linear = 3)
plot_complex <- bind_rows(lapply(names(res_large), function(model) {
  data.frame(Model = model, Complexity = complexity[model], MSE = res_large[[model]]$mse, R2 = res_large[[model]]$r2, Time = res_large[[model]]$time)
}))

plot_list[["图6_复杂度精度权衡"]] <- ggplot(plot_complex, aes(x = Complexity, y = MSE, size = Time, color = Model)) +
  geom_point(alpha = 0.9, stroke = 0.8) +
  geom_text(aes(label = model_labels[Model]), vjust = -1.2, size = 3.5, family = font_family) +
  labs(title = "模型复杂度-精度-耗时权衡", x = "模型复杂度评分", y = "均方误差（MSE）", size = "计算时间（秒）", color = "模型") +
  theme_cn() +
  scale_color_manual(values = palette_main, labels = model_labels) +
  scale_size_continuous(range = c(3, 15))

save_to_ppt(plot_list, file.path(output_dir, "simulation_figures.pptx"))

# ----------------------------------------------------------------------------
# 表格输出
# ----------------------------------------------------------------------------
message("\n=== 生成表格输出 ===")

for (n_now in sample_sizes) {
  res <- all_results[[as.character(n_now)]]
  tbl <- data.frame(
    模型 = names(res),
    计算时间_秒 = round(sapply(res, `[[`, "time"), 4),
    MSE = round(sapply(res, `[[`, "mse"), 6),
    R2 = round(sapply(res, `[[`, "r2"), 4)
  ) |>
    arrange(MSE)
  write_csv_bom(tbl, file.path(output_dir, sprintf("sim_table1_model_n%d.csv", n_now)))
}

dist_table <- bind_rows(lapply(K_values, function(K) {
  data.frame(
    K = K,
    SAE_时间_秒 = round(dist_df$Time[dist_df$K == K & dist_df$Method == "SAE"], 4),
    SAE_加速比 = round(dist_df$Speedup[dist_df$K == K & dist_df$Method == "SAE"], 2),
    OneStep_时间_秒 = round(dist_df$Time[dist_df$K == K & dist_df$Method == "One-step"], 4),
    OneStep_加速比 = round(dist_df$Speedup[dist_df$K == K & dist_df$Method == "One-step"], 2),
    理想加速比 = K
  )
}))

write_csv_bom(dist_table, file.path(output_dir, "sim_table2_distributed.csv"))
write_csv_bom(blb_df, file.path(output_dir, "sim_table3_blb.csv"))
write_csv_bom(sgd_summary_df, file.path(output_dir, "sim_table4_sgd.csv"))

recommendation <- data.frame(
  场景 = c("快速原型", "高精度需求", "大数据集", "非线性关系", "可解释性优先"),
  推荐模型 = c("OLS", "GAM", "分布式OLS", "GAM/核回归", "OLS/多项式"),
  理由 = c("计算最快，易于实现", "能捕捉复杂非线性关系", "分布式计算可加速", "非参数方法更灵活", "线性模型易于解释"),
  注意事项 = c("假设线性关系", "计算成本较高", "需要多核环境", "需要更多数据", "可能欠拟合")
)
write_csv_bom(recommendation, file.path(output_dir, "sim_table5_recommendations.csv"))

message("\n=== 数值模拟全部完成，结果保存在 output/ 目录 ===")
