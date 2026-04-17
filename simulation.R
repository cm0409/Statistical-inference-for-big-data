rm(list = ls())
set.seed(42)

invisible(lapply(
  c("MASS", "glmnet", "mgcv", "np", "ggplot2", "dplyr", "tidyr", "scales", "officer", "rvg"),
  function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
))

output_dir <- file.path(getwd(), "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
      panel.grid.minor = element_blank(), panel.border = element_blank(),
      axis.line = element_line(color = "#333333", linewidth = 0.4),
      text = element_text(color = "#333333"),
      plot.title = element_text(face = "bold", color = "#1A1A1A", size = base_size + 2),
      plot.subtitle = element_text(color = "#555555", size = base_size),
      axis.text = element_text(color = "#444444"), legend.text = element_text(color = "#444444"),
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
palette_batch <- c(
  "32" = "#4472C4", "64" = "#70AD47", "128" = "#ED7D31",
  "256" = "#FFC000", "512" = "#5B9BD5"
)
palette_dist <- c("SAE" = "#4472C4", "One-step" = "#ED7D31", "Ideal" = "#A5A5A5")
model_labels <- c(OLS = "OLS", Polynomial = "多项式回归", Ridge = "岭回归", GAM = "GAM", Kernel = "核回归", Partial_Linear = "部分线性模型")

save_to_ppt <- function(plot_list, outfile) {
  doc <- read_pptx()
  for (nm in names(plot_list)) {
    doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
    doc <- ph_with(doc, value = rvg::dml(ggobj = plot_list[[nm]], fonts = list(sans = "Microsoft YaHei")), location = ph_location_fullsize())
  }
  print(doc, target = outfile)
}

write_csv_bom <- function(df, path) write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8-BOM")

beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)
p <- 8
rho <- 0.5
sigma <- 1
# AR(1) 协方差：Sigma[i,j] = rho^|i-j|
Sigma <- rho^abs(outer(seq_len(p), seq_len(p), "-"))
sample_sizes <- c(1000, 10000, 100000)
n_large <- max(sample_sizes)
n_medium <- median(sample_sizes)

fit_lm_fast <- function(Xm, yv) lm.fit(Xm, yv)$coefficients
lm_metrics <- function(y_true, y_pred, elapsed) {
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  list(time = elapsed, mse = mean((y_true - y_pred)^2), r2 = 1 - ss_res / ss_tot)
}

all_results <- setNames(vector("list", length(sample_sizes)), as.character(sample_sizes))

for (n in sample_sizes) {
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- c("income", "age", "rooms", "distance", "crime_rate", "school_rating", "tax_rate", "employment")
  y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))
  df <- data.frame(y = y, X)

  t0 <- Sys.time()
  fit_ols <- lm(y ~ ., data = df)
  res_ols <- lm_metrics(y, predict(fit_ols), as.numeric(Sys.time() - t0, units = "secs"))

  t0 <- Sys.time()
  fit_poly <- lm(y ~ poly(income, 2) + poly(age, 2) + rooms + distance + crime_rate + school_rating + tax_rate + employment, data = df)
  res_poly <- lm_metrics(y, predict(fit_poly), as.numeric(Sys.time() - t0, units = "secs"))

  t0 <- Sys.time()
  cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 5, standardize = TRUE)
  fit_ridge <- glmnet(X, y, alpha = 0, lambda = cv_ridge$lambda.min, standardize = TRUE)
  t_ridge <- as.numeric(Sys.time() - t0, units = "secs")
  pred_ridge <- as.vector(predict(fit_ridge, newx = X))
  metric_ridge <- lm_metrics(y, pred_ridge, t_ridge)
  res_ridge <- list(time = metric_ridge$time, mse = metric_ridge$mse, r2 = metric_ridge$r2, lambda = cv_ridge$lambda.min)

  t0 <- Sys.time()
  fit_gam <- mgcv::gam(y ~ s(income) + s(age) + s(rooms) + distance + crime_rate + school_rating + tax_rate + employment, data = df, method = "REML")
  t_gam <- as.numeric(Sys.time() - t0, units = "secs")
  metric_gam <- lm_metrics(y, predict(fit_gam), t_gam)
  res_gam <- list(time = metric_gam$time, mse = metric_gam$mse, r2 = summary(fit_gam)$r.sq)

  t0 <- Sys.time()
  bw_obj <- np::npregbw(xdat = X[, "income"], ydat = y, regtype = "ll")
  fit_kernel <- np::npreg(bw_obj)
  t_kernel <- as.numeric(Sys.time() - t0, units = "secs")
  pred_kernel <- fitted(fit_kernel)
  metric_kernel <- lm_metrics(y, pred_kernel, t_kernel)
  res_kernel <- list(time = metric_kernel$time, mse = metric_kernel$mse, r2 = metric_kernel$r2, bandwidth = fit_kernel$bw)

  t0 <- Sys.time()
  fit_pl <- mgcv::gam(y ~ rooms + distance + crime_rate + school_rating + tax_rate + employment + s(income), data = df, method = "REML")
  t_pl <- as.numeric(Sys.time() - t0, units = "secs")
  metric_pl <- lm_metrics(y, predict(fit_pl), t_pl)
  res_pl <- list(time = metric_pl$time, mse = metric_pl$mse, r2 = summary(fit_pl)$r.sq)

  all_results[[as.character(n)]] <- list(OLS = res_ols, Polynomial = res_poly, Ridge = res_ridge, GAM = res_gam, Kernel = res_kernel, Partial_Linear = res_pl)
}

# 分布式模拟
n <- n_large
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- c("income", "age", "rooms", "distance", "crime_rate", "school_rating", "tax_rate", "employment")
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))
Xd <- cbind(1, X)

t0 <- Sys.time()
fit_full <- fit_lm_fast(Xd, y)
base_time <- as.numeric(Sys.time() - t0, units = "secs")

K_values <- c(5, 10, 20, 50)
dist_rows <- vector("list", length(K_values) * 2)
row_id <- 1

for (K in K_values) {
  t0 <- Sys.time()
  split_idx <- split(seq_len(n), sample(rep(seq_len(K), length.out = n)))
  local_est <- vapply(split_idx, function(ix) fit_lm_fast(Xd[ix, , drop = FALSE], y[ix]), numeric(p + 1))
  sae_time <- as.numeric(Sys.time() - t0, units = "secs")

  t0 <- Sys.time()
  pilot_idx <- sample.int(n, min(1000, n))
  beta0 <- fit_lm_fast(Xd[pilot_idx, , drop = FALSE], y[pilot_idx])
  split_idx2 <- split(seq_len(n), sample(rep(seq_len(K), length.out = n)))
  grads <- vapply(split_idx2, function(ix) {
    Xi <- Xd[ix, , drop = FALSE]
    as.vector(-2 * crossprod(Xi, y[ix] - Xi %*% beta0) / length(ix))
  }, numeric(p + 1))
  avg_grad <- rowMeans(grads)
  fisher <- 2 * crossprod(Xd) / n
  onestep_est <- as.vector(beta0 - solve(fisher, avg_grad))
  onestep_time <- as.numeric(Sys.time() - t0, units = "secs")

  dist_rows[[row_id]] <- data.frame(K = K, Method = "SAE", Time = sae_time, Speedup = base_time / sae_time)
  row_id <- row_id + 1
  dist_rows[[row_id]] <- data.frame(K = K, Method = "One-step", Time = onestep_time, Speedup = base_time / onestep_time)
  row_id <- row_id + 1
}

dist_df <- dplyr::bind_rows(dist_rows, data.frame(K = K_values, Method = "Ideal", Time = base_time / K_values, Speedup = K_values))

# BLB
n <- n_medium
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- c("income", "age", "rooms", "distance", "crime_rate", "school_rating", "tax_rate", "employment")
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))
Xd <- cbind(1, X)

coef_boot <- function(Xm, ym, idx) fit_lm_fast(Xm[idx, , drop = FALSE], ym[idx])
gamma_values <- c(0.6, 0.7)
s_values <- c(5, 10, 20)

B_full <- 500
full_boot <- replicate(B_full, coef_boot(Xd, y, sample.int(n, n, replace = TRUE)))
full_boot_se <- apply(full_boot, 1, sd)
full_boot_mean <- rowMeans(full_boot)
full_cov <- mean(beta_true >= (full_boot_mean - 1.96 * full_boot_se) & beta_true <= (full_boot_mean + 1.96 * full_boot_se))

blb_rows <- list()
for (gamma in gamma_values) {
  for (s in s_values) {
    m <- floor(n^gamma)
    B <- 100
    subset_mean <- matrix(0, s, p + 1)
    subset_var <- matrix(0, s, p + 1)

    for (i in seq_len(s)) {
      sub_idx <- sample.int(n, m)
      X_sub <- Xd[sub_idx, , drop = FALSE]
      y_sub <- y[sub_idx]
      boot_sub <- replicate(B, coef_boot(X_sub, y_sub, sample.int(m, m, replace = TRUE)))
      subset_mean[i, ] <- rowMeans(boot_sub)
      subset_var[i, ] <- apply(boot_sub, 1, var)
    }

    blb_est <- colMeans(subset_mean)
    blb_se <- sqrt(colMeans(subset_var))
    ci_lower <- blb_est - 1.96 * blb_se
    ci_upper <- blb_est + 1.96 * blb_se

    blb_rows[[length(blb_rows) + 1]] <- data.frame(
      Gamma = gamma, S = s,
      Coverage = mean(beta_true >= ci_lower & beta_true <= ci_upper),
      CI_Width = mean(ci_upper - ci_lower),
      Method = sprintf("γ=%.1f, s=%d", gamma, s)
    )
  }
}

blb_df <- dplyr::bind_rows(blb_rows, data.frame(Gamma = NA, S = NA, Coverage = full_cov, CI_Width = mean(2 * 1.96 * full_boot_se), Method = "全数据Bootstrap"))

# SGD
X_design <- Xd
batch_sizes <- c(32, 64, 128, 256, 512)
sgd_loss_rows <- list()
sgd_summary_rows <- list()

for (bs in batch_sizes) {
  beta <- rnorm(p + 1, 0, 0.1)
  lr <- 0.01
  epochs <- 50
  loss_hist <- numeric(epochs * ceiling(n / bs))
  iter <- 1

  for (epoch in seq_len(epochs)) {
    ord <- sample.int(n)
    Xs <- X_design[ord, , drop = FALSE]
    ys <- y[ord]
    num_batches <- ceiling(n / bs)

    for (b in seq_len(num_batches)) {
      ix <- ((b - 1) * bs + 1):min(b * bs, n)
      Xb <- Xs[ix, , drop = FALSE]
      yb <- ys[ix]
      beta <- beta - lr * as.vector(-2 * crossprod(Xb, yb - Xb %*% beta) / length(yb))
      loss_hist[iter] <- mean((y - X_design %*% beta)^2)
      iter <- iter + 1
    }
    lr <- lr * 0.99
  }

  loss_hist <- loss_hist[seq_len(iter - 1)]
  pred <- X_design %*% beta
  sample_ix <- round(seq(1, length(loss_hist), length.out = min(200, length(loss_hist))))
  sgd_loss_rows[[length(sgd_loss_rows) + 1]] <- data.frame(Iteration = sample_ix, Loss = loss_hist[sample_ix], BatchSize = factor(bs, levels = batch_sizes))
  sgd_summary_rows[[length(sgd_summary_rows) + 1]] <- data.frame(
    BatchSize = bs, Epochs = epochs, 初始学习率 = 0.01,
    最终MSE = round(mean((y - pred)^2), 6),
    R2 = round(1 - sum((y - pred)^2) / sum((y - mean(y))^2), 4),
    收敛迭代次数 = length(loss_hist)
  )
}

sgd_loss_df <- dplyr::bind_rows(sgd_loss_rows)
sgd_summary_df <- dplyr::bind_rows(sgd_summary_rows)

# 绘图
plot_list <- list()

plot_compute <- dplyr::bind_rows(lapply(names(all_results), function(size) {
  dplyr::bind_rows(lapply(names(all_results[[size]]), function(model) {
    z <- all_results[[size]][[model]]
    data.frame(SampleSize = factor(size, levels = as.character(sample_sizes)), Model = model, Time = z$time, MSE = z$mse, R2 = z$r2)
  }))
}))

p1 <- ggplot(plot_compute, aes(x = SampleSize, y = Time, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "white", linewidth = 0.3) +
  scale_y_log10(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "模型计算效率比较", subtitle = "不同样本规模下的时间开销", x = "样本量", y = "计算时间（秒，对数坐标）", fill = "模型") +
  theme_cn() + theme(legend.position = "bottom") +
  scale_fill_manual(values = palette_main, labels = model_labels)
plot_list[["图1_计算时间对比"]] <- p1

res_large <- all_results[[as.character(max(sample_sizes))]]
plot_mse <- dplyr::bind_rows(lapply(names(res_large), function(model) data.frame(Model = model, MSE = res_large[[model]]$mse))) |>
  dplyr::arrange(MSE) |>
  dplyr::mutate(Model = factor(Model, levels = Model))

p2 <- ggplot(plot_mse, aes(x = Model, y = MSE, fill = Model)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.4f", MSE)), vjust = -0.5, size = 3.5, family = font_family) +
  labs(title = "统计精度比较", subtitle = "样本量最大的情形下 MSE 排序", x = "模型", y = "均方误差（MSE）") +
  theme_cn() + theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_manual(values = palette_main, labels = model_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
plot_list[["图2_MSE对比"]] <- p2

p3 <- ggplot(dist_df, aes(x = K, y = Speedup, color = Method, linetype = Method)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(fill = Method), size = 3.5, shape = 21, color = "white", stroke = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(title = "分布式线性回归加速效果", subtitle = "与理想线性加速对比", x = "节点数量 K", y = "加速比", color = "方法", linetype = "方法") +
  theme_cn() + theme(legend.position = "bottom") +
  scale_color_manual(values = palette_dist) + scale_fill_manual(values = palette_dist, guide = "none") +
  scale_x_continuous(breaks = K_values)
plot_list[["图3_加速比曲线"]] <- p3

# 反转顺序以便在横向柱图中把全数据基准放在最上方
blb_df$Method <- factor(blb_df$Method, levels = rev(unique(blb_df$Method)))
p4 <- ggplot(blb_df, aes(x = Method, y = Coverage, fill = Method)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#C44E52", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f%%", Coverage * 100)), hjust = -0.15, size = 3.2, family = font_family) +
  labs(title = "BLB 区间覆盖率", subtitle = "红线为 95% 目标覆盖率", x = "方法", y = "覆盖率") +
  theme_cn() + theme(legend.position = "none") +
  scale_fill_manual(values = palette_blb) +
  scale_y_continuous(labels = percent, limits = c(0, 1.05), expand = c(0, 0)) +
  coord_flip()
plot_list[["图4_BLB覆盖率"]] <- p4

p5 <- ggplot(sgd_loss_df, aes(x = Iteration, y = Loss, color = BatchSize)) +
  geom_line(linewidth = 0.8, alpha = 0.85) +
  labs(title = "SGD 收敛轨迹", subtitle = "批大小影响优化稳定性与速度", x = "迭代次数", y = "损失值（MSE，对数坐标）", color = "批大小") +
  theme_cn() + theme(legend.position = "right") +
  scale_color_manual(values = palette_batch) + scale_y_log10()
plot_list[["图5_SGD收敛曲线"]] <- p5

complexity <- c(OLS = 1, Polynomial = 2, Ridge = 2, GAM = 4, Kernel = 5, Partial_Linear = 3)
plot_complex <- dplyr::bind_rows(lapply(names(res_large), function(model) {
  data.frame(Model = model, Complexity = complexity[model], MSE = res_large[[model]]$mse, R2 = res_large[[model]]$r2, Time = res_large[[model]]$time)
}))

p6 <- ggplot(plot_complex, aes(x = Complexity, y = MSE, size = Time, color = Model)) +
  geom_point(alpha = 0.85, stroke = 0.8) +
  geom_text(aes(label = model_labels[Model]), vjust = -1.1, size = 3.5, family = font_family) +
  labs(title = "模型复杂度-精度-时间权衡", subtitle = "用于方法选择的三维对照图", x = "模型复杂度评分", y = "均方误差（MSE）", size = "计算时间（秒）", color = "模型") +
  theme_cn() + theme(legend.position = "right") +
  scale_color_manual(values = palette_main, labels = model_labels) +
  scale_size_continuous(range = c(3, 15))
plot_list[["图6_复杂度精度权衡"]] <- p6

save_to_ppt(plot_list, file.path(output_dir, "simulation_figures.pptx"))

for (n in sample_sizes) {
  res <- all_results[[as.character(n)]]
  tbl <- data.frame(模型 = names(res), 计算时间_秒 = round(sapply(res, `[[`, "time"), 4), MSE = round(sapply(res, `[[`, "mse"), 6), R2 = round(sapply(res, `[[`, "r2"), 4))
  write_csv_bom(tbl[order(tbl$MSE), ], file.path(output_dir, sprintf("sim_table1_model_n%d.csv", n)))
}

dist_table <- dplyr::bind_rows(lapply(K_values, function(K) {
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
write_csv_bom(data.frame(
  场景 = c("快速原型", "高精度需求", "大数据集", "非线性关系", "可解释性优先"),
  推荐模型 = c("OLS", "GAM", "分布式OLS", "GAM/核回归", "OLS/多项式"),
  理由 = c("计算最快，易于实现", "能捕捉复杂非线性关系", "分布式计算可加速", "非参数方法更灵活", "线性模型易于解释"),
  注意事项 = c("假设线性关系", "计算成本较高", "需要多核环境", "需要更多数据", "可能欠拟合")
), file.path(output_dir, "sim_table5_recommendations.csv"))

message("simulation.R 执行完成，结果保存在 output/ 目录")
