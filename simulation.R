# ============================================================================
# 数值模拟：海量数据统计计算方法
# ============================================================================
# 本脚本为顺序执行脚本，不封装函数。
# 包含：模型比较、分布式计算、BLB子抽样、SGD小批次优化。
# 所有图表横纵坐标使用中文，并汇总导出为可编辑 PPTX。
# ============================================================================

rm(list = ls())
set.seed(42)

# ----------------------------------------------------------------------------
# 1. 包依赖与初始化
# ----------------------------------------------------------------------------
packages <- c("MASS", "glmnet", "mgcv", "np", "ggplot2", "dplyr", "tidyr",
              "reshape2", "scales", "viridis", "microbenchmark",
              "officer", "rvg", "svglite")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Windows 中文字体设置（避免乱码）
if (.Platform$OS.type == "windows") {
  windowsFonts(YaHei = windowsFont("微软雅黑"))
  font_family <- "YaHei"
} else {
  font_family <- "sans"
}

theme_cn <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = font_family) +
    theme(
      text = element_text(family = font_family),
      plot.title = element_text(family = font_family, face = "bold"),
      plot.subtitle = element_text(family = font_family),
      axis.title = element_text(family = font_family),
      axis.text = element_text(family = font_family),
      legend.title = element_text(family = font_family),
      legend.text = element_text(family = font_family),
      strip.text = element_text(family = font_family)
    )
}

# PPT 导出辅助：将多张 ggplot 汇总到一个可编辑 pptx
plot_list <- list()

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

# CSV 写出辅助（UTF-8 BOM，兼容 Excel 中文）
write_csv_bom <- function(df, path) {
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8-BOM")
  message("表格已保存: ", path)
}

message("=== 环境初始化完成 ===")

# ----------------------------------------------------------------------------
# 2. 数据生成参数
# ----------------------------------------------------------------------------
beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)
p <- 8
rho <- 0.5
sigma <- 1

# AR(1) 协方差矩阵
Sigma <- matrix(0, p, p)
for (i in 1:p) {
  for (j in 1:p) {
    Sigma[i, j] <- rho^abs(i - j)
  }
}

# 样本量设置
sample_sizes <- c(1000, 10000, 100000)
n_large <- max(sample_sizes)
n_medium <- median(sample_sizes)

# ----------------------------------------------------------------------------
# 实验1：不同样本量的模型比较
# ----------------------------------------------------------------------------
message("\n=== 实验1：不同样本量的模型比较 ===")
all_results <- list()

for (n in sample_sizes) {
  message(sprintf("--- 样本量 n = %d ---", n))
  
  # 生成数据
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  colnames(X) <- c("income", "age", "rooms", "distance",
                   "crime_rate", "school_rating", "tax_rate", "employment")
  X_design <- cbind(1, X)
  y <- as.vector(X_design %*% beta_true + rnorm(n, 0, sigma))
  
  res_n <- list()
  
  # 1. OLS
  t0 <- Sys.time()
  fit_ols <- lm(y ~ ., data = data.frame(y = y, X))
  t_ols <- as.numeric(Sys.time() - t0, units = "secs")
  pred_ols <- predict(fit_ols)
  res_n$OLS <- list(time = t_ols,
                    mse = mean((y - pred_ols)^2),
                    r2 = summary(fit_ols)$r.squared)
  message(sprintf("OLS: 时间 %.4f 秒, MSE %.6f", res_n$OLS$time, res_n$OLS$mse))
  
  # 2. 多项式回归（income, age 用二次正交多项式）
  t0 <- Sys.time()
  fit_poly <- lm(y ~ poly(income, 2) + poly(age, 2) + rooms + distance +
                   crime_rate + school_rating + tax_rate + employment,
                 data = data.frame(y = y, X))
  t_poly <- as.numeric(Sys.time() - t0, units = "secs")
  pred_poly <- predict(fit_poly)
  res_n$Polynomial <- list(time = t_poly,
                           mse = mean((y - pred_poly)^2),
                           r2 = summary(fit_poly)$r.squared)
  message(sprintf("Polynomial: 时间 %.4f 秒, MSE %.6f", res_n$Polynomial$time, res_n$Polynomial$mse))
  
  # 3. 岭回归
  t0 <- Sys.time()
  cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 5, standardize = TRUE)
  fit_ridge <- glmnet(X, y, alpha = 0, lambda = cv_ridge$lambda.min, standardize = TRUE)
  t_ridge <- as.numeric(Sys.time() - t0, units = "secs")
  pred_ridge <- as.vector(predict(fit_ridge, newx = X))
  ss_res <- sum((y - pred_ridge)^2)
  ss_tot <- sum((y - mean(y))^2)
  res_n$Ridge <- list(time = t_ridge,
                      mse = mean((y - pred_ridge)^2),
                      r2 = 1 - ss_res / ss_tot,
                      lambda = cv_ridge$lambda.min)
  message(sprintf("Ridge: 时间 %.4f 秒, MSE %.6f, Lambda %.4f", res_n$Ridge$time, res_n$Ridge$mse, res_n$Ridge$lambda))
  
  # 4. GAM
  t0 <- Sys.time()
  df_gam <- data.frame(y = y, X)
  fit_gam <- mgcv::gam(y ~ s(income) + s(age) + s(rooms) + distance +
                         crime_rate + school_rating + tax_rate + employment,
                       data = df_gam, method = "REML")
  t_gam <- as.numeric(Sys.time() - t0, units = "secs")
  pred_gam <- predict(fit_gam)
  res_n$GAM <- list(time = t_gam,
                    mse = mean((y - pred_gam)^2),
                    r2 = summary(fit_gam)$r.sq)
  message(sprintf("GAM: 时间 %.4f 秒, MSE %.6f", res_n$GAM$time, res_n$GAM$mse))
  
  # 5. 核回归（仅对 income 单变量做局部线性核回归）
  t0 <- Sys.time()
  bw_obj <- np::npregbw(xdat = X[, "income"], ydat = y, regtype = "ll")
  fit_kernel <- np::npreg(bw_obj)
  t_kernel <- as.numeric(Sys.time() - t0, units = "secs")
  pred_kernel <- fitted(fit_kernel)
  res_n$Kernel <- list(time = t_kernel,
                       mse = mean((y - pred_kernel)^2),
                       r2 = 1 - sum((y - pred_kernel)^2) / sum((y - mean(y))^2),
                       bandwidth = fit_kernel$bw)
  message(sprintf("Kernel: 时间 %.4f 秒, MSE %.6f", res_n$Kernel$time, res_n$Kernel$mse))
  
  # 6. 部分线性模型
  t0 <- Sys.time()
  fit_pl <- mgcv::gam(y ~ rooms + distance + crime_rate + school_rating +
                        tax_rate + employment + s(income),
                      data = df_gam, method = "REML")
  t_pl <- as.numeric(Sys.time() - t0, units = "secs")
  pred_pl <- predict(fit_pl)
  res_n$Partial_Linear <- list(time = t_pl,
                               mse = mean((y - pred_pl)^2),
                               r2 = summary(fit_pl)$r.sq)
  message(sprintf("Partial Linear: 时间 %.4f 秒, MSE %.6f", res_n$Partial_Linear$time, res_n$Partial_Linear$mse))
  
  all_results[[as.character(n)]] <- res_n
}

# ----------------------------------------------------------------------------
# 实验2：分布式计算（基于最大样本量）
# ----------------------------------------------------------------------------
message("\n=== 实验2：分布式计算模拟 ===")
n <- n_large
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- c("income", "age", "rooms", "distance",
                 "crime_rate", "school_rating", "tax_rate", "employment")
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))

# 基准 OLS
t0 <- Sys.time()
fit_full <- lm(y ~ ., data = data.frame(y = y, X))
base_time <- as.numeric(Sys.time() - t0, units = "secs")
base_coef <- coef(fit_full)

K_values <- c(5, 10, 20, 50)
dist_df <- data.frame()

for (K in K_values) {
  message(sprintf("分布式 K = %d ...", K))
  
  # SAE
  t0 <- Sys.time()
  idx <- sample(rep(1:K, length.out = n))
  local_est <- matrix(0, K, p + 1)
  for (k in 1:K) {
    ik <- which(idx == k)
    local_est[k, ] <- coef(lm(y ~ ., data = data.frame(y = y[ik], X[ik, , drop = FALSE])))
  }
  sae_est <- colMeans(local_est)
  sae_time <- as.numeric(Sys.time() - t0, units = "secs")
  
  # One-step
  t0 <- Sys.time()
  pilot_idx <- sample(1:n, min(1000, n))
  beta0 <- coef(lm(y ~ ., data = data.frame(y = y[pilot_idx], X[pilot_idx, , drop = FALSE])))
  idx2 <- sample(rep(1:K, length.out = n))
  grads <- matrix(0, K, p + 1)
  for (k in 1:K) {
    ik <- which(idx2 == k)
    Xd <- cbind(1, X[ik, , drop = FALSE])
    resid <- y[ik] - Xd %*% beta0
    grads[k, ] <- -2 * t(Xd) %*% resid / length(ik)
  }
  avg_grad <- colMeans(grads)
  Xd_full <- cbind(1, X)
  fisher <- 2 * t(Xd_full) %*% Xd_full / n
  onestep_est <- beta0 - solve(fisher) %*% avg_grad
  onestep_time <- as.numeric(Sys.time() - t0, units = "secs")
  
  dist_df <- rbind(dist_df,
                   data.frame(K = K, Method = "SAE", Time = sae_time,
                              Speedup = base_time / sae_time),
                   data.frame(K = K, Method = "One-step", Time = onestep_time,
                              Speedup = base_time / onestep_time))
}

dist_df <- rbind(dist_df,
                 data.frame(K = K_values, Method = "Ideal",
                            Time = base_time / K_values, Speedup = K_values))

# ----------------------------------------------------------------------------
# 实验3：BLB 子抽样（基于中等样本量）
# ----------------------------------------------------------------------------
message("\n=== 实验3：BLB 子抽样 ===")
n <- n_medium
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
colnames(X) <- c("income", "age", "rooms", "distance",
                 "crime_rate", "school_rating", "tax_rate", "employment")
y <- as.vector(cbind(1, X) %*% beta_true + rnorm(n, 0, sigma))

gamma_values <- c(0.6, 0.7)
s_values <- c(5, 10, 20)
blb_df <- data.frame()

# 基准：全数据 bootstrap（500 次，减少耗时）
message("计算全数据 Bootstrap 基准...")
B_full <- 500
full_boot_est <- matrix(0, B_full, p + 1)
for (b in 1:B_full) {
  boot_idx <- sample(1:n, n, replace = TRUE)
  full_boot_est[b, ] <- coef(lm(y ~ ., data = data.frame(y = y[boot_idx], X[boot_idx, , drop = FALSE])))
}
full_boot_se <- apply(full_boot_est, 2, sd)
full_boot_mean <- colMeans(full_boot_est)
full_cov <- mean(beta_true >= (full_boot_mean - 1.96 * full_boot_se) &
                   beta_true <= (full_boot_mean + 1.96 * full_boot_se))

for (gamma in gamma_values) {
  for (s in s_values) {
    message(sprintf("BLB: gamma=%.1f, s=%d", gamma, s))
    m <- floor(n^gamma)
    B <- 100
    subset_means <- matrix(0, s, p + 1)
    subset_vars <- matrix(0, s, p + 1)
    
    for (i in 1:s) {
      sub_idx <- sample(1:n, m, replace = FALSE)
      X_sub <- X[sub_idx, , drop = FALSE]
      y_sub <- y[sub_idx]
      boot_est <- matrix(0, B, p + 1)
      for (b in 1:B) {
        bidx <- sample(1:m, m, replace = TRUE)
        boot_est[b, ] <- coef(lm(y ~ ., data = data.frame(y = y_sub[bidx], X_sub[bidx, , drop = FALSE])))
      }
      subset_means[i, ] <- colMeans(boot_est)
      subset_vars[i, ] <- apply(boot_est, 2, var)
    }
    
    blb_est <- colMeans(subset_means)
    blb_se <- sqrt(colMeans(subset_vars))
    ci_lower <- blb_est - 1.96 * blb_se
    ci_upper <- blb_est + 1.96 * blb_se
    coverage <- mean(beta_true >= ci_lower & beta_true <= ci_upper)
    ci_width <- mean(ci_upper - ci_lower)
    
    blb_df <- rbind(blb_df, data.frame(
      Gamma = gamma, S = s, Coverage = coverage, CI_Width = ci_width,
      Method = sprintf("γ=%.1f, s=%d", gamma, s)
    ))
  }
}

blb_df <- rbind(blb_df,
                data.frame(Gamma = NA, S = NA, Coverage = full_cov,
                           CI_Width = mean(2 * 1.96 * full_boot_se),
                           Method = "全数据Bootstrap"))

# ----------------------------------------------------------------------------
# 实验4：SGD 小批次优化（基于中等样本量）
# ----------------------------------------------------------------------------
message("\n=== 实验4：SGD 小批次优化 ===")
# 复用实验3的数据
X_design <- cbind(1, X)
batch_sizes <- c(32, 64, 128, 256, 512)
sgd_loss_df <- data.frame()
sgd_summary_df <- data.frame()

for (bs in batch_sizes) {
  message(sprintf("SGD: batch_size = %d", bs))
  beta <- rnorm(p + 1, 0, 0.1)
  lr <- 0.01
  epochs <- 50
  loss_hist <- numeric(epochs * ceiling(n / bs))
  iter <- 1
  
  for (epoch in 1:epochs) {
    shuffle <- sample(1:n)
    Xs <- X_design[shuffle, , drop = FALSE]
    ys <- y[shuffle]
    num_batches <- ceiling(n / bs)
    for (b in 1:num_batches) {
      start_idx <- (b - 1) * bs + 1
      end_idx <- min(b * bs, n)
      Xb <- Xs[start_idx:end_idx, , drop = FALSE]
      yb <- ys[start_idx:end_idx]
      resid <- yb - Xb %*% beta
      grad <- -2 * t(Xb) %*% resid / length(yb)
      beta <- beta - lr * as.vector(grad)
      if (iter <= length(loss_hist)) {
        loss_hist[iter] <- mean((y - X_design %*% beta)^2)
      }
      iter <- iter + 1
    }
    lr <- lr * 0.99
  }
  
  loss_hist <- loss_hist[1:(iter - 1)]
  pred_sgd <- X_design %*% beta
  mse_sgd <- mean((y - pred_sgd)^2)
  r2_sgd <- 1 - sum((y - pred_sgd)^2) / sum((y - mean(y))^2)
  t_sgd <- NA  # 未单独计时，用于汇总表占位
  
  # 采样用于绘图
  sample_idx <- round(seq(1, length(loss_hist), length.out = min(200, length(loss_hist))))
  sgd_loss_df <- rbind(sgd_loss_df,
                       data.frame(Iteration = sample_idx,
                                  Loss = loss_hist[sample_idx],
                                  BatchSize = factor(bs, levels = batch_sizes)))
  
  sgd_summary_df <- rbind(sgd_summary_df,
                          data.frame(
                            BatchSize = bs, Epochs = epochs,
                            初始学习率 = 0.01, 最终MSE = round(mse_sgd, 6),
                            R2 = round(r2_sgd, 4), 收敛迭代次数 = length(loss_hist)
                          ))
}

# ----------------------------------------------------------------------------
# 可视化
# ----------------------------------------------------------------------------
message("\n=== 生成可视化图表 ===")

# 图1：不同样本量下的计算时间对比
plot_compute <- data.frame()
for (size in as.character(sample_sizes)) {
  res <- all_results[[size]]
  for (model in names(res)) {
    plot_compute <- rbind(plot_compute, data.frame(
      SampleSize = factor(size, levels = as.character(sample_sizes)),
      Model = model, Time = res[[model]]$time, MSE = res[[model]]$mse, R2 = res[[model]]$r2
    ))
  }
}

model_labels <- c(
  OLS = "OLS", Polynomial = "多项式回归", Ridge = "岭回归",
  GAM = "GAM", Kernel = "核回归", Partial_Linear = "部分线性模型"
)

p1 <- ggplot(plot_compute, aes(x = SampleSize, y = Time, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10() +
  labs(x = "样本量", y = "计算时间（秒，对数坐标）", fill = "模型") +
  theme_cn(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") +
  scale_fill_viridis_d(labels = model_labels)
plot_list[["图1_计算时间对比"]] <- p1

# 图2：模型 MSE 对比（最大样本量）
res_large <- all_results[[as.character(n_large)]]
plot_mse <- data.frame()
for (model in names(res_large)) {
  plot_mse <- rbind(plot_mse, data.frame(
    Model = model, MSE = res_large[[model]]$mse
  ))
}
plot_mse$Model <- factor(plot_mse$Model, levels = plot_mse$Model[order(plot_mse$MSE)])

p2 <- ggplot(plot_mse, aes(x = Model, y = MSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", MSE)), vjust = -0.5, size = 3.5, family = font_family) +
  labs(x = "模型", y = "均方误差（MSE）") +
  theme_cn(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_viridis_d(labels = model_labels) +
  scale_x_discrete(labels = model_labels)
plot_list[["图2_MSE对比"]] <- p2

# 图3：分布式计算加速比
p3 <- ggplot(dist_df, aes(x = K, y = Speedup, color = Method, linetype = Method)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", alpha = 0.7) +
  labs(x = "节点数量 K", y = "加速比", color = "方法", linetype = "方法") +
  theme_cn(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = K_values)
plot_list[["图3_加速比曲线"]] <- p3

# 图4：BLB 覆盖率
p4 <- ggplot(blb_df, aes(x = Method, y = Coverage, fill = Method)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f%%", Coverage * 100)), vjust = -0.5, size = 3.5, family = font_family) +
  labs(x = "方法", y = "覆盖率") +
  theme_cn(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent, limits = c(0, 1))
plot_list[["图4_BLB覆盖率"]] <- p4

# 图5：SGD 收敛曲线
p5 <- ggplot(sgd_loss_df, aes(x = Iteration, y = Loss, color = BatchSize)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  labs(x = "迭代次数", y = "损失值（MSE，对数坐标）", color = "批大小") +
  theme_cn(base_size = 12) +
  theme(legend.position = "right") +
  scale_color_viridis_d() +
  scale_y_log10()
plot_list[["图5_SGD收敛曲线"]] <- p5

# 图6：模型复杂度-精度权衡
complexity <- c(OLS = 1, Polynomial = 2, Ridge = 2, GAM = 4, Kernel = 5, Partial_Linear = 3)
plot_complex <- data.frame()
for (model in names(res_large)) {
  plot_complex <- rbind(plot_complex, data.frame(
    Model = model, Complexity = complexity[model],
    MSE = res_large[[model]]$mse, R2 = res_large[[model]]$r2,
    Time = res_large[[model]]$time
  ))
}

p6 <- ggplot(plot_complex, aes(x = Complexity, y = MSE, size = Time, color = Model)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = model_labels[Model]), vjust = -1, size = 3.5, family = font_family) +
  labs(x = "模型复杂度评分", y = "均方误差（MSE）",
       size = "计算时间（秒）", color = "模型") +
  theme_cn(base_size = 12) +
  theme(legend.position = "right") +
  scale_color_viridis_d(labels = model_labels) +
  scale_size_continuous(range = c(3, 15))
plot_list[["图6_复杂度精度权衡"]] <- p6

# 导出 PPT
save_to_ppt(plot_list, file.path(output_dir, "simulation_figures.pptx"))

# ----------------------------------------------------------------------------
# 表格输出
# ----------------------------------------------------------------------------
message("\n=== 生成表格输出 ===")

# 表1：各样本量模型性能对比
for (n in sample_sizes) {
  res <- all_results[[as.character(n)]]
  tbl <- data.frame(
    模型 = names(res),
    计算时间_秒 = round(sapply(res, function(x) x$time), 4),
    MSE = round(sapply(res, function(x) x$mse), 6),
    R2 = round(sapply(res, function(x) x$r2), 4)
  )
  tbl <- tbl[order(tbl$MSE), ]
  write_csv_bom(tbl, file.path(output_dir, sprintf("sim_table1_model_n%d.csv", n)))
}

# 表2：分布式计算结果
dist_table <- data.frame()
for (K in K_values) {
  dist_table <- rbind(dist_table, data.frame(
    K = K,
    SAE_时间_秒 = round(dist_df$Time[dist_df$K == K & dist_df$Method == "SAE"], 4),
    SAE_加速比 = round(dist_df$Speedup[dist_df$K == K & dist_df$Method == "SAE"], 2),
    OneStep_时间_秒 = round(dist_df$Time[dist_df$K == K & dist_df$Method == "One-step"], 4),
    OneStep_加速比 = round(dist_df$Speedup[dist_df$K == K & dist_df$Method == "One-step"], 2),
    理想加速比 = K
  ))
}
write_csv_bom(dist_table, file.path(output_dir, "sim_table2_distributed.csv"))

# 表3：BLB 结果
write_csv_bom(blb_df, file.path(output_dir, "sim_table3_blb.csv"))

# 表4：SGD 结果
write_csv_bom(sgd_summary_df, file.path(output_dir, "sim_table4_sgd.csv"))

# 表5：模型选择建议
recommendation <- data.frame(
  场景 = c("快速原型", "高精度需求", "大数据集", "非线性关系", "可解释性优先"),
  推荐模型 = c("OLS", "GAM", "分布式OLS", "GAM/核回归", "OLS/多项式"),
  理由 = c("计算最快，易于实现", "能捕捉复杂非线性关系", "分布式计算可加速",
           "非参数方法更灵活", "线性模型易于解释"),
  注意事项 = c("假设线性关系", "计算成本较高", "需要多核环境", "需要更多数据", "可能欠拟合")
)
write_csv_bom(recommendation, file.path(output_dir, "sim_table5_recommendations.csv"))

message("\n=== 数值模拟全部完成，结果保存在 output/ 目录 ===")
