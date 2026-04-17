# ============================================================================
# 实证分析：基于 NYC Yellow Taxi 数据的海量数据统计推断（精简 + 加速版）
# ============================================================================

rm(list = ls())
set.seed(42)

# ----------------------------------------------------------------------------
# 1) 依赖与通用设置
# ----------------------------------------------------------------------------
required_pkgs <- c("dplyr", "ggplot2", "arrow", "officer", "rvg", "scales", "viridis")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
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

palette_var <- c("fare_amount" = "#4472C4", "tip_amount" = "#ED7D31", "trip_distance" = "#70AD47")
palette_payment <- c("信用卡" = "#4472C4", "现金" = "#ED7D31")
palette_coef <- c("截距" = "#4472C4", "行程距离" = "#ED7D31", "乘客数" = "#70AD47")
palette_method <- c("全量OLS" = "#4472C4", "1%子样本OLS" = "#ED7D31", "模拟分布式SAE" = "#70AD47")

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

n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
map_reps <- function(X, FUN, ...) {
  if (.Platform$OS.type == "unix" && n_cores > 1L) {
    parallel::mclapply(X, FUN, ..., mc.cores = n_cores)
  } else {
    lapply(X, FUN, ...)
  }
}

fast_lm_coef <- function(X, y) {
  fit <- lm.fit(x = X, y = y)
  b <- fit$coefficients
  b[is.na(b)] <- 0
  b
}

fast_wls_coef <- function(X, y, w) {
  fit <- lm.wfit(x = X, y = y, w = w)
  b <- fit$coefficients
  b[is.na(b)] <- 0
  b
}

message("=== 环境初始化完成 ===")

# ----------------------------------------------------------------------------
# 2) 数据读取与清洗
# ----------------------------------------------------------------------------
taxi_file <- file.path(getwd(), "data", "yellow_tripdata_2023-01.parquet")
if (!file.exists(taxi_file)) stop("数据文件不存在: ", taxi_file)

message("正在读取出租车数据...")
taxi_clean <- read_parquet(taxi_file) |>
  filter(
    fare_amount > 0, fare_amount < 500,
    trip_distance > 0, trip_distance < 100,
    tip_amount >= 0, tip_amount < 200,
    passenger_count > 0, passenger_count <= 6
  )
message(sprintf("清洗后数据维度: %d 行 × %d 列", nrow(taxi_clean), ncol(taxi_clean)))

plot_list <- list()

# ----------------------------------------------------------------------------
# 实验1：简单随机子抽样推断
# ----------------------------------------------------------------------------
message("\n=== 实验1：简单随机子抽样推断 ===")

n_subsamples <- 30
subsample_size <- 10000
variables <- c("fare_amount", "tip_amount", "trip_distance")
N <- nrow(taxi_clean)

subsample_results <- lapply(variables, function(var) {
  true_mean <- mean(taxi_clean[[var]], na.rm = TRUE)
  reps <- map_reps(seq_len(n_subsamples), function(i) {
    idx <- sample.int(N, subsample_size)
    v <- taxi_clean[[var]][idx]
    est <- mean(v)
    se <- sd(v) / sqrt(subsample_size)
    c(rep_id = i, estimate = est, se = se, ci_lower = est - 1.96 * se, ci_upper = est + 1.96 * se)
  })
  out <- as.data.frame(do.call(rbind, reps))
  out$covers_true <- out$ci_lower <= true_mean & true_mean <= out$ci_upper
  out
})
names(subsample_results) <- variables

subsample_summary <- bind_rows(lapply(variables, function(var) {
  true_mean <- mean(taxi_clean[[var]], na.rm = TRUE)
  est <- subsample_results[[var]]$estimate
  data.frame(
    变量 = var,
    真实均值 = round(true_mean, 4),
    子抽样均值 = round(mean(est), 4),
    子抽样标准误 = round(sd(est), 4),
    覆盖率 = round(mean(subsample_results[[var]]$covers_true), 4),
    相对偏差 = round((mean(est) - true_mean) / true_mean * 100, 4)
  )
}))
print(subsample_summary)

fare_res <- subsample_results[["fare_amount"]]
true_fare <- subsample_summary$真实均值[subsample_summary$变量 == "fare_amount"]
cover_fare <- subsample_summary$覆盖率[subsample_summary$变量 == "fare_amount"]
plot_list[["图1_车费子抽样估计"]] <- ggplot(fare_res, aes(x = factor(rep_id), y = estimate)) +
  geom_point(color = "#4472C4", size = 2.2, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, alpha = 0.45, linewidth = 0.45) +
  geom_hline(yintercept = true_fare, color = "#C44E52", linetype = "dashed", linewidth = 1) +
  labs(
    title = "车费均值的子抽样估计稳定性",
    subtitle = sprintf("红线为全数据均值 %.2f，95%% 区间覆盖率 %.1f%%", true_fare, cover_fare * 100),
    x = "子样本编号", y = "车费均值估计（美元）"
  ) +
  theme_cn() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

var_labels <- c(fare_amount = "车费", tip_amount = "小费", trip_distance = "行程距离")
plot_density <- bind_rows(lapply(variables, function(var) data.frame(Variable = var, Estimate = subsample_results[[var]]$estimate)))
plot_list[["图2_子抽样估计分布"]] <- ggplot(plot_density, aes(x = Estimate, fill = Variable)) +
  geom_density(alpha = 0.8, color = "white", linewidth = 0.45) +
  facet_wrap(~Variable, scales = "free", labeller = labeller(Variable = var_labels)) +
  labs(title = "简单随机子抽样估计量分布", x = "估计值", y = "密度") +
  theme_cn() +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette_var, labels = var_labels)

# ----------------------------------------------------------------------------
# 实验2：分层子抽样推断（按支付方式）
# ----------------------------------------------------------------------------
message("\n=== 实验2：分层子抽样推断 ===")

payment_data <- taxi_clean |>
  filter(payment_type %in% c(1, 2)) |>
  mutate(payment_type = if_else(payment_type == 1, "信用卡", "现金"))

n_reps <- 50
sample_per_stratum <- 5000
strata_idx <- split(seq_len(nrow(payment_data)), payment_data$payment_type)

strat_results <- bind_rows(map_reps(seq_len(n_reps), function(i) {
  tips <- lapply(names(strata_idx), function(g) {
    idx <- sample(strata_idx[[g]], sample_per_stratum)
    data.frame(payment_type = g, mean_tip = mean(payment_data$tip_amount[idx]), rep_id = i)
  })
  bind_rows(tips)
}))

true_by_payment <- payment_data |>
  summarise(true_mean = mean(tip_amount), .by = payment_type)

strat_summary <- strat_results |>
  summarise(
    est_mean = mean(mean_tip),
    se = sd(mean_tip),
    ci_lower = quantile(mean_tip, 0.025),
    ci_upper = quantile(mean_tip, 0.975),
    .by = payment_type
  ) |>
  left_join(true_by_payment, by = "payment_type")
print(strat_summary)

plot_list[["图3_分层小费分布"]] <- ggplot(strat_results, aes(x = payment_type, y = mean_tip, fill = payment_type)) +
  geom_boxplot(alpha = 0.85, color = "#333333", linewidth = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 0.8, color = "#333333") +
  geom_point(data = true_by_payment, aes(y = true_mean), color = "#C44E52", size = 4, shape = 18) +
  scale_fill_manual(values = palette_payment) +
  labs(title = "按支付方式分层的小费均值分布", subtitle = "红色菱形为全数据真实均值", x = "支付方式", y = "平均小费（美元）") +
  theme_cn() +
  theme(legend.position = "none")

# ----------------------------------------------------------------------------
# 实验3：BLB 回归推断
# ----------------------------------------------------------------------------
message("\n=== 实验3：BLB 回归推断 ===")

n_sub <- 20
sub_size <- 10000
n_boot <- 300

X_full <- model.matrix(~ trip_distance + passenger_count, data = taxi_clean)
y_full <- taxi_clean$fare_amount

blb_list <- map_reps(seq_len(n_sub), function(i) {
  idx <- sample.int(nrow(taxi_clean), sub_size)
  X_sub <- X_full[idx, , drop = FALSE]
  y_sub <- y_full[idx]

  coef_sub <- fast_lm_coef(X_sub, y_sub)
  boot_coef <- replicate(n_boot, {
    w <- as.numeric(rmultinom(1, size = sub_size, prob = rep.int(1 / sub_size, sub_size)))
    fast_wls_coef(X_sub, y_sub, w)
  })
  boot_coef <- t(boot_coef)

  data.frame(
    subsample_id = i,
    intercept = coef_sub[1],
    beta_distance = coef_sub[2],
    beta_passenger = coef_sub[3],
    se_distance = sd(boot_coef[, 2]),
    ci_lower_distance = quantile(boot_coef[, 2], 0.025),
    ci_upper_distance = quantile(boot_coef[, 2], 0.975)
  )
})
blb_results <- bind_rows(blb_list)

# 全数据模型（用于精度基准）
t0 <- Sys.time()
coef_full <- fast_lm_coef(X_full, y_full)
full_time <- as.numeric(Sys.time() - t0, units = "secs")

blb_summary <- data.frame(
  参数 = c("截距", "行程距离系数", "乘客数系数"),
  BLB估计 = c(mean(blb_results$intercept), mean(blb_results$beta_distance), mean(blb_results$beta_passenger)),
  BLB标准误 = c(sd(blb_results$intercept), sd(blb_results$beta_distance), sd(blb_results$beta_passenger)),
  全数据估计 = coef_full,
  全数据时间_秒 = round(full_time, 4)
)
blb_summary$相对偏差 <- round((blb_summary$BLB估计 - blb_summary$全数据估计) / blb_summary$全数据估计 * 100, 4)
print(blb_summary)

plot_list[["图4_BLB行程距离系数"]] <- ggplot(blb_results, aes(x = factor(subsample_id), y = beta_distance)) +
  geom_point(color = "#4472C4", size = 2.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower_distance, ymax = ci_upper_distance), width = 0.3, alpha = 0.45, linewidth = 0.45) +
  geom_hline(yintercept = coef_full[2], color = "#C44E52", linetype = "dashed", linewidth = 1) +
  labs(
    title = "行程距离系数的 BLB 估计稳定性",
    subtitle = sprintf("红线为全数据估计 %.3f，BLB 均值 %.3f", coef_full[2], mean(blb_results$beta_distance)),
    x = "子样本编号", y = "系数估计（美元/英里）"
  ) +
  theme_cn() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

blb_coef_long <- data.frame(
  参数 = rep(c("截距", "行程距离", "乘客数"), each = nrow(blb_results)),
  估计值 = c(blb_results$intercept, blb_results$beta_distance, blb_results$beta_passenger)
)
true_vals <- data.frame(参数 = c("截距", "行程距离", "乘客数"), 真实值 = coef_full)

plot_list[["图5_BLB系数分布"]] <- ggplot(blb_coef_long, aes(x = 参数, y = 估计值, fill = 参数)) +
  geom_violin(alpha = 0.82, color = "white", linewidth = 0.35, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.9, color = "#333333", linewidth = 0.3, outlier.shape = NA) +
  geom_point(data = true_vals, aes(y = 真实值), color = "#C44E52", size = 4, shape = 18, inherit.aes = TRUE) +
  labs(title = "BLB 回归系数分布", subtitle = "红色菱形为全数据估计值", x = "参数", y = "估计值") +
  theme_cn() +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette_coef)

# ----------------------------------------------------------------------------
# 实验4：回归计算时间对比
# ----------------------------------------------------------------------------
message("\n=== 实验4：大数据回归计算时间对比 ===")

X_time <- model.matrix(~ trip_distance + passenger_count + tip_amount, data = taxi_clean)
y_time <- taxi_clean$fare_amount

# 全量 OLS
t0 <- Sys.time()
coef_all <- fast_lm_coef(X_time, y_time)
time_all <- as.numeric(Sys.time() - t0, units = "secs")

# 1% 子样本 OLS
idx_1pct <- sample.int(nrow(taxi_clean), round(nrow(taxi_clean) * 0.01))
X_1pct <- X_time[idx_1pct, , drop = FALSE]
y_1pct <- y_time[idx_1pct]
t0 <- Sys.time()
coef_1pct <- fast_lm_coef(X_1pct, y_1pct)
time_1pct <- as.numeric(Sys.time() - t0, units = "secs")

# 模拟分布式 SAE
K <- 10
idx <- sample(rep(seq_len(K), length.out = nrow(taxi_clean)))
split_idx <- split(seq_len(nrow(taxi_clean)), idx)
t0 <- Sys.time()
local_coef <- vapply(split_idx, function(ik) fast_lm_coef(X_time[ik, , drop = FALSE], y_time[ik]), numeric(ncol(X_time)))
coef_sae <- rowMeans(local_coef)
time_dist <- as.numeric(Sys.time() - t0, units = "secs")

time_compare <- data.frame(
  方法 = c("全量OLS", "1%子样本OLS", "模拟分布式SAE"),
  计算时间_秒 = round(c(time_all, time_1pct, time_dist), 4),
  样本量 = c(nrow(taxi_clean), length(idx_1pct), nrow(taxi_clean)),
  与全量估计L2误差 = c(0, sqrt(sum((coef_1pct - coef_all)^2)), sqrt(sum((coef_sae - coef_all)^2)))
)
print(time_compare)

plot_list[["图6_大数据计算时间对比"]] <- ggplot(time_compare, aes(x = 方法, y = 计算时间_秒, fill = 方法)) +
  geom_col(width = 0.62, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.3f s", 计算时间_秒)), vjust = -0.45, size = 4, family = font_family) +
  labs(title = "大数据回归计算时间对比", x = "方法", y = "计算时间（秒）") +
  theme_cn() +
  theme(legend.position = "none") +
  scale_fill_manual(values = palette_method) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)))

# 导出 PPTX
save_to_ppt(plot_list, file.path(output_dir, "empirical_figures.pptx"))

# ----------------------------------------------------------------------------
# 表格输出
# ----------------------------------------------------------------------------
message("\n=== 生成表格输出 ===")
write_csv_bom(subsample_summary, file.path(output_dir, "emp_table1_subsample_summary.csv"))
write_csv_bom(strat_summary, file.path(output_dir, "emp_table2_stratified_summary.csv"))
write_csv_bom(blb_summary, file.path(output_dir, "emp_table3_blb_regression.csv"))
write_csv_bom(time_compare, file.path(output_dir, "emp_table4_time_comparison.csv"))
write_csv_bom(subsample_results[["fare_amount"]], file.path(output_dir, "emp_table5_fare_subsample_detail.csv"))

message("\n=== 实证分析全部完成，结果保存在 output/ 目录 ===")
