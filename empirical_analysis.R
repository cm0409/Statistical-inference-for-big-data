# ============================================================================
# 实证分析：基于 NYC Yellow Taxi 数据的海量数据统计推断
# ============================================================================
# 数据集：data/yellow_tripdata_2023-01.parquet（约300万行）
# 方法：简单随机子抽样、分层子抽样、BLB回归推断、大数据回归对比
# 图表：横纵坐标中文，汇总导出为可编辑 PPTX
# ============================================================================

rm(list = ls())
set.seed(42)

# ----------------------------------------------------------------------------
# 1. 包依赖与初始化
# ----------------------------------------------------------------------------
packages <- c("dplyr", "ggplot2", "arrow", "officer", "rvg", "scales", "viridis")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Windows 中文字体设置
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

write_csv_bom <- function(df, path) {
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8-BOM")
  message("表格已保存: ", path)
}

message("=== 环境初始化完成 ===")

# ----------------------------------------------------------------------------
# 2. 数据读取与清洗
# ----------------------------------------------------------------------------
taxi_file <- file.path(getwd(), "data", "yellow_tripdata_2023-01.parquet")
if (!file.exists(taxi_file)) {
  stop("数据文件不存在: ", taxi_file)
}

message("正在读取出租车数据...")
taxi_raw <- read_parquet(taxi_file)

taxi_clean <- taxi_raw |>
  filter(
    fare_amount > 0, fare_amount < 500,
    trip_distance > 0, trip_distance < 100,
    tip_amount >= 0, tip_amount < 200,
    passenger_count > 0, passenger_count <= 6
  )

message(sprintf("清洗后数据维度: %d 行 × %d 列", nrow(taxi_clean), ncol(taxi_clean)))

# ----------------------------------------------------------------------------
# 实验1：简单随机子抽样推断（车费、小费、行程距离）
# ----------------------------------------------------------------------------
message("\n=== 实验1：简单随机子抽样推断 ===")
n_subsamples <- 30
subsample_size <- 10000
variables <- c("fare_amount", "tip_amount", "trip_distance")
subsample_results <- list()
subsample_summary <- data.frame()

for (var in variables) {
  true_mean <- mean(taxi_clean[[var]], na.rm = TRUE)
  estimates <- numeric(n_subsamples)
  ses <- numeric(n_subsamples)
  covers <- logical(n_subsamples)
  ci_lower <- numeric(n_subsamples)
  ci_upper <- numeric(n_subsamples)
  
  for (i in 1:n_subsamples) {
    idx <- sample(nrow(taxi_clean), subsample_size)
    sub <- taxi_clean[idx, ]
    est <- mean(sub[[var]], na.rm = TRUE)
    se <- sd(sub[[var]], na.rm = TRUE) / sqrt(subsample_size)
    estimates[i] <- est
    ses[i] <- se
    ci_lower[i] <- est - 1.96 * se
    ci_upper[i] <- est + 1.96 * se
    covers[i] <- (ci_lower[i] <= true_mean) && (true_mean <= ci_upper[i])
  }
  
  subsample_results[[var]] <- data.frame(
    rep_id = 1:n_subsamples,
    estimate = estimates,
    se = ses,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    covers_true = covers
  )
  
  subsample_summary <- rbind(subsample_summary, data.frame(
    变量 = var,
    真实均值 = round(true_mean, 4),
    子抽样均值 = round(mean(estimates), 4),
    子抽样标准误 = round(sd(estimates), 4),
    覆盖率 = round(mean(covers), 4),
    相对偏差 = round((mean(estimates) - true_mean) / true_mean * 100, 4)
  ))
}

message("子抽样推断汇总:")
print(subsample_summary)

# 图1：车费均值的子抽样估计及置信区间
fare_res <- subsample_results[["fare_amount"]]
p1 <- ggplot(fare_res, aes(x = factor(rep_id), y = estimate)) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, alpha = 0.5) +
  geom_hline(yintercept = subsample_summary$真实均值[subsample_summary$变量 == "fare_amount"],
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "车费均值的子抽样估计",
    subtitle = sprintf("红线为全数据均值 %.2f，子抽样覆盖率 %.1f%%",
                       subsample_summary$真实均值[subsample_summary$变量 == "fare_amount"],
                       subsample_summary$覆盖率[subsample_summary$变量 == "fare_amount"] * 100),
    x = "子样本编号", y = "车费均值估计 (美元)"
  ) +
  theme_cn(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
plot_list[["图1_车费子抽样估计"]] <- p1

# 图2：三个变量的子抽样分布密度
plot_density <- data.frame()
for (var in variables) {
  plot_density <- rbind(plot_density, data.frame(
    Variable = var,
    Estimate = subsample_results[[var]]$estimate
  ))
}

var_labels <- c(
  fare_amount = "车费",
  tip_amount = "小费",
  trip_distance = "行程距离"
)

p2 <- ggplot(plot_density, aes(x = Estimate, fill = Variable)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Variable, scales = "free", labeller = labeller(Variable = var_labels)) +
  labs(title = "简单随机子抽样的估计量分布", x = "估计值", y = "密度") +
  theme_cn(base_size = 12) +
  theme(legend.position = "none") +
  scale_fill_viridis_d(labels = var_labels)
plot_list[["图2_子抽样估计分布"]] <- p2

# ----------------------------------------------------------------------------
# 实验2：分层子抽样推断（按支付方式分析小费）
# ----------------------------------------------------------------------------
message("\n=== 实验2：分层子抽样推断 ===")
payment_data <- taxi_clean |>
  filter(payment_type %in% c(1, 2)) |>
  mutate(payment_type = if_else(payment_type == 1, "信用卡", "现金"))

n_reps <- 50
sample_per_stratum <- 5000
strat_results <- data.frame()

for (i in 1:n_reps) {
  rep_data <- payment_data |>
    slice_sample(n = sample_per_stratum, by = payment_type) |>
    summarise(mean_tip = mean(tip_amount), .by = payment_type) |>
    mutate(rep_id = i)
  strat_results <- rbind(strat_results, rep_data)
}

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

message("分层子抽样汇总:")
print(strat_summary)

# 图3：分层小费分布
p3 <- ggplot(strat_results, aes(x = payment_type, y = mean_tip, fill = payment_type)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(data = true_by_payment, aes(y = true_mean),
             color = "red", size = 4, shape = 18) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "按支付方式分层的小费子抽样分布",
    subtitle = "红色菱形为全数据真实均值",
    x = "支付方式", y = "平均小费 (美元)"
  ) +
  theme_cn(base_size = 12) +
  theme(legend.position = "none")
plot_list[["图3_分层小费分布"]] <- p3

# ----------------------------------------------------------------------------
# 实验3：BLB 回归推断（车费 ~ 行程距离 + 乘客数）
# ----------------------------------------------------------------------------
message("\n=== 实验3：BLB 回归推断 ===")
n_subsamples <- 20
subsample_size <- 10000
n_bootstrap <- 300

blb_results <- data.frame()
for (i in 1:n_subsamples) {
  sub_idx <- sample(nrow(taxi_clean), subsample_size)
  sub_data <- taxi_clean[sub_idx, ]
  fit_sub <- lm(fare_amount ~ trip_distance + passenger_count, data = sub_data)
  
  boot_intercept <- numeric(n_bootstrap)
  boot_dist <- numeric(n_bootstrap)
  boot_pass <- numeric(n_bootstrap)
  
  for (b in 1:n_bootstrap) {
    bidx <- sample(nrow(sub_data), replace = TRUE)
    fit_boot <- lm(fare_amount ~ trip_distance + passenger_count, data = sub_data[bidx, ])
    boot_intercept[b] <- coef(fit_boot)[1]
    boot_dist[b] <- coef(fit_boot)[2]
    boot_pass[b] <- coef(fit_boot)[3]
  }
  
  blb_results <- rbind(blb_results, data.frame(
    subsample_id = i,
    intercept = coef(fit_sub)[1],
    beta_distance = coef(fit_sub)[2],
    beta_passenger = coef(fit_sub)[3],
    se_distance = sd(boot_dist),
    ci_lower_distance = quantile(boot_dist, 0.025),
    ci_upper_distance = quantile(boot_dist, 0.975)
  ))
}

# 全数据模型
t0 <- Sys.time()
full_model <- lm(fare_amount ~ trip_distance + passenger_count, data = taxi_clean)
full_time <- as.numeric(Sys.time() - t0, units = "secs")

blb_summary <- data.frame(
  参数 = c("截距", "行程距离系数", "乘客数系数"),
  BLB估计 = c(mean(blb_results$intercept), mean(blb_results$beta_distance), mean(blb_results$beta_passenger)),
  BLB标准误 = c(sd(blb_results$intercept), sd(blb_results$beta_distance), sd(blb_results$beta_passenger)),
  全数据估计 = coef(full_model),
  全数据时间_秒 = round(full_time, 4)
)
blb_summary$相对偏差 <- round((blb_summary$BLB估计 - blb_summary$全数据估计) / blb_summary$全数据估计 * 100, 4)

message("BLB 与全数据回归系数对比:")
print(blb_summary)

# 图4：行程距离系数的 BLB 估计
p4 <- ggplot(blb_results, aes(x = factor(subsample_id), y = beta_distance)) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_errorbar(aes(ymin = ci_lower_distance, ymax = ci_upper_distance),
                width = 0.3, alpha = 0.6) +
  geom_hline(yintercept = coef(full_model)[2], color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "行程距离系数的 BLB 估计",
    subtitle = sprintf("红线为全数据估计值 %.3f，BLB 均值 %.3f",
                       coef(full_model)[2], mean(blb_results$beta_distance)),
    x = "子样本编号", y = "系数估计 (美元/英里)"
  ) +
  theme_cn(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
plot_list[["图4_BLB行程距离系数"]] <- p4

# 图5：BLB 三个系数的分布小提琴图
blb_coef_long <- data.frame(
  参数 = rep(c("截距", "行程距离", "乘客数"), each = nrow(blb_results)),
  估计值 = c(blb_results$intercept, blb_results$beta_distance, blb_results$beta_passenger)
)

true_vals <- data.frame(
  参数 = c("截距", "行程距离", "乘客数"),
  真实值 = coef(full_model)
)

p5 <- ggplot(blb_coef_long, aes(x = 参数, y = 估计值, fill = 参数)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.8) +
  geom_point(data = true_vals, aes(y = 真实值), color = "red", size = 4, shape = 18, inherit.aes = TRUE) +
  labs(title = "BLB 回归系数的分布", subtitle = "红色菱形为全数据估计值", x = "参数", y = "估计值") +
  theme_cn(base_size = 12) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
plot_list[["图5_BLB系数分布"]] <- p5

# ----------------------------------------------------------------------------
# 实验4：大数据回归计算时间对比（全量 vs 子样本 vs 分布式模拟）
# ----------------------------------------------------------------------------
message("\n=== 实验4：大数据回归计算时间对比 ===")

# 全量 OLS
t0 <- Sys.time()
fit_all <- lm(fare_amount ~ trip_distance + passenger_count + tip_amount, data = taxi_clean)
time_all <- as.numeric(Sys.time() - t0, units = "secs")

# 子样本 OLS（1% 样本）
sample_1pct <- taxi_clean |>
  slice_sample(n = round(nrow(taxi_clean) * 0.01))
t0 <- Sys.time()
fit_1pct <- lm(fare_amount ~ trip_distance + passenger_count + tip_amount, data = sample_1pct)
time_1pct <- as.numeric(Sys.time() - t0, units = "secs")

# 模拟分布式：分 10 个子集求平均
t0 <- Sys.time()
K <- 10
idx <- sample(rep(1:K, length.out = nrow(taxi_clean)))
local_coefs <- matrix(0, K, 4)
for (k in 1:K) {
  ik <- which(idx == k)
  local_coefs[k, ] <- coef(lm(fare_amount ~ trip_distance + passenger_count + tip_amount,
                              data = taxi_clean[ik, ]))
}
sae_coef <- colMeans(local_coefs)
time_dist <- as.numeric(Sys.time() - t0, units = "secs")

time_compare <- data.frame(
  方法 = c("全量OLS", "1%子样本OLS", "模拟分布式SAE"),
  计算时间_秒 = round(c(time_all, time_1pct, time_dist), 4),
  样本量 = c(nrow(taxi_clean), nrow(sample_1pct), nrow(taxi_clean))
)

message("计算时间对比:")
print(time_compare)

# 图6：计算时间对比柱状图
p6 <- ggplot(time_compare, aes(x = 方法, y = 计算时间_秒, fill = 方法)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.3f s", 计算时间_秒)), vjust = -0.5, size = 4, family = font_family) +
  labs(title = "大数据回归计算时间对比", x = "方法", y = "计算时间（秒）") +
  theme_cn(base_size = 12) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
plot_list[["图6_大数据计算时间对比"]] <- p6

# 导出 PPT
save_to_ppt(plot_list, file.path(output_dir, "empirical_figures.pptx"))

# ----------------------------------------------------------------------------
# 表格输出
# ----------------------------------------------------------------------------
message("\n=== 生成表格输出 ===")

write_csv_bom(subsample_summary, file.path(output_dir, "emp_table1_subsample_summary.csv"))
write_csv_bom(strat_summary, file.path(output_dir, "emp_table2_stratified_summary.csv"))
write_csv_bom(blb_summary, file.path(output_dir, "emp_table3_blb_regression.csv"))
write_csv_bom(time_compare, file.path(output_dir, "emp_table4_time_comparison.csv"))

# 详细子抽样结果（仅保存车费）
write_csv_bom(subsample_results[["fare_amount"]], file.path(output_dir, "emp_table5_fare_subsample_detail.csv"))

message("\n=== 实证分析全部完成，结果保存在 output/ 目录 ===")
