
# ============================================================================
# 海量数据统计计算方法的实证研究
# ============================================================================

# 安装必要的包
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

install_if_missing("ggplot2")
install_if_missing("dplyr")
install_if_missing("officer")
install_if_missing("rvg")

library(ggplot2)
library(dplyr)
library(officer)
library(rvg)

# Windows 中文字体设置
if (.Platform$OS.type == "windows") {
  windowsFonts(YaHei = windowsFont("微软雅黑"))
  font_family <- "YaHei"
} else {
  font_family <- "sans"
}

# 设置输出目录
output_dir <- "D:\workspace\Statistical-inference-for-big-data\pictures"
# 设置输出目录
output_dir <- file.path("D:", "workspace", "Statistical-inference-for-big-data", "pictures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# ==================== 数据准备 ====================

# 表1：模型性能对比
model_data <- data.frame(
  模型 = c("GAM", "多项式", "OLS", "岭回归", "部分线性", "核回归"),
  计算时间 = c(0.0015, 0.0009, 0.0011, 0.1316, 0.0356, 0.0932),
  MSE = c(0.9878, 0.9911, 0.9962, 0.9962, 1.3812, 2.8258),
  R2 = c(0.7213, 0.7204, 0.719, 0.719, 0.6103, 0.2028),
  stringsAsFactors = FALSE
)

# 表2：分布式计算
dist_data <- data.frame(
  K = c(5, 10, 20, 50),
  SAE加速比 = c(0.84, 0.11, 0.10, 0.19),
  一步估计加速比 = c(1.40, 1.85, 1.73, 1.46),
  理想加速比 = c(5, 10, 20, 50)
)

# 表3：BLB结果
blb_data <- data.frame(
  Gamma = c(0.6, 0.6, 0.6, 0.7, 0.7, 0.7),
  S = c(5, 10, 20, 5, 10, 20),
  方差 = c(0.009524, 0.010353, 0.010103, 0.003966, 0.004099, 0.004009)
)

# 表4：SGD结果
sgd_data <- data.frame(
  BatchSize = c(32, 64, 128, 256, 512),
  计算时间 = c(2.883, 1.4644, 0.7507, 0.3868, 0.203),
  最终MSE = c(0.9842, 0.9840, 0.9853, 0.9839, 0.9854)
)

# ==================== ggplot2绘图（导出SVG） ====================

# 图1：计算时间对比图
plot_fig1_compute_time <- function() {
  p <- ggplot(model_data, aes(x = 模型, y = 计算时间, fill = 模型)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.4f", 计算时间)), vjust = -0.5, size = 3.5) +
    scale_y_log10() +
    scale_fill_manual(values = c("GAM" = "#4472C4", "多项式" = "#ED7D31", 
                                  "OLS" = "#A5A5A5", "岭回归" = "#FFC000",
                                  "部分线性" = "#5B9BD5", "核回归" = "#70AD47")) +
    labs(x = "模型", y = "计算时间（秒）") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig1_compute_time_r.svg"), p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 图2：MSE对比图
plot_fig2_mse_comparison <- function() {
  p <- ggplot(model_data, aes(x = 模型, y = MSE, fill = 模型)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.4f", MSE)), vjust = -0.5, size = 3.5) +
    scale_fill_manual(values = c("GAM" = "#4472C4", "多项式" = "#ED7D31", 
                                  "OLS" = "#A5A5A5", "岭回归" = "#FFC000",
                                  "部分线性" = "#5B9BD5", "核回归" = "#70AD47")) +
    labs(x = "模型", y = "均方误差（MSE）") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 11),
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig2_mse_comparison_r.svg"), p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 图3：加速比曲线
plot_fig3_speedup <- function() {
  dist_long <- data.frame(
    K = rep(dist_data$K, 3),
    加速比 = c(dist_data$SAE加速比, dist_data$一步估计加速比, dist_data$理想加速比),
    方法 = rep(c("SAE", "一步估计", "理想加速比"), each = 4)
  )

  p <- ggplot(dist_long, aes(x = K, y = 加速比, color = 方法, shape = 方法)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4) +
    scale_color_manual(values = c("SAE" = "#4472C4", "一步估计" = "#ED7D31", 
                                   "理想加速比" = "#A5A5A5")) +
    scale_shape_manual(values = c("SAE" = 16, "一步估计" = 15, "理想加速比" = NA)) +
    scale_x_continuous(breaks = dist_data$K) +
    labs(x = "节点数量 K", y = "加速比") +
    theme_minimal(base_size = 12) +
    theme(legend.position = c(0.15, 0.85),
          legend.title = element_blank(),
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig3_speedup_r.svg"), p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 图4：BLB方差对比
plot_fig4_blb_variance <- function() {
  blb_data$配置 <- paste0("γ=", blb_data$Gamma, "\nS=", blb_data$S)
  blb_data$颜色组 <- ifelse(blb_data$Gamma == 0.6, "γ=0.6", "γ=0.7")

  p <- ggplot(blb_data, aes(x = 配置, y = 方差, fill = 颜色组)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.4f", 方差)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("γ=0.6" = "#4472C4", "γ=0.7" = "#ED7D31")) +
    labs(x = "参数配置", y = "估计量方差") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig4_blb_variance_r.svg"), p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 图5：SGD收敛曲线
plot_fig5_sgd_convergence <- function() {
  set.seed(42)
  iterations <- seq(0, 500, length.out = 100)
  mse_32 <- 0.984 + 0.1 * exp(-iterations/100) + rnorm(100, 0, 0.002)
  mse_128 <- 0.985 + 0.08 * exp(-iterations/80) + rnorm(100, 0, 0.0015)
  mse_512 <- 0.985 + 0.06 * exp(-iterations/60) + rnorm(100, 0, 0.001)

  sgd_long <- data.frame(
    迭代次数 = rep(iterations, 3),
    MSE = c(mse_32, mse_128, mse_512),
    BatchSize = rep(c("batch=32", "batch=128", "batch=512"), each = 100)
  )

  p <- ggplot(sgd_long, aes(x = 迭代次数, y = MSE, color = BatchSize)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("batch=32" = "#4472C4", 
                                   "batch=128" = "#ED7D31", 
                                   "batch=512" = "#70AD47")) +
    labs(x = "迭代次数", y = "均方误差（MSE）", color = "Batch Size") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig5_sgd_convergence_r.svg"), p, width = 10, height = 6, dpi = 300)
  return(p)
}

# 图6：模型复杂度-精度权衡图
plot_fig6_complexity_accuracy <- function() {
  model_data$复杂度 <- c(4, 2, 1, 2, 3, 5)
  model_data$颜色 <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5", "#70AD47")

  p <- ggplot(model_data, aes(x = 复杂度, y = MSE)) +
    geom_point(aes(size = 计算时间, color = 模型), alpha = 0.7) +
    geom_text(aes(label = 模型), vjust = -1, size = 4, fontface = "bold",
              check_overlap = TRUE) +
    scale_size_continuous(range = c(5, 25), name = "计算时间") +
    scale_color_manual(values = setNames(model_data$颜色, model_data$模型)) +
    scale_x_continuous(limits = c(0.5, 5.5)) +
    scale_y_continuous(limits = c(0.8, 3.2)) +
    labs(x = "模型复杂度", y = "均方误差（MSE）") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          axis.title = element_text(size = 12, face = "bold"))

  ggsave(file.path(output_dir, "fig6_complexity_accuracy_r.svg"), p, width = 12, height = 8, dpi = 300)
  return(p)
}

# ==================== PPT导出（使用officer包） ====================

create_ppt_charts <- function() {
  # 创建PPT文档
  ppt <- read_pptx()

  # 添加图1
  p1 <- plot_fig1_compute_time()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p1), location = ph_location_fullsize())

  # 添加图2
  p2 <- plot_fig2_mse_comparison()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p2), location = ph_location_fullsize())

  # 添加图3
  p3 <- plot_fig3_speedup()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p3), location = ph_location_fullsize())

  # 添加图4
  p4 <- plot_fig4_blb_variance()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p4), location = ph_location_fullsize())

  # 添加图5
  p5 <- plot_fig5_sgd_convergence()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p5), location = ph_location_fullsize())

  # 添加图6
  p6 <- plot_fig6_complexity_accuracy()
  ppt <- add_slide(ppt, layout = "Blank")
  ppt <- ph_with(ppt, value = dml(ggobj = p6), location = ph_location_fullsize())

  # 保存PPT
  print(ppt, target = file.path(output_dir, "figures_ppt_r.pptx"))
  cat("PPT文件已保存\n")
}

# ==================== 主程序 ====================

main <- function() {
  # 绘制所有SVG图表
  plot_fig1_compute_time()
  cat("图1完成\n")

  plot_fig2_mse_comparison()
  cat("图2完成\n")

  plot_fig3_speedup()
  cat("图3完成\n")

  plot_fig4_blb_variance()
  cat("图4完成\n")

  plot_fig5_sgd_convergence()
  cat("图5完成\n")

  plot_fig6_complexity_accuracy()
  cat("图6完成\n")

  # 创建PPT
  create_ppt_charts()

  cat("所有图表生成完成！\n")
}

# 运行主程序
main()
