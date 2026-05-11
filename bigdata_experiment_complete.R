# ============================================================================
# 海量数据统计计算方法的实证研究：以房价预测为例
# ============================================================================
# 作者：hcm
# 日期：2026年
# 版本：1.0
# ============================================================================
# 
# 代码结构说明：
# 第一部分：包依赖和初始化
# 第二部分：数据生成模块（AR(1)协方差矩阵、房价数据生成）
# 第三部分：模型拟合模块（OLS、多项式回归、岭回归、GAM、核回归、部分线性模型）
# 第四部分：分布式计算模块（SAE、一步估计）
# 第五部分：BLB子抽样模块
# 第六部分：SGD小批次模块
# 第七部分：可视化模块
# 第八部分：表格输出模块
# 第九部分：主实验流程
# 第十部分：辅助函数和工具
# 第十一部分：快速测试函数
# ============================================================================

# =============================================================================
# 第一部分：包依赖和初始化
# =============================================================================

# 清除环境变量，确保实验环境干净
rm(list = ls())

# 设置随机种子以确保结果可重复
set.seed(42)

# 定义必要的R包列表
packages <- c(
  "MASS", "glmnet", "mgcv", "np", "ggplot2", "dplyr", "tidyr",
  "parallel", "doParallel", "foreach", "reshape2", "gridExtra",
  "scales", "viridis", "Rcpp", "microbenchmark",
  "officer", "rvg", "svglite", "showtext"
)

#' 检查并安装缺失的R包
#' @param pkg 包名
#' @return 无返回值，副作用是加载包
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# 静默安装和加载所有必要的包
invisible(sapply(packages, install_if_missing))

# 设置输出目录
output_dir <- "D:/workspace/Statistical-inference-for-big-data/output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 中文字体与主题设置（用于图表和表格中文显示）
if (.Platform$OS.type == "windows") {
  grDevices::windowsFonts(
    YaHei = grDevices::windowsFont("微软雅黑"),
    SimHei = grDevices::windowsFont("黑体"),
    SimSun = grDevices::windowsFont("宋体")
  )
}

plot_font_family <- if (.Platform$OS.type == "windows") "YaHei" else "sans"
showtext::showtext_auto(enable = TRUE)

theme_cn <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = plot_font_family) +
    theme(text = element_text(family = plot_font_family))
}

MODEL_LABELS_CN <- c(
  OLS = "普通最小二乘",
  Polynomial = "多项式回归",
  Ridge = "岭回归",
  GAM = "广义加性模型",
  Kernel = "核回归",
  Partial_Linear = "部分线性模型"
)
SPEEDUP_METHOD_LABELS_CN <- c(
  SAE = "SAE",
  "One-step" = "一步估计",
  Ideal = "理想加速比"
)

ensure_model_labels <- function(models) {
  missing <- setdiff(models, names(MODEL_LABELS_CN))
  if (length(missing) > 0) {
    stop("缺少模型中文映射: ", paste(missing, collapse = ", "), "。请在 MODEL_LABELS_CN 中添加对应映射。")
  }
}

# 统一保存图表：支持SVG（可编辑矢量）
save_plot_editable <- function(plot_obj, output_file, width = 10, height = 7, dpi = 300) {
  ext <- tolower(tools::file_ext(output_file))

  if (ext == "svg") {
    ggplot2::ggsave(
      filename = output_file,
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi,
      device = svglite::svglite
    )
  } else {
    ggplot2::ggsave(output_file, plot_obj, width = width, height = height, dpi = dpi)
  }

  cat(sprintf("图表已保存: %s\n", output_file))
}

# 将多张图导出为可编辑PPT（每图一页）
create_ppt_charts <- function(plot_list, output_file) {
  doc <- officer::read_pptx()

  for (nm in names(plot_list)) {
    doc <- officer::add_slide(doc, layout = "Blank", master = "Office Theme")
    doc <- officer::ph_with(
      doc,
      value = rvg::dml(ggobj = plot_list[[nm]]),
      location = officer::ph_location_fullsize()
    )
  }

  print(doc, target = output_file)
  cat(sprintf("可编辑PPT已保存: %s\n", output_file))
}

# 统一CSV写出（UTF-8-BOM，兼容Excel中文）
write_csv_cn <- function(data, output_file) {
  utils::write.csv(data, output_file, row.names = FALSE, fileEncoding = "UTF-8-BOM")
  cat(sprintf("表格已保存: %s\n", output_file))
}

cat("=== 环境初始化完成 ===\n")

# =============================================================================
# 第二部分：数据生成模块
# =============================================================================

#' 生成AR(1)相关结构的协方差矩阵
#' 
#' AR(1)模型：协方差矩阵元素 Sigma[i,j] = rho^|i-j|
#' 用于模拟特征之间的自相关结构
#' 
#' @param p 特征维度
#' @param rho 自相关系数，取值范围(0,1)，默认0.5
#' @return p×p协方差矩阵
#' @examples
#' Sigma <- generate_ar1_cov(8, 0.5)
generate_ar1_cov <- function(p, rho = 0.5) {
  Sigma <- matrix(0, p, p)
  for (i in 1:p) {
    for (j in 1:p) {
      Sigma[i, j] <- rho^abs(i - j)
    }
  }
  return(Sigma)
}

#' 生成房价预测合成数据（线性模型）
#' 
#' 生成具有AR(1)相关结构的特征数据，响应变量通过线性模型生成
#' y = Xβ + ε，其中 ε ~ N(0, σ²)
#' 
#' @param n 样本量
#' @param p 特征维度（不包括截距）
#' @param beta_true 真实系数向量（包括截距），默认为预设值
#' @param rho AR(1)相关系数，控制特征相关性
#' @param sigma 误差标准差
#' @return 包含X, y, X_design, beta_true, epsilon的列表
#' @examples
#' data <- generate_housing_data(1000, p = 8)
generate_housing_data <- function(n, p = 8, beta_true = NULL, 
                                   rho = 0.5, sigma = 1) {
  # 设置默认真实系数（第一个为截距）
  if (is.null(beta_true)) {
    beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)
  }

  # 生成AR(1)相关结构的特征
  Sigma <- generate_ar1_cov(p, rho)
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)

  # 特征命名（模拟房价相关特征）
  colnames(X) <- c("income", "age", "rooms", "distance", 
                   "crime_rate", "school_rating", "tax_rate", "employment")

  # 添加截距列
  X_design <- cbind(1, X)

  # 生成响应变量（房价）
  epsilon <- rnorm(n, 0, sigma)
  y <- X_design %*% beta_true + epsilon

  return(list(X = X, y = as.vector(y), X_design = X_design, 
              beta_true = beta_true, epsilon = epsilon))
}

#' 生成非线性房价数据（用于复杂模型测试）
#' 
#' 生成具有非线性关系的房价数据，用于测试GAM、核回归等非线性模型
#' 
#' @param n 样本量
#' @return 包含非线性关系的列表，包括各组件效应
#' @examples
#' data <- generate_nonlinear_housing_data(1000)
generate_nonlinear_housing_data <- function(n) {
  # 基础特征
  Sigma <- generate_ar1_cov(8, 0.5)
  X <- MASS::mvrnorm(n, mu = rep(0, 8), Sigma = Sigma)
  colnames(X) <- c("income", "age", "rooms", "distance", 
                   "crime_rate", "school_rating", "tax_rate", "employment")

  # 非线性关系：房价 = f(收入) + g(房龄) + 线性项 + 噪声
  # 收入效应：对数形式（边际效应递减）
  income_effect <- 5 * log(abs(X[, "income"]) + 2)

  # 房龄效应：二次函数（新房和旧房价值较低）
  age_effect <- 0.5 * X[, "age"]^2 - 2 * abs(X[, "age"])

  # 房间数效应：饱和效应
  rooms_effect <- 10 * (1 - exp(-0.5 * X[, "rooms"]))

  # 距离效应：负相关
  distance_effect <- -2 * X[, "distance"]

  # 其他线性效应
  other_effect <- 0.3 * X[, "crime_rate"] - 0.7 * X[, "school_rating"] + 
                  0.4 * X[, "tax_rate"] + 0.6 * X[, "employment"]

  # 基础价格
  base_price <- 50

  # 生成房价
  epsilon <- rnorm(n, 0, 2)
  y <- base_price + income_effect + age_effect + rooms_effect + 
       distance_effect + other_effect + epsilon

  return(list(X = X, y = y, true_components = list(
    income = income_effect,
    age = age_effect,
    rooms = rooms_effect,
    distance = distance_effect,
    other = other_effect
  )))
}

# =============================================================================
# 第三部分：模型拟合模块
# =============================================================================

#' 1. 线性回归（OLS）
#' 
#' 使用lm函数拟合普通最小二乘回归模型
#' 
#' @param X 设计矩阵（不含截距）
#' @param y 响应变量
#' @return 模型结果列表，包含系数、计算时间、MSE、R²等
#' @examples
#' result <- fit_ols(X, y)
fit_ols <- function(X, y) {
  start_time <- Sys.time()

  # 使用lm函数拟合OLS模型
  data <- data.frame(y = y, X)
  model <- lm(y ~ ., data = data)

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  # 预测和评估
  y_pred <- predict(model)
  mse <- mean((y - y_pred)^2)
  r2 <- summary(model)$r.squared
  adj_r2 <- summary(model)$adj.r.squared

  return(list(
    model = model,
    coefficients = coef(model),
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    adj_r2 = adj_r2,
    predictions = y_pred
  ))
}

#' 2. 多项式回归
#' 
#' 对指定变量使用正交多项式变换，捕捉非线性关系
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param degree 多项式次数，默认2
#' @param poly_vars 需要多项式变换的变量名
#' @return 模型结果列表
#' @examples
#' result <- fit_polynomial(X, y, degree = 2, poly_vars = c("income", "age"))
fit_polynomial <- function(X, y, degree = 2, 
                            poly_vars = c("income", "age")) {
  start_time <- Sys.time()

  # 构建数据框
  data <- data.frame(y = y, X)

  # 构建公式：对指定变量使用多项式
  all_vars <- colnames(X)
  linear_vars <- setdiff(all_vars, poly_vars)

  formula_str <- "y ~"

  # 添加多项式项
  for (i in seq_along(poly_vars)) {
    if (i == 1) {
      formula_str <- paste0(formula_str, " poly(", poly_vars[i], ", ", degree, ")")
    } else {
      formula_str <- paste0(formula_str, " + poly(", poly_vars[i], ", ", degree, ")")
    }
  }

  # 添加线性项
  if (length(linear_vars) > 0) {
    formula_str <- paste0(formula_str, " + ", paste(linear_vars, collapse = " + "))
  }

  model <- lm(as.formula(formula_str), data = data)

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  y_pred <- predict(model)
  mse <- mean((y - y_pred)^2)
  r2 <- summary(model)$r.squared

  return(list(
    model = model,
    coefficients = coef(model),
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    predictions = y_pred,
    formula = formula_str
  ))
}

#' 3. 岭回归（Ridge Regression）
#' 
#' 使用glmnet包进行L2正则化回归，通过交叉验证选择最优lambda
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param nfolds 交叉验证折数，默认10
#' @return 模型结果列表
#' @examples
#' result <- fit_ridge(X, y, nfolds = 10)
fit_ridge <- function(X, y, nfolds = 10) {
  start_time <- Sys.time()

  # 使用glmnet进行岭回归（alpha=0）
  cv_fit <- cv.glmnet(X, y, alpha = 0, nfolds = nfolds, 
                       standardize = TRUE, parallel = FALSE)

  # 使用最优lambda
  best_lambda <- cv_fit$lambda.min
  model <- glmnet(X, y, alpha = 0, lambda = best_lambda, standardize = TRUE)

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  # 预测
  y_pred <- predict(model, newx = X)
  mse <- mean((y - as.vector(y_pred))^2)

  # 计算伪R²
  ss_tot <- sum((y - mean(y))^2)
  ss_res <- sum((y - as.vector(y_pred))^2)
  r2 <- 1 - ss_res / ss_tot

  return(list(
    model = model,
    cv_model = cv_fit,
    coefficients = as.vector(coef(model)),
    lambda = best_lambda,
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    predictions = as.vector(y_pred)
  ))
}

#' 4. 广义可加模型（GAM）
#' 
#' 使用mgcv包拟合广义可加模型，对指定变量使用样条平滑
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param smooth_vars 需要平滑的变量名
#' @return 模型结果列表
#' @examples
#' result <- fit_gam(X, y, smooth_vars = c("income", "age", "rooms"))
fit_gam <- function(X, y, smooth_vars = c("income", "age", "rooms")) {
  start_time <- Sys.time()

  data <- data.frame(y = y, X)
  all_vars <- colnames(X)
  linear_vars <- setdiff(all_vars, smooth_vars)

  # 构建GAM公式
  formula_parts <- c()

  # 添加平滑项
  for (var in smooth_vars) {
    if (var %in% all_vars) {
      formula_parts <- c(formula_parts, paste0("s(", var, ")"))
    }
  }

  # 添加线性项
  for (var in linear_vars) {
    formula_parts <- c(formula_parts, var)
  }

  formula_str <- paste("y ~", paste(formula_parts, collapse = " + "))

  model <- mgcv::gam(as.formula(formula_str), data = data, method = "REML")

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  y_pred <- predict(model)
  mse <- mean((y - y_pred)^2)
  r2 <- summary(model)$r.sq

  return(list(
    model = model,
    coefficients = coef(model),
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    predictions = y_pred,
    formula = formula_str,
    smooth_terms = summary(model)$s.table
  ))
}

#' 5. 核回归（Nadaraya-Watson）
#' 
#' 使用np包进行局部线性核回归，自动选择最优带宽
#' 
#' @param x 单变量特征
#' @param y 响应变量
#' @param bw 带宽（NULL表示自动选择）
#' @return 模型结果列表
#' @examples
#' result <- fit_kernel(X[, "income"], y)
fit_kernel <- function(x, y, bw = NULL) {
  start_time <- Sys.time()

  # 使用np包进行核回归
  data <- data.frame(x = x, y = y)

  if (is.null(bw)) {
    # 自动选择带宽
    bw_obj <- np::npregbw(xdat = x, ydat = y, regtype = "ll")
  } else {
    bw_obj <- np::npregbw(xdat = x, ydat = y, bws = bw, regtype = "ll")
  }

  model <- np::npreg(bw_obj)

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  y_pred <- fitted(model)
  mse <- mean((y - y_pred)^2)
  r2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)

  return(list(
    model = model,
    bandwidth = model$bw,
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    predictions = y_pred
  ))
}

#' 6. 部分线性模型
#' 
#' 结合线性部分和非参数部分，使用GAM实现
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param linear_vars 线性部分变量
#' @param nonpar_var 非参数部分变量
#' @return 模型结果列表
#' @examples
#' result <- fit_partial_linear(X, y, c("rooms", "distance"), "income")
fit_partial_linear <- function(X, y, linear_vars, nonpar_var = "income") {
  start_time <- Sys.time()

  data <- data.frame(y = y, X)

  # 构建公式：线性部分 + 非参数部分
  linear_part <- paste(linear_vars, collapse = " + ")
  formula_str <- paste0("y ~ ", linear_part, " + s(", nonpar_var, ")")

  model <- mgcv::gam(as.formula(formula_str), data = data, method = "REML")

  end_time <- Sys.time()
  compute_time <- as.numeric(end_time - start_time, units = "secs")

  y_pred <- predict(model)
  mse <- mean((y - y_pred)^2)
  r2 <- summary(model)$r.sq

  return(list(
    model = model,
    coefficients = coef(model),
    compute_time = compute_time,
    mse = mse,
    r2 = r2,
    predictions = y_pred,
    formula = formula_str
  ))
}

# =============================================================================
# 第四部分：分布式计算模块
# =============================================================================

#' 简单平均估计量（SAE - Simple Averaging Estimator）
#' 
#' 将数据随机划分到K个节点，每个节点独立计算OLS估计，然后取平均
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param K 节点数量
#' @return 分布式估计结果列表
#' @examples
#' result <- distributed_sae(X, y, K = 10)
distributed_sae <- function(X, y, K = 10) {
  start_time <- Sys.time()

  n <- nrow(X)
  p <- ncol(X)

  # 随机划分数据到K个节点
  indices <- sample(rep(1:K, length.out = n))

  # 每个节点计算本地估计
  local_estimates <- matrix(0, nrow = K, ncol = p + 1)
  local_times <- numeric(K)

  for (k in 1:K) {
    node_start <- Sys.time()

    node_idx <- which(indices == k)
    X_k <- X[node_idx, , drop = FALSE]
    y_k <- y[node_idx]

    # 本地OLS估计
    data_k <- data.frame(y = y_k, X_k)
    local_model <- lm(y ~ ., data = data_k)
    local_estimates[k, ] <- coef(local_model)

    node_end <- Sys.time()
    local_times[k] <- as.numeric(node_end - node_start, units = "secs")
  }

  # 简单平均
  sae_estimate <- colMeans(local_estimates)

  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time, units = "secs")

  return(list(
    estimate = sae_estimate,
    local_estimates = local_estimates,
    local_times = local_times,
    total_time = total_time,
    K = K
  ))
}

#' 一步估计量（One-step Estimator）
#' 
#' 使用部分数据计算初始估计，然后通过分布式梯度更新一步
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param K 节点数量
#' @param pilot_n 初始估计使用的样本量
#' @return 分布式估计结果列表
#' @examples
#' result <- distributed_onestep(X, y, K = 10, pilot_n = 1000)
distributed_onestep <- function(X, y, K = 10, pilot_n = 1000) {
  start_time <- Sys.time()

  n <- nrow(X)
  p <- ncol(X)

  # 步骤1：使用部分数据计算初始估计
  pilot_idx <- sample(1:n, min(pilot_n, n))
  X_pilot <- X[pilot_idx, , drop = FALSE]
  y_pilot <- y[pilot_idx]

  data_pilot <- data.frame(y = y_pilot, X_pilot)
  pilot_model <- lm(y ~ ., data = data_pilot)
  beta_0 <- coef(pilot_model)

  # 步骤2：分布式计算梯度
  indices <- sample(rep(1:K, length.out = n))
  local_gradients <- matrix(0, nrow = K, ncol = p + 1)

  for (k in 1:K) {
    node_idx <- which(indices == k)
    X_k <- X[node_idx, , drop = FALSE]
    y_k <- y[node_idx]
    n_k <- length(node_idx)

    # 计算本地梯度
    X_design <- cbind(1, X_k)
    residual <- y_k - X_design %*% beta_0
    local_gradients[k, ] <- -2 * t(X_design) %*% residual / n_k
  }

  # 聚合梯度
  avg_gradient <- colMeans(local_gradients)

  # 步骤3：一步更新
  # 使用Fisher信息矩阵的估计
  X_design_full <- cbind(1, X)
  fisher_info <- 2 * t(X_design_full) %*% X_design_full / n

  # 一步估计
  beta_onestep <- beta_0 - solve(fisher_info) %*% avg_gradient

  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time, units = "secs")

  return(list(
    estimate = as.vector(beta_onestep),
    pilot_estimate = beta_0,
    gradient = avg_gradient,
    total_time = total_time,
    K = K
  ))
}

#' 比较不同K值的分布式计算性能
#' 
#' 对比SAE和One-step方法在不同节点数下的性能
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param K_values 节点数量向量
#' @return 性能比较结果列表
#' @examples
#' results <- compare_distributed(X, y, K_values = c(5, 10, 20, 50))
compare_distributed <- function(X, y, K_values = c(5, 10, 20, 50)) {
  results <- list()

  # 基准：完整数据OLS
  cat("计算基准OLS...\n")
  data_full <- data.frame(y = y, X)
  start_time <- Sys.time()
  full_model <- lm(y ~ ., data = data_full)
  full_time <- as.numeric(Sys.time() - start_time, units = "secs")
  full_coef <- coef(full_model)

  results$baseline <- list(
    estimate = full_coef,
    time = full_time
  )

  # 不同K值的SAE
  cat("计算SAE...\n")
  results$sae <- list()
  for (K in K_values) {
    cat(sprintf("  K=%d...\n", K))
    results$sae[[paste0("K", K)]] <- distributed_sae(X, y, K)
  }

  # 不同K值的One-step
  cat("计算One-step...\n")
  results$onestep <- list()
  for (K in K_values) {
    cat(sprintf("  K=%d...\n", K))
    results$onestep[[paste0("K", K)]] <- distributed_onestep(X, y, K)
  }

  results$K_values <- K_values
  return(results)
}

# =============================================================================
# 第五部分：BLB子抽样模块
# =============================================================================

#' Bag of Little Bootstrap (BLB) 实现
#' 
#' BLB方法：将数据划分为s个子集，每个子集进行B次bootstrap重采样
#' 适用于大规模数据的统计推断
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param s 子集数量
#' @param gamma 子集比例（m = n^gamma）
#' @param B 每子集重采样次数
#' @return BLB结果列表
#' @examples
#' result <- blb_regression(X, y, s = 10, gamma = 0.6, B = 100)
blb_regression <- function(X, y, s = 10, gamma = 0.6, B = 100) {
  start_time <- Sys.time()

  n <- nrow(X)
  p <- ncol(X)
  m <- floor(n^gamma)

  cat(sprintf("BLB参数: n=%d, s=%d, m=%d, gamma=%.2f, B=%d\n", 
              n, s, m, gamma, B))

  # 存储所有子集的估计
  all_estimates <- array(NA_real_, dim = c(s, B, p + 1))
  subset_means <- matrix(NA_real_, nrow = s, ncol = p + 1)
  subset_vars <- matrix(NA_real_, nrow = s, ncol = p + 1)

  # 固定系数顺序，避免奇异拟合时系数缺失/顺序变化
  coef_names <- c("(Intercept)", colnames(X))

  for (i in 1:s) {
    # 随机选择大小为m的子集（不放回）
    subset_idx <- sample(1:n, m, replace = FALSE)
    X_subset <- X[subset_idx, , drop = FALSE]
    y_subset <- y[subset_idx]

    # 在该子集上进行B次bootstrap重采样
    for (b in 1:B) {
      # 有放回重采样
      boot_idx <- sample(1:m, m, replace = TRUE)
      X_boot <- X_subset[boot_idx, , drop = FALSE]
      y_boot <- y_subset[boot_idx]

      # 拟合模型
      data_boot <- data.frame(y = y_boot, X_boot)
      tryCatch({
        model_boot <- lm(y ~ ., data = data_boot)
        coef_vec <- coef(model_boot)
        aligned_coef <- rep(NA_real_, p + 1)
        names(aligned_coef) <- coef_names
        matched <- intersect(names(coef_vec), coef_names)
        aligned_coef[matched] <- coef_vec[matched]
        all_estimates[i, b, ] <- aligned_coef
      }, error = function(e) {
        all_estimates[i, b, ] <- NA_real_
      })
    }

    # 计算该子集的均值和方差
    est_i <- all_estimates[i, , , drop = FALSE]
    est_i <- matrix(est_i, nrow = B, ncol = p + 1)

    subset_means[i, ] <- colMeans(est_i, na.rm = TRUE)
    subset_vars[i, ] <- apply(est_i, 2, var, na.rm = TRUE)
  }

  # 聚合所有子集的结果
  blb_estimate <- colMeans(subset_means, na.rm = TRUE)
  blb_se <- sqrt(colMeans(subset_vars, na.rm = TRUE))

  # 计算置信区间（95%）
  ci_lower <- blb_estimate - 1.96 * blb_se
  ci_upper <- blb_estimate + 1.96 * blb_se

  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time, units = "secs")

  return(list(
    estimate = blb_estimate,
    std_error = blb_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    subset_means = subset_means,
    subset_vars = subset_vars,
    all_estimates = all_estimates,
    parameters = list(s = s, gamma = gamma, B = B, m = m),
    compute_time = total_time
  ))
}

#' 比较不同BLB参数设置
#' 
#' 对比不同gamma和s参数组合下的BLB性能
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param gamma_values gamma参数向量
#' @param s_values s参数向量
#' @return 比较结果列表
#' @examples
#' results <- compare_blb(X, y, gamma_values = c(0.6, 0.7), s_values = c(5, 10, 20))
compare_blb <- function(X, y, gamma_values = c(0.6, 0.7), 
                         s_values = c(5, 10, 20)) {
  results <- list()

  # 基准：完整bootstrap
  cat("计算完整自助法基准...\n")
  n <- nrow(X)
  B_full <- 1000
  boot_estimates <- matrix(0, nrow = B_full, ncol = ncol(X) + 1)

  for (b in 1:B_full) {
    boot_idx <- sample(1:n, n, replace = TRUE)
    X_boot <- X[boot_idx, , drop = FALSE]
    y_boot <- y[boot_idx]
    data_boot <- data.frame(y = y_boot, X_boot)
    model_boot <- lm(y ~ ., data = data_boot)
    boot_estimates[b, ] <- coef(model_boot)
  }

  results$full_bootstrap <- list(
    estimate = colMeans(boot_estimates),
    std_error = apply(boot_estimates, 2, sd),
    all_estimates = boot_estimates
  )

  # 不同参数组合的BLB
  for (gamma in gamma_values) {
    for (s in s_values) {
      cat(sprintf("计算BLB: gamma=%.2f, s=%d...\n", gamma, s))
      key <- sprintf("gamma%.1f_s%d", gamma, s)
      results[[key]] <- blb_regression(X, y, s = s, gamma = gamma, B = 100)
    }
  }

  results$gamma_values <- gamma_values
  results$s_values <- s_values
  return(results)
}

# =============================================================================
# 第六部分：SGD小批次模块
# =============================================================================

#' 小批次随机梯度下降（SGD）用于线性回归
#' 
#' 使用小批次SGD优化线性回归参数，支持学习率衰减
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param batch_size 批次大小
#' @param epochs 训练轮数
#' @param learning_rate 初始学习率
#' @param decay 学习率衰减系数
#' @return SGD结果列表
#' @examples
#' result <- sgd_regression(X, y, batch_size = 32, epochs = 100)
sgd_regression <- function(X, y, batch_size = 32, epochs = 100, 
                            learning_rate = 0.01, decay = 0.99) {
  start_time <- Sys.time()

  n <- nrow(X)
  p <- ncol(X)

  # 初始化参数
  beta <- rnorm(p + 1, 0, 0.1)

  # 添加截距列
  X_design <- cbind(1, X)

  # 存储损失历史
  loss_history <- numeric(epochs * ceiling(n / batch_size))
  iter <- 1

  lr <- learning_rate

  for (epoch in 1:epochs) {
    # 随机打乱数据
    shuffle_idx <- sample(1:n)
    X_shuffled <- X_design[shuffle_idx, , drop = FALSE]
    y_shuffled <- y[shuffle_idx]

    # 小批次迭代
    num_batches <- ceiling(n / batch_size)

    for (b in 1:num_batches) {
      start_idx <- (b - 1) * batch_size + 1
      end_idx <- min(b * batch_size, n)

      X_batch <- X_shuffled[start_idx:end_idx, , drop = FALSE]
      y_batch <- y_shuffled[start_idx:end_idx]

      # 计算梯度
      residual <- y_batch - X_batch %*% beta
      gradient <- -2 * t(X_batch) %*% residual / length(y_batch)

      # 更新参数
      beta <- beta - lr * as.vector(gradient)

      # 记录损失
      if (iter <= length(loss_history)) {
        y_pred <- X_design %*% beta
        loss_history[iter] <- mean((y - y_pred)^2)
      }

      iter <- iter + 1
    }

    # 学习率衰减
    lr <- lr * decay
  }

  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time, units = "secs")

  # 最终预测
  y_pred <- X_design %*% beta
  mse <- mean((y - y_pred)^2)
  r2 <- 1 - sum((y - y_pred)^2) / sum((y - mean(y))^2)

  return(list(
    coefficients = beta,
    loss_history = loss_history[1:(iter-1)],
    compute_time = total_time,
    mse = mse,
    r2 = r2,
    predictions = as.vector(y_pred),
    parameters = list(
      batch_size = batch_size,
      epochs = epochs,
      learning_rate = learning_rate,
      decay = decay
    )
  ))
}

#' 比较不同batch size的SGD
#' 
#' 对比不同批次大小下的SGD收敛性能
#' 
#' @param X 特征矩阵
#' @param y 响应变量
#' @param batch_sizes 批次大小向量
#' @return 比较结果列表
#' @examples
#' results <- compare_sgd(X, y, batch_sizes = c(32, 64, 128, 256, 512))
compare_sgd <- function(X, y, batch_sizes = c(32, 64, 128, 256, 512)) {
  results <- list()

  for (bs in batch_sizes) {
    cat(sprintf("计算SGD: batch_size=%d...\n", bs))
    key <- paste0("bs", bs)
    results[[key]] <- sgd_regression(X, y, batch_size = bs, 
                                      epochs = 50, learning_rate = 0.01)
  }

  results$batch_sizes <- batch_sizes
  return(results)
}

# =============================================================================
# 第七部分：可视化模块
# =============================================================================

#' 绘制不同样本量下的计算时间对比图
#' 
#' 生成柱状图展示各模型在不同样本量下的计算时间
#' 
#' @param results_list 不同样本量的结果列表
#' @param output_file 输出文件路径（NULL表示不保存）
#' @return ggplot对象
#' @examples
#' p <- plot_compute_time(results_list, "fig1_compute_time.svg")
plot_compute_time <- function(results_list, output_file = NULL) {
  sample_sizes <- names(results_list)
  models <- c("OLS", "Polynomial", "Ridge", "GAM", "Kernel", "Partial_Linear")
  ensure_model_labels(models)

  plot_data <- data.frame()
  for (size in sample_sizes) {
    res <- results_list[[size]]
    for (model in models) {
      if (!is.null(res[[model]])) {
        plot_data <- rbind(
          plot_data,
          data.frame(
            SampleSize = as.numeric(size),
            Model = model,
            Time = res[[model]]$compute_time,
            MSE = res[[model]]$mse,
            R2 = res[[model]]$r2
          )
        )
      }
    }
  }

  plot_data$SampleSize <- factor(
    plot_data$SampleSize,
    levels = sort(unique(plot_data$SampleSize))
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = SampleSize, y = Time, fill = Model)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = "样本量",
      y = "计算时间（秒，对数坐标）",
      fill = "模型"
    ) +
    theme_cn(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0),
      legend.position = "bottom"
    ) +
    ggplot2::scale_fill_viridis_d(labels = MODEL_LABELS_CN)

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

#' 绘制模型MSE对比柱状图
#' 
#' 对比各模型的预测均方误差
#' 
#' @param results 模型结果
#' @param output_file 输出文件路径
#' @return ggplot对象
#' @examples
#' p <- plot_mse_comparison(results, "fig2_mse_comparison.svg")
plot_mse_comparison <- function(results, output_file = NULL) {
  models <- c("OLS", "Polynomial", "Ridge", "GAM", "Kernel", "Partial_Linear")

  plot_data <- data.frame()
  for (model in models) {
    if (!is.null(results[[model]])) {
      plot_data <- rbind(plot_data, data.frame(
        Model = model,
        MSE = results[[model]]$mse,
        R2 = results[[model]]$r2,
        Time = results[[model]]$compute_time
      ))
    }
  }

  # 按MSE排序
  ensure_model_labels(plot_data$Model)
  plot_data$ModelLabel <- MODEL_LABELS_CN[plot_data$Model]
  plot_data$ModelLabel <- factor(
    plot_data$ModelLabel,
    levels = MODEL_LABELS_CN[plot_data$Model[order(plot_data$MSE)]]
  )

  p <- ggplot(plot_data, aes(x = ModelLabel, y = MSE, fill = ModelLabel)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.4f", MSE)), vjust = -0.5, size = 3.5, family = plot_font_family) +
    labs(
      x = "模型",
      y = "均方误差（MSE）"
    ) +
    theme_cn(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    scale_fill_viridis_d()

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

#' 绘制分布式计算的加速比曲线
#' 
#' 展示SAE和One-step方法在不同节点数下的加速比
#' 
#' @param dist_results 分布式计算结果
#' @param output_file 输出文件路径
#' @return ggplot对象
#' @examples
#' p <- plot_speedup(dist_results, "fig3_speedup.svg")
plot_speedup <- function(dist_results, output_file = NULL) {
  K_values <- dist_results$K_values
  baseline_time <- dist_results$baseline$time

  plot_data <- data.frame()

  # SAE方法
  for (K in K_values) {
    key <- paste0("K", K)
    time <- dist_results$sae[[key]]$total_time
    plot_data <- rbind(plot_data, data.frame(
      K = K,
      Method = "SAE",
      Time = time,
      Speedup = baseline_time / time
    ))
  }

  # One-step方法
  for (K in K_values) {
    key <- paste0("K", K)
    time <- dist_results$onestep[[key]]$total_time
    plot_data <- rbind(plot_data, data.frame(
      K = K,
      Method = "One-step",
      Time = time,
      Speedup = baseline_time / time
    ))
  }

  # 理想加速比
  ideal_data <- data.frame(
    K = K_values,
    Method = "Ideal",
    Time = baseline_time / K_values,
    Speedup = K_values
  )
  plot_data <- rbind(plot_data, ideal_data)

  plot_data$MethodLabel <- SPEEDUP_METHOD_LABELS_CN[plot_data$Method]
  plot_data$MethodLabel <- factor(plot_data$MethodLabel, levels = SPEEDUP_METHOD_LABELS_CN[unique(plot_data$Method)])

  p <- ggplot(plot_data, aes(x = K, y = Speedup, color = MethodLabel, linetype = MethodLabel)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                color = "gray50", alpha = 0.7) +
    labs(
      x = "节点数量 K",
      y = "加速比",
      color = "方法",
      linetype = "方法"
    ) +
    theme_cn(base_size = 12) +
    theme(legend.position = "bottom") +
    scale_color_viridis_d() +
    scale_x_continuous(breaks = K_values)

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

#' 绘制BLB方法的置信区间覆盖率对比
#' 
#' 比较不同BLB参数设置下的置信区间覆盖率
#' 
#' @param blb_results BLB结果
#' @param true_coef 真实系数
#' @param output_file 输出文件路径
#' @return ggplot对象
#' @examples
#' p <- plot_blb_coverage(blb_results, beta_true, "fig4_blb_coverage.svg")
plot_blb_coverage <- function(blb_results, true_coef, output_file = NULL) {
  # 提取不同参数组合的结果
  gamma_values <- blb_results$gamma_values
  s_values <- blb_results$s_values

  plot_data <- data.frame()

  for (gamma in gamma_values) {
    for (s in s_values) {
      key <- sprintf("gamma%.1f_s%d", gamma, s)
      if (!is.null(blb_results[[key]])) {
        res <- blb_results[[key]]

        # 计算覆盖率
        coverage <- mean(true_coef >= res$ci_lower & true_coef <= res$ci_upper)

        # 计算平均CI宽度
        ci_width <- mean(res$ci_upper - res$ci_lower)

        plot_data <- rbind(plot_data, data.frame(
          Gamma = gamma,
          S = s,
          Coverage = coverage,
          CI_Width = ci_width,
          Method = sprintf("γ=%.1f, s=%d", gamma, s)
        ))
      }
    }
  }

  # 添加完整bootstrap结果
  full_cov <- mean(true_coef >= (blb_results$full_bootstrap$estimate - 1.96 * blb_results$full_bootstrap$std_error) & 
                   true_coef <= (blb_results$full_bootstrap$estimate + 1.96 * blb_results$full_bootstrap$std_error))
  plot_data <- rbind(plot_data, data.frame(
    Gamma = NA,
    S = NA,
    Coverage = full_cov,
    CI_Width = mean(2 * 1.96 * blb_results$full_bootstrap$std_error),
    Method = "完整自助法"
  ))

  p <- ggplot(plot_data, aes(x = Method, y = Coverage, fill = Method)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f%%", Coverage * 100)), vjust = -0.5, size = 3.5, family = plot_font_family) +
    labs(
      x = "方法",
      y = "覆盖率"
    ) +
    theme_cn(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    scale_fill_viridis_d() +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1))

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

#' 绘制不同batch size的SGD收敛曲线
#' 
#' 展示SGD在不同批次大小下的损失函数收敛过程
#' 
#' @param sgd_results SGD结果
#' @param output_file 输出文件路径
#' @return ggplot对象
#' @examples
#' p <- plot_sgd_convergence(sgd_results, "fig5_sgd_convergence.svg")
plot_sgd_convergence <- function(sgd_results, output_file = NULL) {
  batch_sizes <- sgd_results$batch_sizes

  plot_data <- data.frame()

  for (bs in batch_sizes) {
    key <- paste0("bs", bs)
    res <- sgd_results[[key]]

    # 采样损失历史以减少数据点
    loss_hist <- res$loss_history
    sample_idx <- round(seq(1, length(loss_hist), length.out = min(200, length(loss_hist))))

    plot_data <- rbind(plot_data, data.frame(
      Iteration = sample_idx,
      Loss = loss_hist[sample_idx],
      BatchSize = factor(bs, levels = batch_sizes)
    ))
  }

  p <- ggplot(plot_data, aes(x = Iteration, y = Loss, color = BatchSize)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(
      x = "迭代次数",
      y = "损失值（MSE，对数坐标）",
      color = "批大小"
    ) +
    theme_cn(base_size = 12) +
    theme(legend.position = "right") +
    scale_color_viridis_d() +
    scale_y_log10()

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

#' 绘制模型复杂度-精度权衡图
#' 
#' 气泡图展示模型复杂度、精度和计算时间的关系
#' 
#' @param results 模型结果
#' @param output_file 输出文件路径
#' @return ggplot对象
#' @examples
#' p <- plot_complexity_accuracy(results, "fig6_complexity_accuracy.svg")
plot_complexity_accuracy <- function(results, output_file = NULL) {
  models <- c("OLS", "Polynomial", "Ridge", "GAM", "Kernel", "Partial_Linear")
  ensure_model_labels(models)

  # 定义模型复杂度（主观评分，基于参数数量和计算复杂度）
  complexity <- c(
    OLS = 1,
    Polynomial = 2,
    Ridge = 2,
    GAM = 4,
    Kernel = 5,
    Partial_Linear = 3
  )

  plot_data <- data.frame()
  for (model in models) {
    if (!is.null(results[[model]])) {
      plot_data <- rbind(plot_data, data.frame(
        Model = model,
        Complexity = complexity[model],
        MSE = results[[model]]$mse,
        R2 = results[[model]]$r2,
        Time = results[[model]]$compute_time
      ))
    }
  }

  plot_data$ModelLabel <- MODEL_LABELS_CN[plot_data$Model]

  p <- ggplot(plot_data, aes(x = Complexity, y = MSE, size = Time, color = ModelLabel)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = ModelLabel), vjust = -1, size = 3.5, family = plot_font_family) +
    labs(
      x = "模型复杂度评分",
      y = "均方误差（MSE）",
      size = "计算时间（秒）",
      color = "模型"
    ) +
    theme_cn(base_size = 12) +
    theme(legend.position = "right") +
    scale_color_viridis_d() +
    scale_size_continuous(range = c(3, 15))

  if (!is.null(output_file)) {
    save_plot_editable(p, output_file, width = 10, height = 7, dpi = 300)
  }

  p
}

# 三类模型综合指标分析（参数/非参数/半参数）
analyze_three_model_families <- function(results, output_file = NULL) {
  family_map <- list(
    参数模型 = c("OLS", "Polynomial", "Ridge"),
    非参数模型 = c("Kernel"),
    半参数模型 = c("GAM", "Partial_Linear")
  )

  rows <- list()
  for (fam in names(family_map)) {
    candidates <- family_map[[fam]]
    candidates <- candidates[candidates %in% names(results)]

    if (length(candidates) == 0) {
      next
    }

    df <- do.call(rbind, lapply(candidates, function(md) {
      data.frame(
        模型 = md,
        计算时间_秒 = results[[md]]$compute_time,
        MSE = results[[md]]$mse,
        R2 = results[[md]]$r2,
        stringsAsFactors = FALSE
      )
    }))

    df$score <- scales::rescale(-df$MSE) * 0.5 +
      scales::rescale(df$R2) * 0.3 +
      scales::rescale(-df$计算时间_秒) * 0.2

    best <- df[which.max(df$score), ]
    best$模型类别 <- fam
    rows[[fam]] <- best
  }

  out <- do.call(rbind, rows)
  out <- out[, c("模型类别", "模型", "MSE", "R2", "计算时间_秒", "score")]
  names(out)[names(out) == "score"] <- "综合得分"

  if (!is.null(output_file)) {
    write_csv_cn(out, output_file)
  }

  out
}

generate_model_comparison_table <- function(results, output_file = NULL) {
  models <- c("OLS", "Polynomial", "Ridge", "GAM", "Kernel", "Partial_Linear")

  table_data <- data.frame()
  for (model in models) {
    if (!is.null(results[[model]])) {
      table_data <- rbind(table_data, data.frame(
        模型 = model,
        计算时间_秒 = round(results[[model]]$compute_time, 4),
        MSE = round(results[[model]]$mse, 6),
        R2 = round(results[[model]]$r2, 4),
        参数数量 = length(results[[model]]$coefficients)
      ))
    }
  }

  # 按MSE排序
  table_data <- table_data[order(table_data$MSE), ]

  if (!is.null(output_file)) {
    write_csv_cn(table_data, output_file)
  }

  table_data
}

generate_distributed_table <- function(dist_results, output_file = NULL) {
  K_values <- dist_results$K_values
  baseline_time <- dist_results$baseline$time

  table_data <- data.frame()

  for (K in K_values) {
    key <- paste0("K", K)
    sae_res <- dist_results$sae[[key]]
    onestep_res <- dist_results$onestep[[key]]

    table_data <- rbind(table_data, data.frame(
      K = K,
      SAE_时间_秒 = round(sae_res$total_time, 4),
      SAE_加速比 = round(baseline_time / sae_res$total_time, 2),
      OneStep_时间_秒 = round(onestep_res$total_time, 4),
      OneStep_加速比 = round(baseline_time / onestep_res$total_time, 2),
      理想加速比 = K
    ))
  }

  if (!is.null(output_file)) {
    write_csv_cn(table_data, output_file)
  }

  table_data
}

generate_blb_table <- function(blb_results, output_file = NULL) {
  gamma_values <- blb_results$gamma_values
  s_values <- blb_results$s_values

  table_data <- data.frame()

  for (gamma in gamma_values) {
    for (s in s_values) {
      key <- sprintf("gamma%.1f_s%d", gamma, s)
      if (!is.null(blb_results[[key]])) {
        res <- blb_results[[key]]
        table_data <- rbind(table_data, data.frame(
          Gamma = gamma,
          S = s,
          M = res$parameters$m,
          B = res$parameters$B,
          计算时间_秒 = round(res$compute_time, 4),
          估计量方差均值 = round(mean(res$std_error^2), 6)
        ))
      }
    }
  }

  # 添加完整bootstrap
  table_data <- rbind(data.frame(
    Gamma = NA,
    S = NA,
    M = nrow(blb_results$full_bootstrap$all_estimates),
    B = 1000,
    计算时间_秒 = "基准",
    估计量方差均值 = round(mean(blb_results$full_bootstrap$std_error^2), 6)
  ), table_data)

  if (!is.null(output_file)) {
    write_csv_cn(table_data, output_file)
  }

  table_data
}

generate_sgd_table <- function(sgd_results, output_file = NULL) {
  batch_sizes <- sgd_results$batch_sizes

  table_data <- data.frame()

  for (bs in batch_sizes) {
    key <- paste0("bs", bs)
    res <- sgd_results[[key]]
    table_data <- rbind(table_data, data.frame(
      BatchSize = bs,
      Epochs = res$parameters$epochs,
      初始学习率 = res$parameters$learning_rate,
      计算时间_秒 = round(res$compute_time, 4),
      最终MSE = round(res$mse, 6),
      R2 = round(res$r2, 4),
      收敛迭代次数 = length(res$loss_history)
    ))
  }

  if (!is.null(output_file)) {
    write_csv_cn(table_data, output_file)
  }

  table_data
}

# =============================================================================
# 第九部分：主实验流程
# =============================================================================

#' 运行完整实验
#' 
#' 执行所有实验模块：模型比较、分布式计算、BLB、SGD，并生成图表和表格
#' 
#' @param sample_sizes 样本量向量，默认c(1000, 10000, 100000)
#' @param output_dir 输出目录，默认"/mnt/okcomputer/output"
#' @return 完整实验结果列表
#' @examples
#' results <- run_full_experiment(c(1000, 10000, 100000), "/mnt/okcomputer/output")
run_full_experiment <- function(sample_sizes = c(1000, 10000, 100000),
                                output_dir = "D:/workspace/Statistical-inference-for-big-data/output") {

  cat("=================================================================\n")
  cat("海量数据统计计算方法的实证研究：以房价预测为例\n")
  cat("=================================================================\n\n")

  # 设置真实系数
  beta_true <- c(2.5, 0.8, -0.5, 1.2, 0.3, -0.7, 0.4, 0.6, -0.3)

  # 存储所有结果
  all_results <- list()

  # ========================================================================
  # 实验1：不同样本量的模型比较
  # ========================================================================
  cat("\n=================================================================\n")
  cat("实验1：不同样本量的模型比较\n")
  cat("=================================================================\n")

  for (n in sample_sizes) {
    cat(sprintf("\n--- 样本量 n = %d ---\n", n))

    # 生成数据
    data <- generate_housing_data(n, p = 8, beta_true = beta_true, 
                                   rho = 0.5, sigma = 1)
    X <- data$X
    y <- data$y

    results_n <- list()

    # 1. OLS
    cat("拟合OLS模型...\n")
    results_n$OLS <- fit_ols(X, y)
    cat(sprintf("  时间: %.4f秒, MSE: %.6f\n", 
                results_n$OLS$compute_time, results_n$OLS$mse))

    # 2. 多项式回归
    cat("拟合多项式回归...\n")
    results_n$Polynomial <- fit_polynomial(X, y, degree = 2, 
                                            poly_vars = c("income", "age"))
    cat(sprintf("  时间: %.4f秒, MSE: %.6f\n", 
                results_n$Polynomial$compute_time, results_n$Polynomial$mse))

    # 3. 岭回归
    cat("拟合岭回归...\n")
    results_n$Ridge <- fit_ridge(X, y, nfolds = 5)
    cat(sprintf("  时间: %.4f秒, MSE: %.6f, Lambda: %.4f\n", 
                results_n$Ridge$compute_time, results_n$Ridge$mse, 
                results_n$Ridge$lambda))

    # 4. GAM
    cat("拟合GAM...\n")
    results_n$GAM <- fit_gam(X, y, smooth_vars = c("income", "age", "rooms"))
    cat(sprintf("  时间: %.4f秒, MSE: %.6f\n", 
                results_n$GAM$compute_time, results_n$GAM$mse))

    # 5. 核回归（仅对income变量）
    cat("拟合核回归...\n")
    results_n$Kernel <- fit_kernel(X[, "income"], y)
    cat(sprintf("  时间: %.4f秒, MSE: %.6f, 带宽: %.4f\n", 
                results_n$Kernel$compute_time, results_n$Kernel$mse,
                results_n$Kernel$bandwidth))

    # 6. 部分线性模型
    cat("拟合部分线性模型...\n")
    linear_vars <- c("rooms", "distance", "crime_rate", "school_rating", "tax_rate", "employment")
    results_n$Partial_Linear <- fit_partial_linear(X, y, linear_vars, "income")
    cat(sprintf("  时间: %.4f秒, MSE: %.6f\n", 
                results_n$Partial_Linear$compute_time, results_n$Partial_Linear$mse))

    all_results[[as.character(n)]] <- results_n
  }

  # ========================================================================
  # 实验2：分布式计算（使用最大样本量）
  # ========================================================================
  cat("\n=================================================================\n")
  cat("实验2：分布式计算模拟\n")
  cat("=================================================================\n")

  n_large <- max(sample_sizes)
  cat(sprintf("使用样本量 n = %d\n", n_large))

  data_large <- generate_housing_data(n_large, p = 8, beta_true = beta_true)
  X_large <- data_large$X
  y_large <- data_large$y

  cat("运行分布式计算比较...\n")
  dist_results <- compare_distributed(X_large, y_large, 
                                       K_values = c(5, 10, 20, 50))
  all_results$distributed <- dist_results

  # ========================================================================
  # 实验3：BLB子抽样（使用中等样本量）
  # ========================================================================
  cat("\n=================================================================\n")
  cat("实验3：BLB子抽样方法\n")
  cat("=================================================================\n")

  n_medium <- median(sample_sizes)
  cat(sprintf("使用样本量 n = %d\n", n_medium))

  data_medium <- generate_housing_data(n_medium, p = 8, beta_true = beta_true)
  X_medium <- data_medium$X
  y_medium <- data_medium$y

  cat("运行BLB比较...\n")
  blb_results <- compare_blb(X_medium, y_medium, 
                              gamma_values = c(0.6, 0.7), 
                              s_values = c(5, 10, 20))
  all_results$blb <- blb_results

  # ========================================================================
  # 实验4：小批次SGD（使用中等样本量）
  # ========================================================================
  cat("\n=================================================================\n")
  cat("实验4：小批次SGD\n")
  cat("=================================================================\n")

  cat(sprintf("使用样本量 n = %d\n", n_medium))
  cat("运行SGD比较...\n")
  sgd_results <- compare_sgd(X_medium, y_medium, 
                              batch_sizes = c(32, 64, 128, 256, 512))
  all_results$sgd <- sgd_results

  # ========================================================================
  # 生成可视化图表
  # ========================================================================
  cat("\n=================================================================\n")
  cat("生成可视化图表\n")
  cat("=================================================================\n")

  # 图1：不同样本量的计算时间对比
  cat("生成图1：计算时间对比...\\n")
  fig1 <- plot_compute_time(
    all_results[as.character(sample_sizes)],
    file.path(output_dir, "图1_计算时间对比.svg")
  )

  # 图2：模型MSE对比（使用最大样本量）
  cat("生成图2：模型MSE对比...\\n")
  fig2 <- plot_mse_comparison(
    all_results[[as.character(n_large)]],
    file.path(output_dir, "图2_MSE对比.svg")
  )

  # 图3：分布式计算加速比
  cat("生成图3：加速比曲线...\\n")
  fig3 <- plot_speedup(
    dist_results,
    file.path(output_dir, "图3_加速比曲线.svg")
  )

  # 图4：BLB覆盖率
  cat("生成图4：BLB覆盖率...\\n")
  fig4 <- plot_blb_coverage(
    blb_results,
    beta_true,
    file.path(output_dir, "图4_BLB覆盖率.svg")
  )

  # 图5：SGD收敛曲线
  cat("生成图5：SGD收敛曲线...\\n")
  fig5 <- plot_sgd_convergence(
    sgd_results,
    file.path(output_dir, "图5_SGD收敛曲线.svg")
  )

  # 图6：复杂度-精度权衡
  cat("生成图6：复杂度-精度权衡...\\n")
  fig6 <- plot_complexity_accuracy(
    all_results[[as.character(n_large)]],
    file.path(output_dir, "图6_复杂度精度权衡.svg")
  )

  # 汇总导出可编辑PPT
  create_ppt_charts(
    plot_list = list(
      图1_计算时间对比 = fig1,
      图2_MSE对比 = fig2,
      图3_加速比 = fig3,
      图4_BLB覆盖率 = fig4,
      图5_SGD收敛 = fig5,
      图6_复杂度精度权衡 = fig6
    ),
    output_file = file.path(output_dir, "图表汇总.pptx")
  )

  # ========================================================================
  # 生成表格
  # ========================================================================
  cat("\n=================================================================\n")
  cat("生成表格输出\n")
  cat("=================================================================\n")

  # 表1：模型性能对比（各样本量）
  for (n in sample_sizes) {
    cat(sprintf("生成表1-%d：模型性能对比...\\n", n))
    generate_model_comparison_table(
      all_results[[as.character(n)]], 
      file.path(output_dir, sprintf("表1_模型性能对比_n%d.csv", n))
    )
  }

  # 表2：分布式计算结果
  cat("生成表2：分布式计算结果...\\n")
  generate_distributed_table(dist_results, 
                              file.path(output_dir, "表2_分布式计算结果.csv"))

  # 表3：BLB结果
  cat("生成表3：BLB结果...\\n")
  generate_blb_table(blb_results, 
                     file.path(output_dir, "表3_BLB结果.csv"))

  # 表4：SGD结果
  cat("生成表4：SGD结果...\\n")
  generate_sgd_table(sgd_results, 
                     file.path(output_dir, "表4_SGD结果.csv"))

  # 表5：模型选择建议
  cat("生成表5：模型选择建议...\\n")
  recommendation_table <- data.frame(
    场景 = c("快速原型", "高精度需求", "大数据集", "非线性关系", "可解释性优先"),
    推荐模型 = c("OLS", "GAM", "分布式OLS", "GAM/核回归", "OLS/多项式"),
    理由 = c("计算最快，易于实现",
             "能捕捉复杂非线性关系",
             "分布式计算可加速",
             "非参数方法更灵活",
             "线性模型易于解释"),
    注意事项 = c("假设线性关系",
                 "计算成本较高",
                 "需要多核环境",
                 "需要更多数据",
                 "可能欠拟合")
  )
  write_csv_cn(
    recommendation_table,
    file.path(output_dir, "表5_模型选择建议.csv")
  )

  # 三类模型综合分析表（参数/非参数/半参数）
  cat("生成表6：三类模型综合分析...\\n")
  analyze_three_model_families(
    all_results[[as.character(n_large)]],
    file.path(output_dir, "表6_三类模型综合分析.csv")
  )

  cat(sprintf("表格已保存: %s\\n", file.path(output_dir, "表5_模型选择建议.csv")))

  cat("\n=================================================================\n")
  cat("实验完成！所有结果已保存到: ", output_dir, "\n")
  cat("=================================================================\n")

  return(all_results)
}

# =============================================================================
# 第十部分：辅助函数和工具
# =============================================================================

#' 打印实验摘要
#' 
#' 格式化输出实验结果摘要
#' 
#' @param results 实验结果
#' @return 无返回值，直接打印到控制台
#' @examples
#' print_summary(results)
print_summary <- function(results) {
  cat("\n========== 实验结果摘要 ==========\n")

  # 样本量结果摘要
  sample_sizes <- names(results)[!names(results) %in% c("distributed", "blb", "sgd")]

  for (size in sample_sizes) {
    cat(sprintf("\n样本量 n = %s:\n", size))
    res <- results[[size]]

    models <- c("OLS", "Polynomial", "Ridge", "GAM", "Kernel", "Partial_Linear")
    for (model in models) {
      if (!is.null(res[[model]])) {
        cat(sprintf("  %s: MSE=%.6f, R²=%.4f, Time=%.4fs\n",
                    model, res[[model]]$mse, res[[model]]$r2, 
                    res[[model]]$compute_time))
      }
    }
  }

  # 分布式计算摘要
  if (!is.null(results$distributed)) {
    cat("\n分布式计算加速比:\n")
    dist_res <- results$distributed
    K_vals <- dist_res$K_values
    baseline_time <- dist_res$baseline$time

    for (K in K_vals) {
      key <- paste0("K", K)
      sae_speedup <- baseline_time / dist_res$sae[[key]]$total_time
      onestep_speedup <- baseline_time / dist_res$onestep[[key]]$total_time
      cat(sprintf("  K=%d: SAE加速比=%.2fx, One-step加速比=%.2fx\n",
                  K, sae_speedup, onestep_speedup))
    }
  }

  # SGD摘要
  if (!is.null(results$sgd)) {
    cat("\nSGD不同batch size的最终MSE:\n")
    sgd_res <- results$sgd
    for (bs in sgd_res$batch_sizes) {
      key <- paste0("bs", bs)
      cat(sprintf("  Batch Size %d: MSE=%.6f, Time=%.4fs\n",
                  bs, sgd_res[[key]]$mse, sgd_res[[key]]$compute_time))
    }
  }
}

#' 保存完整实验结果到RData文件
#' 
#' @param results 实验结果
#' @param output_file 输出文件
#' @return 无返回值
#' @examples
#' save_results(results, "experiment_results.RData")
save_results <- function(results, output_file = "experiment_results.RData") {
  save(results, file = output_file)
  cat(sprintf("结果已保存到: %s\n", output_file))
}

# =============================================================================
# 第十一部分：快速测试函数
# =============================================================================

#' 运行快速测试（小样本量）
#' 
#' 使用较小的样本量快速验证代码正确性
#' 
#' @param sample_sizes 样本量向量或单个样本量；默认 c(1000, 5000)
#' @return 实验结果列表
#' @examples
#' results <- run_quick_test()
#' results <- run_quick_test(1000)
run_quick_test <- function(sample_sizes = c(1000, 5000)) {
  cat("运行快速测试模式...\n")
  if (length(sample_sizes) == 1) {
    sample_sizes <- as.integer(sample_sizes)
  }
  results <- run_full_experiment(
    sample_sizes = sample_sizes,
    output_dir = "D:/workspace/Statistical-inference-for-big-data/output"
  )
  return(results)
}

#' 运行完整实验（全样本量）
#' 
#' 使用完整的样本量范围进行实验
#' 
 
#' @return 实验结果列表
#' @examples
#' results <- run_complete_experiment()
run_complete_experiment <- function() {
  cat("运行完整实验模式...\n")
  results <- run_full_experiment(
    sample_sizes = c(1000, 10000, 100000, 1000000),
    output_dir = "D:/workspace/Statistical-inference-for-big-data/output"
  )
  return(results)
}

# =============================================================================
# 主程序入口
# =============================================================================

if (interactive()) {
  cat("\n")
  cat("============================================================\n")
  cat("  海量数据统计计算方法实证研究 - R实验代码\n")
  cat("============================================================\n")
  cat("\n")
  cat("可用函数:\n")
  cat("  1. run_quick_test(sample_sizes = c(1000, 5000)) - 快速测试\n")
  cat("  2. run_complete_experiment() - 完整实验（样本量: 1K-1M）\n")
  cat("  3. run_full_experiment(sample_sizes, output_dir) - 自定义实验\n")
  cat("\n")
  cat("示例:\n")
  cat("  results <- run_quick_test()\n")
  cat("  results <- run_quick_test(1000)\n")
  cat("  print_summary(results)\n")
  cat("\n")
}

# ============================================================================
# 代码结束
# ============================================================================
