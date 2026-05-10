## Statistical-inference-for-big-data

本仓库包含“大数据统计推断”相关的模拟与实证脚本，覆盖分布式、子抽样与小批次方法。

### 主要脚本

- `bigdata_experiment_complete.R`
  综合实验主脚本（SAE/One-step、BLB、SGD、图表与表格）
- `simulation.R`（已废弃）
  数值模拟实验脚本（已废弃，请使用 `integrated_comparison_experiment.R`）
- `empirical_analysis.R`（已废弃）
  NYC Taxi 实证分析脚本（已废弃，请使用 `integrated_comparison_experiment.R`）
- `integrated_comparison_experiment.R`
  统一横向对比与综合使用实验脚本

### 新增统一对比实验说明

`integrated_comparison_experiment.R` 实现以下内容：

1. 同口径横向比较：
   - 分布式：SAE、One-step
   - 子抽样：SRS、Stratified、BLB
   - 小批次：Mini-batch SGD（不同 batch）
2. 两条主线：
   - Simulation（可控真值）
   - Empirical（NYC Taxi 真实数据）
3. 综合方案（Hybrid）：
   - BLB -> One-step
   - One-step -> SGD
   - Stratified-SGD -> Aggregate
4. 统一输出：
   - 精度-效率-稳定性总表
   - 参数敏感性表
   - 综合方案收益表
   - 预算视角总表（时间/内存/样本访问）
   - 帕累托图、覆盖率-区间宽度图、收敛曲线、雷达图
   - 分轨输出：同一 run_tag 下分别输出模拟/实证图（文件名为中文图名并追加轨道中文后缀，中文与数字保留，其他字符替换为下划线；若仅含特殊字符则回退为 `未命名轨道_<length>_<codepoint1>_<codepoint2>...`）
   - 运行日志

### 运行方式

在 R 环境中执行：

```r
source("integrated_comparison_experiment.R")
run_integrated_comparison(mode = "quick")
```

完整模式：

```r
run_integrated_comparison(mode = "full")
```

默认输出目录：

`output/unified_comparison`
