# 1. 解析対象の変数リスト（数値型のみに絞り込まれた後の列名を使用）
predictors <- colnames(final_data_survival)[!colnames(final_data_survival) %in% c("lifespan", "event")]

# 2. 各変数に対して単変量Cox回帰を実行
univariate_results <- lapply(predictors, function(var) {
  # 数式を作成 (例: Surv(lifespan, event) ~ direction)
  formula <- as.formula(paste("Surv(lifespan, event) ~", var))
  
  # Coxモデルの実行
  fit <- coxph(formula, data = final_data_survival)
  
  # 統計量の抽出
  summary_fit <- summary(fit)
  hr <- summary_fit$conf.int[1]       # Hazard Ratio
  lower <- summary_fit$conf.int[3]    # Lower 95% CI
  upper <- summary_fit$conf.int[4]    # Upper 95% CI
  p_val <- summary_fit$coefficients[5] # p-value
  
  # 結果をベクトルで返す
  return(data.frame(Variable = var, 
                    HR = round(hr, 3), 
                    Lower_CI = round(lower, 3), 
                    Upper_CI = round(upper, 3), 
                    p_value = round(p_val, 4)))
})

# 3. リストをデータフレームに変換して表示
univariate_table <- do.call(rbind, univariate_results)
print(univariate_table)

# 4. 有意な変数（p < 0.1）だけを確認する場合
significant_vars <- univariate_table %>% filter(p_value < 0.1)
print(significant_vars)