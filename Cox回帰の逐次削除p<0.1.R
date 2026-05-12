library(survival)
library(dplyr)
library(stringr) # 文字列処理（不要かもしれないが安全のため）

# ----------------------------------------------------------------------
# 1. 前提条件と初期設定（前回の成功したステップをベースに再定義）
# ----------------------------------------------------------------------

# 最終的に使用する予測変数の定義 (17項目)
Final_vars_predictors <- c(
  "river", "direction", "composition", "curvature", "skewangle", "lanenumbers", 
  "decklength", "pavementthickness", "antifreeze", "aggregate", "crosssectionrepair", 
  "deckstrengthening", "solarradiation", "summertemprature", "wintertemprature", 
  "previouscracking", "previousspalling"
)

# TimeとEvent変数を追加（前のステップでデータに含まれていると仮定）
Survival_vars <- c(Final_vars_predictors, "lifespan", "event")

# 最終分析用データフレームの抽出
# ※ data_processed は前処理済みのデータフレームとします
final_data_survival <- data_processed %>%
  select(all_of(Survival_vars)) %>%
  select_if(is.numeric) # 数値型のみ維持

# 生存時間オブジェクトの作成
surv_object <- Surv(time = final_data_survival$lifespan, 
                    event = final_data_survival$event)

# 初期変数リスト
current_predictors <- Final_vars_predictors
significance_level <- 0.1 # 削除を停止するp値の閾値

cat("=================================================================\n")
cat("          COX回帰モデルの逐次後退的削除（Backward Elimination）\n")
cat("=================================================================\n")


# ----------------------------------------------------------------------
# 2. 逐次削除の繰返し計算（メインループ）
# ----------------------------------------------------------------------

iteration <- 1
max_p_value <- 1.0

while (max_p_value > significance_level && length(current_predictors) > 1) {
  
  # a. モデル式の構築
  formula_predictors <- paste(current_predictors, collapse = " + ")
  cox_formula <- as.formula(paste("surv_object ~", formula_predictors))
  
  # b. COX回帰の実行
  cox_model <- coxph(cox_formula, data = final_data_survival)
  
  # c. p値の抽出
  p_values_all <- summary(cox_model)$coefficients[, "Pr(>|z|)"]
  
  # d. 有意でない変数（p > 0.05）に限定
  insignificant_p_values <- p_values_all[p_values_all > significance_level]
  
  if (length(insignificant_p_values) == 0) {
    # 削除する変数がなければループ終了
    max_p_value <- 0.0 # 閾値以下に設定してループを抜ける
    break
  }
  
  # e. p値が最大の変数（最も有意でない変数）を特定
  max_p_value <- max(insignificant_p_values)
  variable_to_remove <- names(insignificant_p_values)[which.max(insignificant_p_values)]
  
  # f. 削除とリストの更新
  current_predictors <- setdiff(current_predictors, variable_to_remove)
  
  # g. 結果の出力
  cat(sprintf("--- 実行 #%d (%d 項目) ---\n", iteration, length(p_values_all)))
  cat(sprintf("   削除変数: %s\n", variable_to_remove))
  cat(sprintf("   p値: %.4f\n", max_p_value))
  cat(sprintf("   残り項目数: %d\n", length(current_predictors)))
  
  iteration <- iteration + 1
}


# ----------------------------------------------------------------------
# 3. 最終モデルの構築と結果表示
# ----------------------------------------------------------------------

# 最終モデルの実行
final_formula_predictors <- paste(current_predictors, collapse = " + ")
final_cox_formula <- as.formula(paste("surv_object ~", final_formula_predictors))
final_cox_model <- coxph(final_cox_formula, data = final_data_survival)

cat("\n=================================================================\n")
cat(sprintf("          最終的な有意な変数のみのCOXモデル (N = %d)          \n", length(current_predictors)))
cat("=================================================================\n")

# 最終モデルの要約
summary(final_cox_model)

cat("\n[最終的に残った変数]\n")
print(current_predictors)

# 最終モデルのフォレストプロット
cat("\n--- 最終モデルのハザード比フォレストプロット ---\n")
ggforest(final_cox_model, data = final_data_survival)