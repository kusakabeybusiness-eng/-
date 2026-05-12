# 必要なライブラリの読み込み
library(readxl)
library(dplyr)
library(corrplot)
library(rstatix)
library(tidyr) 

# 1. データの読み込み
path <- "/Users/yukokusakabe/Downloads/Newdata.xlsx"
data <- read_excel(path, sheet=1)
# 1. 抽出したい変数リスト（スペルミスを修正済み）
# ※xlsx内の列名がこれと完全に一致している必要があります
target_vars <- c(
  "composition", "decklength", "aggregate", "maximumangle", "spanlength", 
  "deckthickness", "waterproofing", "precipitation", "curvature", 
  "heavytraffic", "deckrepair", "summertemprature", "skewangle", 
  "antifreeze", "recentleakage", "wintertemprature", "solarradiation",
  "lifespan", "event"
)

# 2. 必要な列だけを抽出して解析用データを作成
# 元のデータ 'data' から target_vars に含まれる列だけを取り出します
final_data_survival <- data %>%
  select(all_of(target_vars))

# 3. データの確認（19列表示されれば成功です）
print(colnames(final_data_survival))

# 1. 必要なライブラリ
library(survival)

# 2. 初期設定
current_predictors <- predictors
final_p_limit <- 0.1

repeat {
  # モデル構築
  formula_str <- paste("Surv(lifespan, event) ~", paste(current_predictors, collapse = " + "))
  fit <- coxph(as.formula(formula_str), data = final_data_survival)
  
  # summaryの係数テーブルからp値を取得
  coef_table <- as.data.frame(summary(fit)$coefficients)
  colnames(coef_table)[5] <- "p_value"
  
  # カテゴリ変数の場合、複数の行（変数名+レベル名）ができるため、
  # 各「元の変数名」ごとに最小のp値を代表値として評価します
  p_summary <- sapply(current_predictors, function(orig_var) {
    # 元の変数名を含む行をすべて抽出
    relevant_p <- coef_table$p_value[grepl(paste0("^", orig_var), rownames(coef_table))]
    if(length(relevant_p) == 0) return(NA)
    min(relevant_p) # カテゴリのうち1つでも有意なら残すため最小値をとる
  })
  
  # NAを除去して最大のp値（最も有意でない変数）を探す
  p_summary <- p_summary[!is.na(p_summary)]
  max_p <- max(p_summary)
  max_p_var <- names(which.max(p_summary))
  
  # 最大p値が閾値(0.1)を超えていれば削除、そうでなければ終了
  if (max_p > final_p_limit) {
    message(paste("削除対象:", max_p_var, "(p-value:", round(max_p, 4), ")"))
    current_predictors <- setdiff(current_predictors, max_p_var)
  } else {
    final_p_model <- fit
    break
  }
}

# 最終結果の表示
cat("\n--- 全ての変数が p < 0.1 となった最終モデル ---\n")
summary(final_p_model)

library(survminer)
library(survival)

# 1. 比較用の仮想データ（New Data）を作成
# 合成桁と非合成桁の2パターンを用意し、他の数値変数は平均値に固定します
avg_data <- final_data_survival %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# 比較用データセットの構築
new_df <- data.frame(
  composition = c("合成", "非合成"), # 実際のデータ内の表記に合わせてください
  decklength = avg_data$decklength,
  aggregate = avg_data$aggregate[1], # カテゴリの場合は代表値を設定
  spanlength = avg_data$spanlength,
  deckthickness = avg_data$deckthickness,
  precipitation = avg_data$precipitation,
  summertemprature = avg_data$summertemprature,
  skewangle = avg_data$skewangle,
  antifreeze = avg_data$antifreeze,
  wintertemprature = avg_data$wintertemprature,
  solarradiation = avg_data$solarradiation
)

# 2. 生存曲線の計算
fit_curves <- survfit(final_p_model, newdata = new_df, data = final_data_survival)

# 3. 作図
ggsurvplot(fit_curves, 
           data = new_df,
           conf.int = TRUE,                # 95%信頼区間を表示
           palette = c("#2E9FDF", "#E7B800"), # 青が合成、黄が非合成
           legend.labs = c("Composite (合成)", "Non-composite (非合成)"),
           xlab = "Years (Lifespan)", 
           ylab = "Survival Probability",
           main = "Survival Curves by Bridge Composition",
           ggtheme = theme_minimal())