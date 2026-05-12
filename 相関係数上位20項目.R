# 必要なライブラリの読み込み
# corrplotとrstatixが未インストールの場合は、install.packages()を実行してください
library(readxl)
library(dplyr)
library(corrplot)
library(rstatix)
library(tidyr) # pivot_longer関数が含まれます

# 1. データの読み込み
path <- "/Users/yukokusakabe/Downloads/Newdata.xlsx"
data <- read_excel(path, sheet=1)

# 2. 変数グループの定義
Group1_vars <- c("river", "direction", "superstructure", "composition", "spanlength", "curvature",
                 "skewangle", "longitudinalgrade", "transversegrade", "transversetypes", "lanenumbers",
                 "lanewidth", "shoulderlength", "decklength", "deckthickness", "pavementthickness",
                 "traffic", "heavytraffic", "antifreeze", "aggregate", "waterproofing", "crosssectionrepair",
                 "deckrepair", "deckstrengthening", "averagetemprature", "sunshinehours", "solarradiation",
                 "elevation", "maximumangle", "precipitation", "summertemprature", "wintertemprature")

Group2_vars <- c("recentspalling", "recentcracking", "recentleakage",
                 "previouscracking", "previousleakage", "previousspalling")

# Group2_vars (被害状況) を数値に変換するための変数リストを定義
damage_vars <- Group2_vars # ★エラー解消のため Group2_vars を定義に使用★

# 3. カテゴリ変数の数値化 (前処理)
data <- data %>%
  # 被害状況の変換 (A=1, B=2, C=3)
  mutate(across(all_of(damage_vars), ~case_when(
    .x == "C" ~ 3,
    .x == "B" ~ 2,
    .x == "A" ~ 1,
    TRUE ~ NA_real_ # それ以外の値は欠損値
  ))) %>%
  # 構造形式の変換 (合成/非合成)
  mutate(composition = case_when(
    composition == "合成" ~ 1,
    composition == "非合成" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  # 桁形式の変換 (連続桁/単純桁)
  mutate(superstructure = case_when(
    superstructure == "連続桁" ~ 1,
    superstructure == "単純桁" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  # 横桁形式の変換 (拝/片)
  mutate(transversetypes = case_when(
    transversetypes == "拝" ~ 1,
    transversetypes == "片" ~ 0,
    TRUE ~ NA_real_
  ))

# 4. 相関分析に必要な変数の抽出
All_vars <- c(Group1_vars, Group2_vars)
cor_data <- data %>%
  select(all_of(All_vars)) %>%
  # 数値に変換できなかった列（例: river, direction, aggregateなど）をここで除外する
  select_if(is.numeric)

# 5. 相関係数行列の計算
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# 6. 相関ヒートマップの作成
# corrplot関数には、Rのグラフィックウィンドウが必要です
cat("\n--- 相関ヒートマップ ---")
corrplot(cor_matrix, method = "color", type = "upper", # 上三角行列のみ表示
         tl.col = "black", tl.srt = 45, # ラベルの色と傾き
         addCoef.col = "black", # 相関係数の値をプロットに追加
         diag = FALSE)



# 7. 相関係数のランキング表示
cat("\n--- 相関係数の絶対値ランキング (上位20ペア) ---")
cor_results <- cor_matrix %>%
  as_tibble(rownames = "var1") %>% # 行名を var1 としてデータフレーム化
  pivot_longer(cols = -var1, names_to = "var2", values_to = "r") %>% # ワイド形式をロング形式に変換
  filter(var1 != var2) %>% # 自分自身との相関（r=1）を除外
  mutate(abs_r = abs(r)) %>% # 相関係数の絶対値を計算
  arrange(desc(abs_r)) # 絶対値の高い順にソート

# 結果の表示
print(head(cor_results, 20))
