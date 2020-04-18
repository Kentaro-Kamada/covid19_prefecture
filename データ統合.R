library(tidyverse)
library(lubridate)
library(rvest)
library(pdftools)

print_all <- function(x){
  print(x, n = Inf)
}

# コーディング方針 ----------------------------------------------------------------

# 公表日
## 患者と紐づけられた日付情報が1つの場合はそれを用いる
## 「公表日」「発症日」のように複数ある場合は「公表日」を用いる

# 性別
## 「男性」「女性」以外は欠損値とする

# 年代
## 10歳未満は「0代」とする
## 90歳以上の場合は「90代」カテゴリに含める
## 調査中等の場合は欠損値とする

# 居住地
## 基本的に市区町村単位でコーディングする（北海道のみ札幌市と振興局でコーディング）
## 都道府県外であることがわかる場合は「県外」とする
## 「調査中」や県内であることはわかるが市町村が判然としない場合は欠損値とする


# 各都道府県のデータ取得 -------------------------------------------------------------

# 北海道（修理中） ---------------------------------------------------------

scrape_res <- read_html('http://www.pref.hokkaido.lg.jp/hf/kth/kak/hasseijoukyou.htm', encoding = 'UTF-8')

# data <-
  scrape_res %>% 
  html_node(xpath = '//*[@id="rs_contents"]/span/table') %>% 
    html_table(fill = TRUE) %>% 
    
    
    
    
  html_table(fill = TRUE) %>% 
  as_tibble() %>% 
  # 全てNAの列を削る
  filter(rowSums(is.na(.)) != ncol(.)) %>% 
  # 全角を半角に
  mutate_all(stringi::stri_trans_nfkc) %>% 
  # 列名を埋める，ついでにいらない行を落とす
  select(ID = 1, 公表日 = 2, 年代 = 3, 性別 = 4, 居住地 = 5) %>% 
  # 1行目（もともとの列名）を落とす
  slice(-1) %>% 
  # 公表日のクリーニング
  separate(col = 公表日, into = c('月', '日'), sep = '/' ) %>% 
  mutate(年 = '2020',
          月 = str_pad(string = 月, width = 2, pad = '0', side = 'left'),
          日 = str_pad(string = 日, width = 2, pad = '0', side = 'left')) %>% 
  unite(col = 公表日, c(年, 月, 日), sep = '-') %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '10歳未満' ~ '0代',
                          grepl(x = 年代, '代') ~ 年代,
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男', '女') ~ paste0(性別, '性'),
                          TRUE ~ NA_character_)) %>% 
  # 居住地のクリーニング
  mutate(居住地 =  gsub(x = 居住地, 
                     pattern = '(管内|\\(.+\\)|\\s.+死亡|\\s*\\r\\n\\s*)', 
                     replacement = '') %>% 
              sub(x = ., pattern  = '総合総合', replacement = '総合')) %>% 
    mutate(居住地 = case_when(grepl(x = 居住地, '(札幌市|振興局)') ~ 居住地,
                           居住地 == '非公表' ~ NA_character_,
                           TRUE ~ '県外')) %>% 
  # 型変換
  mutate(ID = parse_double(ID),
         公表日 = parse_date(公表日)) %>% 
  # 県名を付与
  mutate(都道府県 = '北海道') %>% select(都道府県, everything()) %>% 
  arrange(desc(ID))

# 保存
write_excel_csv(x = data, path = 'data/covid19_Hokkaido.csv')


# 千葉県 --------------------------------------------------------------

scrape_res <-
  read_html('https://www.pref.chiba.lg.jp/shippei/press/2019/ncov-index.html', encoding = 'UTF-8') 

# データの検索
pdf_text <-
  tibble(
  text = scrape_res %>%
    html_nodes('a') %>% 
    html_text(),
  url = scrape_res %>% 
    html_nodes('a') %>% 
    html_attr('href')
  ) %>% 
  filter(grepl(x = text, '新型コロナウイルス感染症患者等の県内発生状況について')) %>% 
  pull(url) %>% 
  str_c('https://www.pref.chiba.lg.jp', .) %>% 
  pdf_text()

data <-
  pdf_text %>% 
  str_flatten() %>% 
  # 邪魔な改行を削除
  gsub(x = ., '（県内発生）\r\n', '') %>% 
  # 改行部分で分割
  str_split('\r\n') %>% 
  as_tibble(.name_repair = 'unique') %>% 
  # 5列目までを削る
  slice(c(-1:-5)) %>% 
  # 左側の空白削除
  mutate(...1 = str_trim(...1, 'left')) %>% 
  # 間の空白で列に分割
  separate(col = ...1, into = c(as.character(1:7)), sep = '\\s+') %>% 
  # NAが多い列を削除
  filter(rowSums(is.na(.)) <= ncol(.)-3) %>% 
  # NAがある行は5,6行目を右にずらす
  # 下準備としてNAがある行をマークする
  mutate(欠損有無 = is.na(`7`)) %>% 
  # 欠損有無がTRUEならばずらし，FALSEならばずらさない
  mutate(`7` = case_when(欠損有無 == TRUE ~ `6`,
                             欠損有無 == FALSE ~ `7`,
                             TRUE ~ NA_character_)) %>% 
  mutate(`6` = case_when(欠損有無 == TRUE ~ `5`, 
                             欠損有無 == FALSE ~ `6`,
                             TRUE ~ NA_character_))%>% 
  # まだNAがある行（無症状病原体保有者）はもう一つずらす
  mutate(`7` = case_when(is.na(`7`) ~ `6`, TRUE ~ `7`)) %>% 
  # いらない列を削る
  select(公表日 = 7, 年代 = 2, 性別 = 3, 居住地 = 4) %>% 
  # 公表日が'検査確定日'になってる行を削る
  filter(公表日 != '検査確定日') %>% 
  # 通しIDの付与
  transmute(ID = row_number(), 公表日, 年代, 性別, 居住地) %>% 
  # 公表日のクリーニング
  separate(col = 公表日, into = c('month', 'day'), sep = '\\p{Han}') %>% 
  mutate(year = 2020, 
         month = str_pad(month, width = 2, side = 'left', pad = '0'),
         day = str_pad(day, width = 2, side = 'left', pad = '0')) %>% 
  unite(col = '公表日', c(year, month, day), sep = '-') %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '10歳未満' ~ '0代',
                          年代 == '90代以上' ~ '90代',
                          grepl(x = 年代, '代$') ~ 年代,
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男性', '女性') ~ 性別,
                          TRUE ~ NA_character_)) %>% 
  # 居住地のクリーニング
　mutate(居住地 = case_when(grepl(x = 居住地, '(?<!武漢)[市町村]', perl = TRUE) ~ 居住地,
　                       TRUE ~ '県外')) %>% 
    # メモ：調査中とかも県外になっちゃう…
  # 型変換
  mutate(公表日 = parse_date(公表日)) %>% 
  # 県名の付与
  mutate(都道府県 = '千葉県') %>% select(都道府県, everything())

# 保存
write_excel_csv(data, 'data/covid19_Chiba.csv')


# 埼玉県 ---------------------------------------------------------

scrape_res <-
  read_html('https://www.pref.saitama.lg.jp/a0701/covid19/jokyo.html', encoding = 'UTF-8') %>%
  html_nodes(xpath = '//*[@id="tmp_contents"]/p[3]/a') %>% 
  html_attr('href') %>% 
  str_c('https://www.pref.saitama.lg.jp', .) %>% 
  pdf_text()

data <-
  scrape_res %>% 
  str_flatten() %>% 
  str_split('\r\n') %>% 
  as_tibble(.name_repair = 'unique') %>% 
  # 空白処理（データフレームにしたときの列を整える）
  mutate(...1 = sub(x = ...1, '^\\s+', '') %>% 
           sub(x = ., '\\sリンク先概要[12]', '')) %>% 
  separate(col = ...1, into = c(as.character(1:6)), sep = '\\s+') %>%
  # いらない行を削る
  slice(c(-1, -2, -nrow(.))) %>% 
  # リネーム＆いらない列を削る
  select(ID = 1, 公表日 = 3, 年代 = 4, 性別 = 5, 居住地 = 6) %>% 
  # 公表日のクリーニング
  separate(col = 公表日, into = c('month', 'day'), sep = '\\p{Han}') %>% 
  mutate(year = '2020',
         month = str_pad(month, width = 2, side = 'left', pad = '0'),
         day = str_pad(day, width = 2, side = 'left', pad = '0')) %>% 
  unite(col = 公表日, c(year, month, day), sep = '-') %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 %in% c('未就学児', '10歳未満') ~ '0代',
                          grepl(x = 年代, '代') ~ 年代,
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男性', '女性') ~ 性別,
                          性別 == '女児' ~ '女性',
                          TRUE ~ NA_character_)) %>% 
  # 居住地のクリーニング（市町村が判明しているもの以外は欠損値指定）
  mutate(居住地 = case_when(grepl(x = 居住地, '[市町]') ~ 居住地, 
                         居住地 %in% c('埼玉県', '調査中') ~ NA_character_,
                         TRUE ~ '県外')) %>% 
  # 型変換
  mutate(ID = parse_double(ID),
         公表日 = parse_date(公表日)) %>% 
  # 県名の付与
  mutate(都道府県 = '埼玉県') %>% select(都道府県, everything()) 
  

# 保存
write_excel_csv(data, 'data/covid19_Saitama.csv')


# 大阪府 ---------------------------------------------------------

download.file(url = 'http://www.pref.osaka.lg.jp/attach/23711/00346644/youseisyajyouhou.xlsx',
              destfile = 'data/covid19_Osaka.xlsx', 
              mode = 'wb')

data <-
  # rangeは長めに取っておく
  readxl::read_excel(path = 'data/covid19_Osaka.xlsx', range = 'A2:E2000') %>% 
  
  # 余分にとった行を削る
  drop_na() %>% 
  # リネーム
  rename(ID = 1, 公表日 = 2) %>% 
  # 公表日のクリーニング
  mutate(公表日 = as_date(公表日)) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(grepl(x = 年代, '.*就学児') ~ sub(x = 年代, '.*就学児', '0代'),
                        TRUE ~ paste0(年代, '代'))) %>%  
  # 性別のクリーニング
  # 今のところなし
  
  # 居住地のクリーニング
  mutate(居住地 = case_when(grepl(x = 居住地, '[市町村]') ~ 居住地,
                         grepl(x = 居住地, '府外') ~ '県外',
                         TRUE ~ NA_character_)) %>% 
  # 県名の付与
  mutate(都道府県 = '大阪府') %>% select(都道府県, everything())
  
# 保存
write_excel_csv(data, path = 'data/covid19_Osaka.csv')


# 東京都 --------------------------------------------------------------

data <-
  read_csv('https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv') %>%  
  select(ID = 1, 公表日 = 5, 年代 = 9, 性別 = 10) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '10歳未満' ~ '0代',
                          年代 == '100歳以上' ~ '90代',
                          grepl(x = 年代, '代$') ~ 年代,
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(grepl(x = 性別, '性') ~ 性別,
                        性別 == '男' ~ '男性',
                        TRUE ~ NA_character_)) %>% 
  # 県名の付与
  mutate(都道府県 = '東京都') %>% select(都道府県, everything()) 
  

# 保存
write_excel_csv(data, 'data/covid19_Tokyo.csv')


# 神奈川県 -------------------------------------------------------------

data <-
  read_csv(file = 'http://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv', 
           locale = locale(encoding = 'SHIFT-JIS')) %>%  
  # 年代のクリーニング
  mutate(年代 = case_when(grepl(x = 年代, pattern = '[ー－]') ~ NA_character_,
                        年代 == '10歳未満' ~ '0代',
                        TRUE ~ 年代)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(grepl(x = 性別, pattern = '－') ~ NA_character_,
                          TRUE ~ 性別)) %>%
  # 居住地のクリーニング
  mutate(居住地 = gsub(x = 居住地, '(（.+）|神奈川県|保健.+管内|内)', '')) %>%
  mutate(居住地 = case_when(grepl(x = 居住地, '(市外|^$)') ~ NA_character_,
                         grepl(x = 居住地, '茅.崎') ~ '茅ヶ崎市',
                         grepl(x = 居住地, '(スペイン|国外)') ~ '県外',
                         TRUE ~ 居住地)) %>% 
  # ID追加，名前を合わせる
  transmute(ID = row_number(),
            公表日 = 発表日, 年代, 性別, 居住地) %>% 
  # 県名の付与
  mutate(都道府県 = '神奈川県') %>% select(都道府県, everything()) 
  

# 保存
write_excel_csv(data, 'data/covid19_Kanagawa.csv')


# データのマージ -----------------------------------------------------------------

merge_data <-
  map_dfr(.x = list.files('data', pattern = 'csv') %>% 
            str_subset(., '(?<!merge).csv$'), 
      .f = ~{read_csv(file = str_c('data/', .),
                      col_types = cols(年代 = col_character()))})

# 保存
write_excel_csv(merge_data, 'data/covid19_merge.csv')


