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
## 基本的に市区町村単位でコーディングする
## 北海道は札幌市と振興局でコーディング
## 福岡県は市と郡を用いる 
## 神奈川県は基本的に市単位だが，保健福祉事務所の管轄を同市に統合しているケースもある。
### 例えば鎌倉保健福祉事務所の管轄は鎌倉市・逗子市・三浦市・葉山町だが，すべて鎌倉市として扱う。
## 都道府県外であることがわかる場合は「県外」とする
## 「調査中」や県内であることはわかるが市町村が判然としない場合は欠損値とする


# 各都道府県のデータ取得 -------------------------------------------------------------

# 北海道 ---------------------------------------------------------

data <-
  read_csv('https://www.harp.lg.jp/opendata/dataset/1369/resource/2828/patients.csv',
         locale = locale(encoding = 'CP932')) %>% 
  select(ID = 1, 公表日 = 2, 年代, 性別, 居住地) %>% 
  
  # 年代のクリーニング
  # とりあえず全角が含まれているので半角に
  mutate(年代 = stringi::stri_trans_nfkc(年代)) %>% 
  mutate(年代 = case_when(年代 %in% c('高齢者', '非公表') ~ NA_character_,
                          年代 == '100代' ~ '90代',
                          str_detect(年代, '(10歳|10未満)') ~ '0代',
                          TRUE ~ str_remove_all(年代, '歳'))) %>%
  
  # 性別のクリーニング
  mutate(性別 = case_when(性別 == '非公表' ~ NA_character_,
                          性別 == '男' ~ '男性',
                          TRUE ~ 性別)) %>% 
  
  # 居住地のクリーニング
  mutate(居住地 = case_when(居住地 == '非公表' ~ NA_character_,
                            str_detect(居住地, '(中国武漢市|[都府県]$)') ~ '県外',
                            str_detect(居住地, '管?内\\s*$') ~ str_remove(居住地, '管?内\\s*$'),
                            str_detect(居住地, '(夕張市|岩見沢市|美唄市|芦別市|赤平市|三笠市|滝川市|砂川市|歌志内市|深川市|南幌町|奈井江町|上砂川町|由仁町|長沼町|栗山町|月形町|浦臼町|新十津川町|妹背牛町|秩父別町|雨竜町|北竜町|沼田町)'
                                          ) ~ '空知総合振興局',
                            str_detect(居住地, '(旭川市|名寄市|富良野市|士別市|鷹栖町|東神楽町|当麻町|比布町|愛別町|上川町|東川町|美瑛町|上富良野町|中富良野町|南富良野町|和寒町|剣淵町|下川町|美深町|中川町|幌加内町|音威子府村|占冠村)'
                                          ) ~ '上川総合振興局',
                            str_detect(居住地, '(江別市|千歳市|恵庭市|石狩市|北広島市|当別町|新篠津村)'
                                          ) ~ '石狩振興局',
                            str_detect(居住地, '(小樽市|島牧村|寿都町|黒松内町|蘭越町|ニセコ町|真狩村|留寿都村|喜茂別町|京極町|倶知安町|共和町|岩内町|泊村|神恵内村|積丹町|古平町|仁木町|余市町|赤井川村)'
                                          ) ~ '後志総合振興局',
                            str_detect(居住地, '(釧路市|釧路町|厚岸町|浜中町|標茶町|弟子屈町|鶴居村|白糠町)'
                                          ) ~ '釧路総合振興局',
                            str_detect(居住地, '(室蘭市|苫小牧市|登別市|伊達市|豊浦町|壮瞥町|白老町|厚真町|洞爺湖町|安平町|むかわ町)'
                                          ) ~ '胆振総合振興局',
                            str_detect(居住地, '(日高町|平取町|新冠町|浦河町|様似町|えりも町|新ひだか町)'
                                          ) ~ '日高振興局',
                            str_detect(居住地, '(函館市|北斗市|松前町|福島町|知内町|木古内町|七飯町|鹿部町|森町|八雲町|長万部町)'
                                          ) ~ '渡島総合振興局',
                            str_detect(居住地, '(江差町|上ノ国町|厚沢部町|乙部町|奥尻町|今金町|せたな町)'
                                          ) ~ '檜山振興局',
                            str_detect(居住地, '(留萌市|増毛町|小平町|苫前町|羽幌町|初山別村|遠別町|天塩町)'
                                          ) ~ '留萌振興局',
                            str_detect(居住地, '(稚内市|猿払村|浜頓別町|中頓別町|枝幸町|豊富町|礼文町|利尻町|利尻富士町|幌延町)'
                                          ) ~ '宗谷総合振興局',
                            str_detect(居住地, '(北見市|網走市|紋別市|美幌町|津別町|斜里町|清里町|小清水町|訓子府町|置戸町|佐呂間町|遠軽町|湧別町|滝上町|興部町|西興部村|雄武町|大空町)'
                                          ) ~ 'オホーツク総合振興局',
                            str_detect(居住地, '(帯広市|音更町|士幌町|上士幌町|鹿追町|新得町|清水町|芽室町|中札内村|更別村|大樹町|広尾町|幕別町|池田町|豊頃町|本別町|足寄町|陸別町|浦幌町)'
                                          ) ~ '十勝総合振興局',
                            str_detect(居住地, '(根室市|別海町|中標津町|標津町|羅臼町)'
                                          ) ~ '根室振興局',
                            TRUE ~ 居住地)) %>% 
  
  # 公表日の変換
  mutate(公表日 = as_date(公表日)) %>% 
  # 都道府県の追加
  mutate(都道府県 = '北海道') %>% select(都道府県, everything())



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
  mutate(公表日 = str_c(str_conv('2020年', 'CP932'), 公表日) %>% 
              parse_date(format = '%Y年%m月%d日' %>% str_conv('CP932'))) %>% 
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
  filter(str_detect(`1`, '\\d+')) %>% 
  # リネーム＆いらない列を削る
  select(ID = 1, 公表日 = 3, 年代 = 4, 性別 = 5, 居住地 = 6) %>% 
  # 公表日のクリーニング
  mutate(公表日 = case_when(grepl(x = 公表日, '\\d*月\\d*日') ~ 公表日, TRUE ~ NA_character_) %>% 
              str_c(str_conv('2020年', 'CP932'), .) %>%
              parse_date(format = str_conv('%Y年%m月%d日', 'CP932'))) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 %in% c('未就学児', '10歳未満') ~ '0代',
                          年代 %in% c('90代以', '90代以上') ~ '80以上',
                          grepl(x = 年代, '代') ~ 年代,
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男性', '女性') ~ 性別,
                          性別 == '女児' ~ '女性',
                          TRUE ~ NA_character_)) %>% 
  # 居住地のクリーニング（市町村が判明しているもの以外は欠損値指定）
  mutate(居住地 = case_when(居住地 %in% c('埼玉県', '調査中', '川口市外') ~ NA_character_,
                            grepl(x = 居住地, '[市町]') ~ 居住地, 
                            TRUE ~ '県外')) %>% 
  # 型変換
  mutate(ID = parse_double(ID)) %>% 
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
  readxl::read_excel(path = 'data/covid19_Osaka.xlsx', range = 'A2:E3000') %>% 
  
  # 余分にとった行を削る
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  # リネーム
  rename(ID = 1, 公表日 = 2) %>% 
  # 公表日のクリーニング
  mutate(公表日 = as_date(公表日)) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '調査中' ~ NA_character_,
                          grepl(x = 年代, '.*就学児') ~ sub(x = 年代, '.*就学児', '0代'),
                          年代 == '100' ~ '90代',
                          TRUE ~ paste0(年代, '代'))) %>%  
  # 性別のクリーニング
  mutate(性別 = case_when(grepl(x = 性別, '^[男女]$') ~ paste0(性別, '性'),
                           TRUE ~ 性別)) %>% 
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
  mutate(年代 = case_when(grepl(x = 年代, pattern = '(非公表|[ー－])') ~ NA_character_,
                        年代 %in% c('90代～', '100歳以上') ~ '90代',
                        年代 %in% c('10歳未満', '10代未満') ~ '0代',
                        TRUE ~ 年代)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(grepl(x = 性別, pattern = '－') ~ NA_character_,
                          TRUE ~ 性別)) %>% 
  # 居住地のクリーニング
  mutate(居住地 = str_replace_all(居住地, 'ケ', 'ヶ')) %>% 
  mutate(居住地 = 
              case_when(str_detect(居住地, '(^神奈川県内?$|市外)') ~ NA_character_,
                        str_detect(居住地, '(東京都|国外|スペイン)') ~ '県外',
                        str_detect(居住地, '(横浜|横須賀|鎌倉|茅ヶ崎|厚木|小田原|川崎|相模原|藤沢|平塚)') ~ 
                          str_c(
                            str_extract(居住地, '(横浜|横須賀|鎌倉|茅ヶ崎|厚木|小田原|川崎|相模原|藤沢|平塚)'),
                            '市'),
                        TRUE ~ 居住地)
            ) %>% 
  # ID追加，名前を合わせる
  transmute(ID = row_number(),
            公表日 = 発表日, 年代, 性別, 居住地) %>% 
  # 県名の付与
  mutate(都道府県 = '神奈川県') %>% select(都道府県, everything()) 
  

# 保存
write_excel_csv(data, 'data/covid19_Kanagawa.csv')


# 兵庫県 ---------------------------------------------------------------------
scrape_res <- read_html('https://web.pref.hyogo.lg.jp/kk03/corona_kanjyajyokyo.html')

scrape_res %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  str_subset('xlsx$') %>% 
  str_c('https://web.pref.hyogo.lg.jp', .) %>% 
  download.file(url = .,
              destfile = 'data/covid19_Hyogo.xlsx', 
              mode = 'wb')
data <-
  readxl::read_excel('data/covid19_Hyogo.xlsx', range = 'B6:G2000') %>% 
    # 余分にとった行を削る
    filter(rowSums(is.na(.)) != ncol(.)) %>% 
    # リネーム
    select(ID = 1, 公表日 = 2, 年代 = 3, 性別 = 4, 居住地 = 6) %>% 
    # 公表日のクリーニング
    mutate(公表日 = as_date(公表日)) %>% 
    # 年代のクリーニング
    mutate(年代 = case_when(年代 == '非公表' ~ NA_character_,
                            年代 %in% c('10歳未満', '10代未満', '1歳未満') ~ '0代',
                            TRUE ~ str_c(年代, str_conv('代', 'CP932')))) %>% 
    # 居住地のクリーニング
    mutate(居住地 = 
                case_when(grepl(x = 居住地, '(調査中|市外|大阪府)') ~ NA_character_,
                          grepl(x = 居住地, '健康福祉事務所管内') ~ sub(x = 居住地, '健康福祉事務所管内', '市'),
                          grepl(x = 居住地, '[市町]') ~ 居住地,
                          TRUE ~ '県外')) %>% 
    # 県名の付与
    mutate(都道府県 = '兵庫県') %>% select(都道府県, everything())
    
# 保存
write_excel_csv(data, 'data/covid19_Hyogo.csv')
  
    
# 福岡県 ---------------------------------------------------------------------
data <-
  read_csv('https://ckan.open-governmentdata.org/dataset/8a9688c2-7b9f-4347-ad6e-de3b339ef740/resource/c27769a2-8634-47aa-9714-7e21c4038dd4/download/400009_pref_fukuoka_covid19_patients.csv') %>% 
  # いらない行を落とす
  select(都道府県 = 3, ID = 1, 公表日 = 5, 年代 = 9, 性別 = 10, 居住地 = 8) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 == '調査中' ~ NA_character_,
                          TRUE ~ 性別)) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '調査中' ~ NA_character_,
                          年代 %in% c('10歳未満', '10代未満', '1歳未満') ~ '0代',
                          年代 == '100代' ~ '90代',
                          TRUE ~ 年代)) %>% 
  # 居住地のクリーニング
  mutate(居住地 = case_when(居住地 %in% c('確認中', '調査中', '北九州市外') ~ NA_character_,
                            居住地 %in% c('山口県下関市', '海外') ~ '県外',
                            居住地 == '八幡西区' ~ '北九州市',
                            grepl(x = 居住地, '福岡市') ~ '福岡市',
                            grepl(x = 居住地, '北九州市') ~ '北九州市',
                            TRUE ~ 居住地))   

# 保存
write_excel_csv(data, 'data/covid19_Fukuoka.csv')
  
# データのマージ -----------------------------------------------------------------

merge_data <-
  map_dfr(.x = 
            list.files('data', pattern = 'csv') %>%
            str_subset(., '(?<!merge).csv$'),
          .f = 
            ~{read_csv(file = str_c('data/', .),
                      col_types = cols(年代 = col_character()))})

# 保存
write_excel_csv(merge_data, 'data/covid19_merge.csv')


