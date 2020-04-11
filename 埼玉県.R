library(tidyverse)
library(lubridate)
library(pdftools)

# 埼玉県のデータをスクレイピング ---------------------------------------------------------

scrape_res <- 
  pdf_text('https://www.pref.saitama.lg.jp/a0701/covid19/documents/yousei0410.pdf')

data <-
  scrape_res %>% 
  str_flatten() %>% 
  str_split('\r\n') %>%
  as_tibble(.name_repair = 'unique') %>% 
  # 空白処理（データフレームにしたときの列を整える）
  mutate(...1 = sub(x = ...1, '^\\s+', '') %>% 
           sub(x = ., '\\s\\d\\sリンク', '')) %>% 
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
  mutate(年代 = case_when(年代 == '未就学児' ~ '0',
                          grepl(x = 年代, '代') ~ sub(x = 年代, '代', ''),
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男性', '女性') ~ sub(x = 性別, '性', ''),
                          TRUE ~ NA_character_)) %>% 
  # 居住地のクリーニング（市町村が判明しているもの以外は欠損値指定）
  mutate(居住地 = case_when(grepl(x = 居住地, '[市町]') ~ 居住地, TRUE ~NA_character_)) %>% 
  # 型変換
  mutate(ID = parse_double(ID),
         公表日 = parse_date(公表日)) 

# 保存
write_excel_csv(data, 'data/covid19_Saitama.csv')


# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_Saitama.csv', col_types = cols(年代 = col_character()))  

df %>% 
  group_by(公表日) %>% 
  summarise(一日の感染者数 = n()) %>% 
  mutate(累計感染者数 = cumsum(一日の感染者数))  %>% 
  arrange(desc(公表日)) %>% {
    print(.)
    ggplot(., aes(x = 公表日)) +
      geom_line(aes(y = 累計感染者数))+
      geom_point(aes(y = 累計感染者数))+
      geom_bar(aes(y = 一日の感染者数), stat = 'identity')
  }


df %>% 
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()

df %>%
  count(居住地, sort = TRUE) %>% print(n = Inf)
  filter(n >= 6) %>% 
  ggplot(aes(x = 居住地, y = n))+
  geom_bar(stat = 'identity')  
