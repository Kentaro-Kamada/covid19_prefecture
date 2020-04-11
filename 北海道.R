library(tidyverse)
library(rvest)
library(lubridate)


# 北海道のデータをスクレイピング ---------------------------------------------------------

scrape_res <- read_html('http://www.pref.hokkaido.lg.jp/hf/kth/kak/hasseijoukyou.htm')

data <- 
  scrape_res %>% 
  html_node(xpath = '//*[@id="rs_contents"]/span/table') %>% 
  html_table() %>% 
  as_tibble() %>% 
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
  mutate(年代 = case_when(年代 == '10歳未満' ~ 0,
                          年代 %in% c('高齢者', '非公表') ~ NA_real_,
                          TRUE ~ parse_number(年代))) %>% 
  # 性別のクリーニング
  mutate(性別 = na_if(性別, '非公表')) %>% 
  # 居住地のクリーニング
  mutate(居住地 =  gsub(x = 居住地, pattern = '(管内|\\(.+\\)|\\s.+死亡|\\s*\\r\\n\\s*)', replacement = '') %>% 
              sub(x = ., pattern  = '総合総合', replacement = '総合')) %>% 
  # 型変換
  mutate(ID = parse_double(ID),
         公表日 = parse_date(公表日)) %>% 
  arrange(desc(ID))
  
# 保存
write_excel_csv(x = data, path = 'data/covid19_Hokkaido.csv')
  

# 分析 ----------------------------------------------------------------------

df <- read_csv(file = 'data/covid19_Hokkaido.csv', col_types = cols(年代 = col_character()))

# 感染者数の推移
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


# 基礎分析

df %>% 
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()

df %>%
  count(居住地) %>% 
  filter(n >= 6) %>% 
  ggplot(aes(x = 居住地, y = n))+
  geom_bar(stat = 'identity')
