library(tidyverse)
library(lubridate)



# 大阪府のデータをスクレイピング ---------------------------------------------------------


download.file(url = 'http://www.pref.osaka.lg.jp/attach/23711/00346644/youseisyajyouhou.xlsx',
              destfile = 'data/covid19_Osaka.xlsx', 
              mode = 'wb')

data <-
# rangeは長めに取っておく
  readxl::read_excel(path = 'data/covid19_Osaka.xlsx', range = 'A2:H1000') %>% 
  # 余分にとった行を削る
  drop_na() %>% 
  # いらない列を落とす
  select(ID = 1, 公表日 = 2, 年代, 性別, 居住地, 入退院の状況) %>% 
  mutate(公表日 = as_date(公表日),
            年代 = sub(x = 年代, pattern = '.*就学児', replacement = '0'),
            性別 = sub(x = 性別, pattern = '性', replacement = ''))

write_excel_csv(data, path = 'data/covid19_Osaka.csv')

# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_Osaka.csv', col_types = cols(公表日 = col_date(),
                                                             年代 = col_character()))

# 感染者数の推移（政府統計の形式）
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
  group_by(公表日) %>% 
  count(入退院の状況) %>% 
  pivot_wider(names_from = 入退院の状況, values_from = n) %>% 
  replace_na(replace = list(退院 = 0, 入院中 = 0, 死亡退院 = 0, 入院調整中 = 0)) %>%
  mutate(PCR陽性 = 退院 + 入院中 + 死亡退院 + 入院調整中) %>% 
  ungroup() %>% 
  # このままではすべて1日当たりなので累計する
  mutate_at(vars(-公表日), cumsum) %>% 
  arrange(desc(公表日))
  

# 基礎分析

df %>% 
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()

df %>%
  count(居住地) %>% 
  filter(n >= 6) %>% 
  ggplot(aes(x = 居住地, y = n))+
  geom_bar(stat = 'identity')
