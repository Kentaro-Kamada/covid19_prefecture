library(tidyverse)
library(lubridate)


# 神奈川県のデータを収集 -------------------------------------------------------------

data <-
  read_csv(file = 'http://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv', 
                locale = locale(encoding = 'SHIFT-JIS')) %>%  
  # 年代のクリーニング
  mutate(年代 = case_when(grepl(x = 年代, pattern = '[ー－]') ~ NA_character_,
                        年代 == '10歳未満' ~ '0',
                        TRUE ~ sub(x = 年代, pattern = '代', '')),
           # 性別のクリーニング
           性別 = case_when(grepl(x = 性別, pattern = '－') ~ NA_character_,
                          TRUE ~ sub(x = 性別, '性', ''))) %>% 
  # 居住地のクリーニング
  mutate(居住地 = gsub(x = 居住地, '(（.+）|神奈川県|保健.+管内|内)', '')) %>%
  mutate(居住地 = case_when(grepl(x = 居住地, '(スペイン|市外|国外|^$)') ~ NA_character_,
                         grepl(x = 居住地, '茅.崎') ~ '茅ヶ崎市',
                         TRUE ~ 居住地)) %>% 
  # ID追加，名前を合わせる
  transmute(ID = row_number(),
            公表日 = 発表日, 年代, 性別, 居住地)
  
# 保存
write_excel_csv(data, 'data/covid19_Kanagawa.csv')

# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_Kanagawa.csv', 
               col_types = cols(年代 = col_character()), 
               locale = locale(encoding = 'UTF-8'))

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
  count(居住地) %>% 
  filter(n >= 6) %>% 
  ggplot(aes(x = 居住地, y = n))+
  geom_bar(stat = 'identity')
