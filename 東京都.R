library(tidyverse)
library(lubridate)


# 東京都のデータを取得 --------------------------------------------------------------

data <- 
  read_csv('https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv') %>%  
  select(ID = 1, 公表日 = 5, 年代 = 9, 性別 = 10) %>% 
  # 年代のクリーニング
  mutate(年代 = case_when(年代 == '10歳未満' ~ '0',
                           grepl(x = 年代, '代') ~ sub(x = 年代, '代', ''),
                           TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(grepl(x = 性別, '性') ~ sub(x = 性別, '性', ''),
                          性別 == '男' ~ 性別,
                        TRUE ~ NA_character_)) 
  
# 保存
write_excel_csv(data, 'data/covid19_Tokyo.csv')


# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_Tokyo.csv',col_types = cols(年代 = col_character()))

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
