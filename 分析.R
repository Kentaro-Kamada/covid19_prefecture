library(tidyverse)

df <-
  read_csv('data/covid19_merge.csv', locale = locale(encoding = 'UTF-8')) %>%
  mutate(年代 = case_when(年代 %in% c('0代', '10代') ~ '20未満',
                          年代 %in% c('80代', '90代') ~ '80以上',
                          TRUE ~ 年代) %>% 
             factor() %>% fct_relevel('20未満'))

# 全体 ----------------------------------------------------------------------

df %>%
  group_by(性別, 年代, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_line(size = 1)+
  scale_x_date(date_breaks = '3 week')+
  facet_wrap(~年代)

df %>%
  group_by(年代, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 年代 %>% fct_reorder(累計, .fun = max, .desc =T)))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '2 week')+
  labs(color = '年代')


# 都道府県別　-------------------------------------------------------------------

df %>%
  group_by(都道府県, 公表日) %>%
  summarise(一日の感染者数 = n()) %>%
  mutate(累計感染者数 = cumsum(一日の感染者数))  %>%
  arrange(都道府県, desc(公表日)) %>% {
    print(.)
    ggplot(., aes(x = 公表日)) +
      geom_line(aes(y = 累計感染者数))+
      geom_bar(aes(y = 一日の感染者数), stat = 'identity')+
      facet_wrap(~都道府県)
  }


df %>%
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()+
  facet_grid(~都道府県)+
  labs(x = '年代')

# 関東に絞る -------------------------------------------------------------------

Kanto <- 
  df %>% 
  filter(都道府県 %in% c('東京都', '埼玉県', '千葉県', '神奈川県'))


# 関東のみ時系列
Kanto %>% 
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

# 年代別感染者数
Kanto %>% 
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()

# 性別
Kanto %>% 
  group_by(性別, 年代, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_point()+
  geom_line()+
  facet_wrap(~年代)

# 年齢別
Kanto %>% 
  group_by(年代, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 年代))+
  geom_point()+
  geom_line()




