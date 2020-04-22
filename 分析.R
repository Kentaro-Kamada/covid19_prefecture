library(tidyverse)

# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_merge.csv')


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
  group_by(性別, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_point()+
  geom_line()

# 年齢別
Kanto %>% 
  group_by(年代, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 年代))+
  geom_point()+
  geom_line()


# 全体 ----------------------------------------------------------------------



df %>% 
  filter(都道府県 == '北海道') %>%
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
  filter(都道府県 == '北海道') %>% 
  group_by(年代, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 年代))+
  geom_point()+
  geom_line()

df %>% 
  filter(都道府県 == '北海道') %>% 
  group_by(性別, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_point()+
  geom_line()


# （修理中） -------------------------------------------------------------------


df %>%
  group_by(都道府県, 公表日) %>%
  summarise(一日の感染者数 = n()) %>%
  mutate(累計感染者数 = cumsum(一日の感染者数))  %>%
  arrange(都道府県, desc(公表日)) %>% {
    print(.)
    ggplot(., aes(x = 公表日)) +
      geom_line(aes(y = 累計感染者数))+
      geom_point(aes(y = 累計感染者数))+
      geom_bar(aes(y = 一日の感染者数), stat = 'identity')+
      facet_wrap(~都道府県)
  }


df %>%
  ggplot(aes(x = 年代, fill = 性別))+
  geom_bar()+
  facet_grid(~都道府県)

df %>%
  group_by(性別, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_point()+
  geom_line()

df %>%
  group_by(年代, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 年代))+
  geom_point()+
  geom_line()



# それぞれの県 ------------------------------------------------------------------


prefecture <- 
  function(x){
df %>% 
  filter(都道府県 == x)
  }

prefecture('東京都') %>% 
  arrange(desc(ID))

prefecture('埼玉県') %>% 
  arrange(desc(ID))

prefecture('神奈川県') %>% 
  arrange(desc(ID))

prefecture('千葉県') %>% 
  arrange(desc(ID)) 

prefecture('大阪府') %>% 
  arrange(desc(ID))


