library(tidyverse)
library(magrittr)

df <-
  read_csv('data/covid19_merge.csv', locale = locale(encoding = 'UTF-8')) %>%
  mutate(年代 = case_when(年代 %in% c('0代', '10代') ~ '20未満',
                          年代 %in% c('80代', '90代') ~ '80以上',
                          TRUE ~ 年代) %>% 
             factor() %>% fct_relevel('20未満'),
           都道府県 = factor(都道府県) %>% fct_infreq())

# 全体 ----------------------------------------------------------------------

df %>%
  group_by(性別, 年代, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_line()+
  geom_point()+
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
# 累計感染者数
df %>%
  group_by(都道府県, 公表日) %>%
  summarise(一日の感染者数 = n()) %>%
  mutate(累計感染者数 = cumsum(一日の感染者数))  %>%
  arrange(都道府県, desc(公表日)) %>% {
    slice(., 1:7) %>% print_all()
    ggplot(., aes(x = 公表日, y = 累計感染者数)) +
      geom_line()+
      facet_wrap(~都道府県)
  }


# 1日当たりの感染者数
# 感染者数が0人の日はNAになってしまうので対処
date_index <- 
  tibble(
    公表日 = seq(as_date('2020-01-11'), Sys.Date(), by = 'day'),
  )

df %>%
  group_by(都道府県, 公表日) %>%
  summarise(一日の感染者数 = n()) %>%
  nest() %>% 
  mutate(data = map(data, 
                    ~{full_join(., date_index, by = '公表日') %>% 
                        mutate(一日の感染者数 = replace_na(一日の感染者数, 0))})) %>% 
  unnest(data) %>%  
  arrange(都道府県, 公表日) %>% 
  mutate(
    前一週間の移動平均 = zoo::rollmean(一日の感染者数, 7, na.pad = T, align = 'right'),
    累計感染者数 = cumsum(一日の感染者数)
    ) %>% 
  arrange(都道府県, desc(公表日)) %>% {
    slice(., 1:7) %>% print_all()
    ggplot(., aes(x = 公表日)) +
    geom_bar(aes(y = 一日の感染者数), stat = 'identity', fill = 'red', color = 'red', alpha = 0.5)+
      geom_line(aes(y = 前一週間の移動平均), color = 'blue')+
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
  group_by(年代, 性別, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>% 
  ggplot(aes(公表日, 累計, color = 年代 %>% fct_reorder(.x = 累計, .fun = max, .desc = T)))+
  geom_point()+
  geom_line()+
  labs(color = '年代')+
  facet_wrap(~性別)




# 北海道 ---------------------------------------------------------------------

Hokkaido <- 
  df %>% filter(都道府県 == '北海道')

Hokkaido %>% 
  group_by(性別, 年代, 公表日) %>%
  summarise(一日当たり = n()) %>%
  mutate(累計 = cumsum(一日当たり)) %>%
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 性別))+
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = '3 week')+
  facet_wrap(~年代)


# 年齢別
Hokkaido %>% 
  group_by(年代, 公表日) %>% 
  summarise(一日当たり = n()) %>% 
  mutate(累計 = cumsum(一日当たり)) %>% 
  drop_na() %>%
  ggplot(aes(公表日, 累計, color = 年代 %>% fct_reorder(.x = 累計, .fun = max, .desc = T)))+
  geom_point()+
  geom_line()+
  labs(color = '年代')




# ログリニアモデル ----------------------------------------------------------------

lm_data <- 
  df %>% 
  group_by(都道府県, 年代, 性別) %>%
  summarise(度数 = n()) %>% 
  drop_na() 

formulas <- lm_data %$% 
  c(度数 ~ 年代 + 性別 + 都道府県,
      度数 ~ 年代 * 性別 + 都道府県,
      度数 ~ 年代 + 性別 * 都道府県,
      度数 ~ 性別 + 都道府県 * 年代,
      度数 ~ 年代 * 性別 + 性別 * 都道府県,
      度数 ~ 性別 * 都道府県 + 都道府県 * 年代,
      度数 ~ 都道府県 * 年代 + 年代 * 性別,
      度数 ~ 年代 * 性別 * 都道府県) %>% 
  enframe('model.no', 'formula')


results <- 
  formulas %>% 
  mutate(model = map(formula, ~{glm(formula = ., data = lm_data, family = 'poisson')}),
         tidied = map(model, broom::tidy),
         glanced = map(model, broom::glance))

# モデル比較
results %>% 
  select(-model, -tidied) %>% 
  unnest(cols = glanced)

# 係数
results %>% 
  select(-model, -glanced) %>% 
  filter(model.no %in% c(2, 5, 7)) %>% 
  unnest(cols = tidied) %>% 
  mutate(star = case_when(p.value < 0.001 ~ '***',
                          p.value < 0.01 ~ '**',
                          p.value < 0.05 ~ '*',
                          p.value < 0.1 ~ '+',
                          TRUE ~ '')) %>% 
  mutate_if(.predicate = is.numeric, .funs = round, digits= 3) %>% 
  print_all()
  