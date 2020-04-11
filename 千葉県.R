library(tidyverse)
library(lubridate)
library(pdftools)


print_all <- 
  function(x){
    print(x, n = Inf)
  }

# 千葉県のデータを取得 --------------------------------------------------------------

scrape_res <- 
  pdf_text('https://www.pref.chiba.lg.jp/shippei/press/2019/documents/0409kanssensya.pdf')

data <- 
  scrape_res %>% 
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
  mutate(年代 = case_when(年代 == '10歳未満' ~ '0',
                          grepl(x = 年代, '代') ~ sub(x = 年代, '(代|代以上)', ''),
                          TRUE ~ NA_character_)) %>% 
  # 性別のクリーニング
  mutate(性別 = case_when(性別 %in% c('男性', '女性') ~ sub(x = 性別, '性', ''),
                          TRUE ~ NA_character_)) %>% 
  # メモ：居住地のクリーニング（県外をNAにするかどうか）
  
  # 型変換
  mutate(公表日 = parse_date(公表日))

# 保存
write_excel_csv(data, 'data/covid19_Chiba.csv')



# 分析 ----------------------------------------------------------------------

df <- read_csv('data/covid19_Chiba.csv', col_types = cols(年代 = col_character()))  

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
  count(居住地, sort = TRUE) %>% 
  filter(n >= 6) %>% 
  ggplot(aes(x = 居住地, y = n))+
  geom_bar(stat = 'identity')  


 
