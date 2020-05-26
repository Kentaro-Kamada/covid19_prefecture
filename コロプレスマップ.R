library(tidyverse)
library(sf)
print_all <- function(x){
  print(x, n = Inf)
}


# 北海道 ---------------------------------------------------------------------


# 地形データの取得 ----------------------------------------------------------------
# サイトURL: https://gadm.org/download_country_v3.html


# 初回はこれを回す
# # shapeファイルのダウンロード（ちょっと重い）
# download.file(url = 'https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_JPN_shp.zip',
#               destfile = 'data/shape_japan.zip',
#               mode = 'wb')
# # zipファイルの解凍
# unzip(zipfile = 'data/shape_japan.zip', exdir = 'data/shape_japan')
# # zipの方はいらんので削除
# file.remove('data/shape_japan.zip')



# 地形データを読み込む
shape <- 
  st_read(dsn = 'data/shape_japan', layer = 'gadm36_JPN_2') %>%
# shapeファイルの整理
  filter(NL_NAME_1 == '北海道') %>% 
  mutate(居住地 = 
              case_when(
                str_detect(NL_NAME_2, '(夕張市|岩見沢市|美唄市|芦別市|赤平市|三笠市|滝川市|砂川市|歌志内市|深川市|南幌町|奈井江町|上砂川町|由仁町|長沼町|栗山町|月形町|浦臼町|新十津川町|妹背牛町|秩父別町|雨竜町|北竜町|沼田町)'
                                      ) ~ '空知総合振興局',
                str_detect(NL_NAME_2, '(旭川市|名寄市|富良野市|士別市|鷹栖町|東神楽町|当麻町|比布町|愛別町|上川町|東川町|美瑛町|上富良野町|中富良野町|南富良野町|和寒町|剣淵町|下川町|美深町|中川町|幌加内町|音威子府村|占冠村)'
                              ) ~ '上川総合振興局',
                str_detect(NL_NAME_2, '(江別市|千歳市|恵庭市|石狩市|北広島市|当別町|新篠津村)'
                              ) ~ '石狩振興局',
                str_detect(NL_NAME_2, '(小樽市|島牧村|寿都町|黒松内町|蘭越町|ニセコ町|真狩村|留寿都村|喜茂別町|京極町|倶知安町|共和町|岩内町|泊村|神恵内村|積丹町|古平町|仁木町|余市町|赤井川村)'
                              ) ~ '後志総合振興局',
                str_detect(NL_NAME_2, '(釧路市|釧路町|厚岸町|浜中町|標茶町|弟子屈町|鶴居村|白糠町)'
                              ) ~ '釧路総合振興局',
                str_detect(NL_NAME_2, '(室蘭市|苫小牧市|登別市|伊達市|豊浦町|壮瞥町|白老町|厚真町|洞爺湖町|安平町|むかわ町|鵡川町)'
                              ) ~ '胆振総合振興局',
                str_detect(NL_NAME_2, '(日高町|平取町|新冠町|浦河町|様似町|えりも町|新ひだか町)'
                              ) ~ '日高振興局',
                str_detect(NL_NAME_2, '(函館市|北斗市|松前町|福島町|知内町|木古内町|七飯町|鹿部町|森町|八雲町|長万部町)'
                              ) ~ '渡島総合振興局',
                str_detect(NL_NAME_2, '(江差町|上ノ国町|厚沢部町|乙部町|奥尻町|今金町|せたな町)'
                              ) ~ '檜山振興局',
                str_detect(NL_NAME_2, '(留萌市|増毛町|小平町|苫前町|羽幌町|初山別村|遠別町|天塩町)'
                              ) ~ '留萌振興局',
                str_detect(NL_NAME_2, '(稚内市|猿払村|浜頓別町|中頓別町|枝幸町|豊富町|礼文町|利尻町|利尻富士町|幌延町)'
                              ) ~ '宗谷総合振興局',
                str_detect(NL_NAME_2, '(北見市|網走市|紋別市|美幌町|津別町|斜里町|清里町|小清水町|訓子府町|置戸町|佐呂間町|遠軽町|湧別町|滝上町|興部町|西興部村|雄武町|大空町)'
                              ) ~ 'オホーツク総合振興局',
                str_detect(NL_NAME_2, '(帯広市|音更町|士幌町|上士幌町|鹿追町|新得町|清水町|芽室町|中札内村|更別村|大樹町|広尾町|幕別町|池田町|豊頃町|本別町|足寄町|陸別町|浦幌町)'
                              ) ~ '十勝総合振興局',
                str_detect(NL_NAME_2, '(根室市|別海町|中標津町|標津町|羅臼町)'
                              ) ~ '根室振興局', TRUE ~ as.character(NL_NAME_2)))
            

# データセットの読み込み -------------------------------------------------------------


# 今回は北海道のデータを使用
data <- read_csv('data/covid19_Hokkaido.csv')

# 居住地で集計
df <- 
  data %>% 
  group_by(居住地) %>% 
  summarise(患者数 = n())


# データの結合 ------------------------------------------------------------------

bind_data <- 
  left_join(shape, df, by = '居住地')

# コロプレスマップ with ggplot2 ----------------------------------------------------------------

# 振興局別の累計感染者数
df %>% 
  arrange(desc(患者数)) %>% 
  print_all()


# 作図
ggplot()+
  geom_sf(data = bind_data, mapping = aes(fill = 患者数))+
  scale_fill_viridis_c(trans = 'log10')+
  labs(title = str_c('振興局別の新型コロナウイルス累計感染者数（',Sys.Date(), '時点）'),
       fill = '累計感染者')+
  # 経緯度線を描画しない
  coord_sf(datum = NA) +
  # 背景色を白にする
  theme_void()




# 兵庫県 ---------------------------------------------------------------------


# 地形データ -------------------------------------------------------------------


# 地形データを読み込む
shape <- 
  st_read(dsn = 'data/shape_japan', layer = 'gadm36_JPN_2') %>%
  # shapeファイルの整理
  filter(NL_NAME_1 == '兵庫県') %>% 
  rename(居住地 = NL_NAME_2)


# データセットの読み込み -------------------------------------------------------------

data <- read_csv('data/covid19_Hyogo.csv')

# 居住地で集計
df <- 
  data %>% 
  group_by(居住地) %>% 
  summarise(患者数 = n())


# データの結合 ------------------------------------------------------------------

bind_data <- 
  left_join(shape, df, by = '居住地')

# コロプレスマップ with ggplot2 ----------------------------------------------------------------

# 振興局別の累計感染者数
df %>% 
  arrange(desc(患者数)) %>% 
  print_all()


# 作図
ggplot()+
  geom_sf(data = bind_data, mapping = aes(fill = 患者数))+
  scale_fill_viridis_c(trans = 'log10')+
  labs(title = str_c('市町村別の新型コロナウイルス累計感染者数（',Sys.Date(), '時点）'),
       fill = '累計感染者')+
  # 経緯度線を描画しない
  coord_sf(datum = NA) +
  # 背景色を白にする
  theme_void()



# 埼玉県 ---------------------------------------------------------------------


# 地形データ -------------------------------------------------------------------


# 地形データを読み込む
shape <- 
  st_read(dsn = 'data/shape_japan', layer = 'gadm36_JPN_2') %>%
  # shapeファイルの整理
  filter(NL_NAME_1 == '埼玉県') %>% 
  rename(居住地 = NL_NAME_2)


# データセットの読み込み -------------------------------------------------------------

data <- read_csv('data/covid19_Saitama.csv')

# 居住地で集計
df <- 
  data %>% 
  group_by(居住地) %>% 
  summarise(患者数 = n())


# データの結合 ------------------------------------------------------------------

bind_data <- 
  left_join(shape, df, by = '居住地')

# コロプレスマップ with ggplot2 ----------------------------------------------------------------

# 振興局別の累計感染者数
df %>% 
  arrange(desc(患者数)) %>% 
  print_all()


# 作図
ggplot()+
  geom_sf(data = bind_data, mapping = aes(fill = 患者数))+
  scale_fill_viridis_c(trans = 'log10')+
  labs(title = str_c('市町村別の新型コロナウイルス累計感染者数（',Sys.Date(), '時点）'),
       fill = '累計感染者')+
  # 経緯度線を描画しない
  coord_sf(datum = NA) +
  # 背景色を白にする
  theme_void()




# 神奈川県 ---------------------------------------------------------------------


# 地形データ -------------------------------------------------------------------


# 地形データを読み込む
shape <- 
  st_read(dsn = 'data/shape_japan', layer = 'gadm36_JPN_2') %>%
  # shapeファイルの整理
  filter(NL_NAME_1 == '神奈川県') %>% 
  rename(居住地 = NL_NAME_2)


# データセットの読み込み -------------------------------------------------------------

data <- read_csv('data/covid19_Kanagawa.csv')

# 居住地で集計
df <- 
  data %>% 
  group_by(居住地) %>% 
  summarise(患者数 = n())


# データの結合 ------------------------------------------------------------------

bind_data <- 
  left_join(shape, df, by = '居住地')

# コロプレスマップ with ggplot2 ----------------------------------------------------------------

# 振興局別の累計感染者数
df %>% 
  arrange(desc(患者数)) %>% 
  print_all()


# 作図
ggplot()+
  geom_sf(data = bind_data, mapping = aes(fill = 患者数))+
  scale_fill_viridis_c(trans = 'log10')+
  labs(title = str_c('市町村別の新型コロナウイルス累計感染者数（',Sys.Date(), '時点）'),
       fill = '累計感染者')+
  # 経緯度線を描画しない
  coord_sf(datum = NA) +
  # 背景色を白にする
  theme_void()






