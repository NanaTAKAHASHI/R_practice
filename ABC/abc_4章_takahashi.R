### 4章　シンプルな関数
# set wd
library(rstudioapi)
library(lintr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 4.1. tapply() 関数
# データの読み込み
Veg <- read.table(file = "Vegetation2.txt", header = T )
names(Veg) # 変数名の確認
str(Veg) # データの構造

# 4.1.1 トランセクトごとの平均の計算

m <- mean(Veg$R)
m1 <- mean(Veg$R[Veg$Transect == 1])
m2 <- mean(Veg$R[Veg$Transect == 2])
m3 <- mean(Veg$R[Veg$Transect == 3])
m4 <- mean(Veg$R[Veg$Transect == 4])
m5 <- mean(Veg$R[Veg$Transect == 5])
m6 <- mean(Veg$R[Veg$Transect == 6])
m7 <- mean(Veg$R[Veg$Transect == 7])
m8 <- mean(Veg$R[Veg$Transect == 8])
c(m1, m2, m3, m4, m5, m6, m7, m8)

#### 4.1.2 トランセクトごとの平均値を効率的に計算する
# tapply 関数：カテゴリごとに列に対して処理を実行
tapply(Veg$R, Veg$Transect, mean)
tapply(X = Veg$R, INDEX  = Veg$Transect, FUN = mean)
# 引数だけでも指定できる

# トランセクトごとに平均値，SD, データ数を算出する
Me <- tapply(Veg$R, Veg$Transect, mean)
Sd <- tapply(Veg$R, Veg$Transect, sd)
Le <- tapply(Veg$R, Veg$Transect, length)
cbind(Me, Sd, Le)

### 4.2 sapply()関数とlapply()関数
# 植物データごとに平均値を算出
str(Veg)

# 1~4列目はカテゴリなので除外する
sapply(Veg[, 5:9], FUN = mean) # tapplyと異なり，カテゴリごとの平均値ではないので注意


lapply(Veg[, 5:9], FUN = mean) # 結果がリストで帰ってくる
Veg[, 5:9]
# cbindだと出力された結果は1つのベクトルになってしまうので注意
sapply(cbind(Veg$R, Veg$ROCK, Veg$LITTER, Veg$ML, Veg$BARESOIL), FUN = mean)

# データフレームとして結果を保存する
sapply(data.frame(Veg$R, Veg$ROCK, Veg$LITTER, Veg$ML, Veg$BARESOIL), FUN = mean)

### 4.3 summary()関数
# 変数ごとに基本統計量を出す
Z <- cbind(Veg$R, Veg$ROCK, Veg$LITTER)
colnames(Z) <- c("R", "ROCK", "LITTER")
summary(Z) # 最小値，第一四分位点，中央値，平均値，第三四分位点

# 以下の書き方でも同じ
summary(Veg[, c ("R", "ROCK", "LITTER")])
summary(Veg[, c(5, 6, 7)])

### 4.4 table()関数
# 鹿データ読み込み
Deer <- read.table(file = "Deer.txt", header = T)
names(Deer)
str(Deer)

# 農場ごとに観測値を集計する
table(Deer$Farm)

# 年ごと x 性別の分割表
table(Deer$Sex, Deer$Year)

### 4.6　練習問題
# 練習問題1
dat <- read.csv("Temperature.csv", header = T)
head(dat)
unique(dat$Year)

# 集計のためにリストとして年と月をlistをまとめる
# それをカテゴリとして平均値を計算する
# 平均値
res1 <- tapply(dat$Temperature, list(dat$Year, dat$Month), mean, na.rm = T)

# 月ごとの標準偏差
res2 <- tapply(dat$Temperature,  dat$Month, sd, na.rm = T)

# 月ごとの観測数
res3 <- tapply(dat$Temperature,  dat$Month, length)

# 練習問題2
head(dat)
# 観測地点あたりの観測数
table(dat$Area)

# 年当たりの観測数
table(dat$Year)

# 観測地点の年ごとの観測数
table(dat$Year, dat$Area)
