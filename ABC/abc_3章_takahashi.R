### 3章 ###
### 変数のアクセスと部分データの操作
### 3.1 データフレームの変数にアクセス
# すみません、パスの指定がめんどいのでパッケージ使ってます
# rstudioapi使うと.Rファイルを開いている場所に自動的にディレクトリが設定されます

# フォルダを行き来して解析したりファイルのやり取りがあるときに楽
# R内で読み込ませたいデータファイルはRファイルと同じフォルダに入れておく
library(rstudioapi)
library(lintr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Squid <- read.table(file = "squid.txt", header = T)
Squid

# データの変数名を確認
names(Squid)

# データフレーム内の変数の種類を確認
str(Squid) # 変数によって整数型(integer)か数値型(numeric)が異なる

### 3.3.1. str関数
# strを使うとデータの変数処理によるミスが見つけられる
Squid2 <- read.table(file = "squidGSI.txt", header = T, dec = ",")

str(Squid2) 
# $GSI変数が因子型と判断されていることがわかる
# 作図や解析をするときにエラーくる

### 3.1.2.
# 関数のデータ引数
# Suidデータを変数に格納しておくとモデル分析などではdata = を引数としてデータを引用できる
M1 <- lm(GSI ~ factor(Location)*factor(Year), data = Squid)

# ただし、data = でデータを引き出せない関数もあるので注意
mean(GSI, data = Squid)

# 3.1.3 $記号
# $を使ってデータにアクセスする
Squid$GSI

# ベクターとして列を指定して取り出す
Squid[, 6]

# mean()ではベクターとしてとりだせばオッケー
mean(Squid$GSI)

### 3.1.4. attach()関数
# attach()関数を使ってSquidをパスとして指定する
# Squid$ の指定をしなくてもよくなる
attach(Squid)

# 箱ひげ図をかく
boxplot(GSI)
mean(GSI)
# ただし、違うデータセットに同じ名前の変数があるときなどに注意

### 3.2 データの一部にアクセスする
# Squid データでは性別が1か2でコードされている
# 1がオス、2がメス
Squid$Sex

# uniqu関数では重複なくデータの要素を返す
unique(Squid$Sex)

# オスの全データにアクセス
Se1 <- Squid$Sex == 1
SquidM <- Squid[Se1,]
SquidM

# 1行でまとめて書いてもよい
SquidM <- Squid[Squid$Sex == 1,]
SquidM

# 同じようにメスデータにアクセス
SquidF <- Squid[Squid$Sex == 2,]
SquidF

# 論理演算子の使い方によって条件付き選択できる
# | or
Squid123 <- Squid[Squid$Location == 1 | Squid$Location == 2 | Squid$Location == 3,]
# != not
Squid123 <- Squid[Squid$Location != 4,]
Squid123 <- Squid[Squid$Location < 4,]
# ～以下
Squid123 <- Squid[Squid$Location <= 3,]
# 値の範囲指定（1以上3以下）
Squid123 <- Squid[Squid$Location >= 1 & Squid$Location <= 3,]

# 位置コードが1かつ場所コードが1のオスのデータのみをとりだす
SquidM.1 <- Squid[Squid$Sex == 1 & Squid$Location == 1, ]
SquidM.1

# オスの位置コード1もしくは2簿データを抽出
SquidM.12 <- Squid[Squid$Sex == 1 & (Squid$Location ==1 | Squid$Location == 2),]

SquidM <- Squid[Squid$Sex == 1,]
SquidM1 <- SquidM[SquidM$Location == 1,]
SquidM1

# 初めに指定したオスのデータは含まれるデータの行数がLocationの指定よりも多いためNAが代入されてしまう

# 含まれるデータがない時は0行と返される
Squid[Squid$Location == 1 & Squid$Year == 4 & Squid$Month == 1,]

### 3.2.1 データのソート
# 月ごとにデータを並び替える
Ord1 <- order(Squid$Month)
Squid[Ord1,]

# 変数一つに関して並び替えも可能
Squid$GSI[Ord1]

### 3.3 共通の識別番号を使った2つのデータセットの結合
Sq1 <- read.table(file = "squid1.txt", header = T)
Sq2 <- read.table(file = "Squid2.txt", header = T)

# データを結合
SquidMerged <- merge(Sq1, Sq2, by = "Sample") # 識別番号としてSample列を指定
SquidMerged

# all をTRUEにするとどちらかのデータセットにデータがない行にはNAが補完される
# デフォルトはFALSE
SquidMerged <- merge(Sq1, Sq2, by = "Sample", all = T) 

### データのエクスポート
SquidM <- Squid[Squid$Sex == 1, ]

write.table(SquidM, file = "MaleSquid.txt", sep = " ", quote = F, na = "NA", append = F)
# sep = データの区切りの指定
# na = 欠損値を表現する値の指定
# append = F 新規にファイルを作成（上書きしない）

### 3.5 カテゴリカルデータの再数値化
str(Squid)

# 変数を因子型として追加
Squid$Location <- factor(Squid$Location)
Squid$Sex <- factor(Squid$Sex)

# 文字列によるラベルをつける
Squid$Sex <- factor(Squid$Sex, levels = c(1, 2), labels = c("M", "F"))
Squid$Sex
Squid$fSex <- factor(Squid$Sex, levels = c(1,2))

# 性別ごとにプロットをかく
boxplot(GSI~ Sex, data = Squid)

#　モデルでもつかえる
M1 <- lm(GSI ~ Sex + Location, data = Squid)
summary(M1) # 性別の効果に関してＦ，Ｍで表記されている
M2 <- lm(GSI ~ factor(Sex) + factor(Location), data = Squid) #as.factorとかいてもよい
summary(M2)

# factorの順序を変えるとboxplot上での順番が変わる
Squid$Location
Squid$fLocation <- factor(Squid$Location, levels = c(2,3,1,4))
Squid$Location
boxplot(GSI ~ fLocation, data = Squid)

SquidM <- Squid[Squid$Sex == 1,]

#factorに変換した数値データは””で囲む
SquidM <- Squid[Squid$fSex == "1",]

# strでfactorにした変数の構造を確認する
Squid$fSex <- factor(Squid$Sex , labels = c ("M", "F"))
Squid$fLocation <- factor(Squid$Location)
str(Squid)

### 練習問題 3.7
### 練習問題1
BirdFlu <- read.csv(file = "BirdFlu.csv", header = F) 
BirdFlu <- as.data.frame(BirdFlu)
str(BirdFlu)

# 列名変更
colnames(BirdFlu) <- c ( "Country","2003_cases", "2003_deaths", "2004_cases", "2004_deaths", 
                        "2005_cases", "2005_deaths", "2006_cases", "2006_deaths",
                        "2007_cases", "2007_deaths", "2008_cases", "2008_deaths")
str(BirdFlu)

# 2003年，2005年の罹患者
cases_year <- apply(BirdFlu[,-1], 2, FUN = sum)　# 1列目は国名なので除外

# 国ごとの罹患者数
BirdFlu$cases_country <- apply(BirdFlu[,-1], 1, FUN = sum)

# 罹患者の多い国
BirdFlu[BirdFlu$cases_country == max(BirdFlu$cases_country),]

# 死亡者の少ない国
BirdFlu[BirdFlu$cases_country == min(BirdFlu$cases_country),]

### 練習問題2
ISIT <- read.table(file = "ISIT.txt", header = T) 
head(ISIT)

# 観測地点１での深度の最小値，中央値，平均値，最大値
res_S1 <- c(min(ISIT[ISIT$Station == 1,]$SampleDepth), median(ISIT[ISIT$Station == 1,]$SampleDepth), mean(ISIT[ISIT$Station == 1,]$SampleDepth), max(ISIT[ISIT$Station == 1,]$SampleDepth))

# 観測地点２での深度の最小値，中央値，平均値，最大値
res_S2 <- c(min(ISIT[ISIT$Station == 2,]$SampleDepth), median(ISIT[ISIT$Station == 2,]$SampleDepth), mean(ISIT[ISIT$Station == 2,]$SampleDepth), max(ISIT[ISIT$Station == 2,]$SampleDepth))

# 観測地点３での深度の最小値，中央値，平均値，最大値
res_S3 <- c(min(ISIT[ISIT$Station == 3,]$SampleDepth), median(ISIT[ISIT$Station == 3,]$SampleDepth), mean(ISIT[ISIT$Station == 3,]$SampleDepth), max(ISIT[ISIT$Station == 3,]$SampleDepth))

# 観測数が少ない観測地点

# 2002年のデータ
dat_2002 <- ISIT[ISIT$Year == 2002,]

# 4月のデータ
dat_april <- ISIT[ISIT$Month == 4,]

# 2000メートルを超える深度のデータ
dat_2000m <- ISIT[ISIT$SampleDepth >= 2000,]

# 4月に観測した2000メートルを超えるデータ
dat_april_2000 <- ISIT[ISIT$Month == 4 & ISIT$SampleDepth >= 2000, ]


### 練習問題３
write.table(dat_april_2000, file = "dat_april_2000.txt", sep = ",", quote = F)

### 練習問題４
str(ISIT)
# 月をかえる
ISIT$NewMonth[ISIT$Station <= 5] <- "April"
ISIT$NewMonth[ISIT$Station > 5 & ISIT$Station <= 11 ] <- "August"
ISIT$NewMonth[ISIT$Station > 11 & ISIT$Station <= 15 ] <- "March"
ISIT$NewMonth[ISIT$Station > 15 & ISIT$Station <= 19 ] <- "October"
ISIT$NewMonth
ISIT$NewMonth <- factor(ISIT$NewMonth)
# 年をかえる
ISIT$NewYear[ISIT$Station <= 5] <- "2001"
ISIT$NewYear[ISIT$Station > 5 & ISIT$Station <= 11 ] <- "2001"
ISIT$NewYear[ISIT$Station > 11 & ISIT$Station <= 15 ] <- "2002"
ISIT$NewYear[ISIT$Station > 15 & ISIT$Station <= 19 ] <- "2002"
ISIT$NewYear
ISIT$NewYear <- factor(ISIT$NewYear)
table(ISIT$NewMonth)
table(ISIT$NewMonth)
str(ISIT)
