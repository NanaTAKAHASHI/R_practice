# set wd
library(rstudioapi)
library(lintr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

### 6章　ループと関数
### 6.1 ループ処理
# まずは設計したい関数を日本語なりでかいて整理する
# いきなりコードを書かない（自戒ブーメラン）

### 6.2 ループ
## 6.2.1 自分自身のコードの設計者になる
## ステップ1: データのインポート
# メンフクロウデータの読み込み
Owls <- read.table('Owls.txt', header = TRUE)
str(Owls)
# 変数の確認
names(Owls)

## ステップ2と3：散布図の作成とラベルの追加
# ラベル作成のために巣の名前を把握
unique(Owls$Nest)
# 特定の巣の情報を抽出する
Owls.AVT <- Owls[Owls$Nest == "AutavauxTV",]

# AutavauxTVでのArrivalTimeとNegPerChickをプロットする
# 散布図の作成
plot(x = Owls.AVT$ArrivalTime, y = Owls.AVT$NegperChick,
xlab = "到着時間", ylab = "餌請い行動",
main = "AutavauxTV")

## 6.2.4 ステップ4：汎用性のあるコード設計
# 上で書いた散布図用のコードをほかの巣でも試してみる
Owls.Bot <- Owls[Owls$Nest == "Bochet",]
plot(x = Owls.Bot$ArrivalTime, y = Owls.Bot$NegperChick,
xlab = "到着時間", ylab = "餌請い行動",
main = "Bochet")

# 27個分繰り返すのはきつい
# 巣の名前を変えれば自動化できるようにしたい
# 巣の名前を指定している部分をiに変更
Owls.i <- Owls[Owls$Nest == "Bochet",]
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegperChick,
xlab = "到着時間", ylab = "餌請い行動",
main = "Bochet")

# 手打ちで名前を入れてる部分をなるべく減らす
# タイプミスの減少と手間の削減
Nest.i <- "Bochet" # Nest.i に入れる巣の名前を変更すればほかのラベルも自動的に変更できる
Owls.i <- Owls[Owls$Nest == Nest.i, ]
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegperChick,
xlab = "到着時間", ylab = "餌請い行動",
main = Nest.i)

# 6.2.5 ステップ5：　グラフの保存
# 保存する図のファイルに名前をつける
paste(Nest.i, sep = "", ".jpg") #出力として文字列をかえす
setwd("C:\\Users\\NANA\\Dropbox\\Rabc")
Nest.i <- "Bochet"
Owls.i <- Owls[Owls$Nest == "Nest.i",]
YourFileName <- paste(Nest.i, sep = "", ".jpg")
jpeg(file = YourFileName)
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegperChick,
xlab = "ArrivalTime", ylab = "BeggingBehaviour",
main = Nest.i)
dev.off()

# ステップ6：ループの作成
# 巣の名前を変えて27回変数を格納→作図というのをくりかえす
AllNests <- unique(Owls$Nest) # 巣の名前を取得
for(i in 1:27)
{
    Nest.i <- AllNests[i] # 1-27までのデータの番号に合わせて巣の名前が代入される
    Owls.i <- Owls[Owls$Nest == Nest.i, ]
    YourFileName <- paste(Nest.i, sep ="", ".jpg")
    jpeg(file = YourFileName)
    plot(x = Owls.i$ArrivalTime, y = Owls.i$NegperChick,
         xlab = "ArrivalTime", ylab = "BeggingBehaviour",
         main = Nest.i)
dev.off()
}

### 6.3 関数
# NAと欠損値
getwd()
Veg <- read.table(file = "Vegetation2.txt", header = TRUE)
names(Veg)
# 最初の4列はトランセクトの名前，番号，調査時期がはいってる

# 各変数ごとに欠損値を数える関数をつくる
NAPerVariable <- function(X1)
{
    D1 <- is.na (X1)
    colSums(D1)
}
# 関数単体を実行しても変数を入れなければ何も出力はない

# 作った関数を実行してみる
NAPerVariable(Veg[, 5:24])

# rowSumsにかえると観測データごとの欠損値がでる
NAPerVariable_row <- function(X2)
{
    D2 <- is.na (X2)
    rowSums(D2)
}
NAPerVariable_row(Veg[, 5:24])

### 6.3.2 技術情報
# function()内で使っている引数は一マッチングにより参照されている
# function()内で使う変数はすでに関数外部で使われているものと重複しないようにする
# 変数のミスタイプですでに関数外で使われている変数を入力した場合
# Rは関数内部で該当する変数を探そうとする
# そのあと，関数の外の全く関係ない変数（Xとかありがちなやつ）を参照してしまう

### 6.3.3. ゼロと欠損値に関する二つ目の事例
# 寄生虫データ
Parasite <- read.table(file = "CodParasite.txt", header = T)
names(Parasite)

# 前の節で作った関数を使って変数ごとの欠損値を調べる
NAPerVariable(Parasite)

# モデル分析を仮定して各変数に0がどれくらいあるかを数える関数をつくる
ZeroPerVariable <- function(X1)
{
    D1 <- (X1 == 0)
    colSums(D1)
}
ZeroPerVariable(Parasite)

# 欠損値を無視して，0を含むデータのみを集計する
ZeroPerVariable <- function(X1)
{
    D1 <- (X1 == 0)
    colSums(D1, na.rm = TRUE)
}
ZeroPerVariable(Parasite)

### 6.3.4 複数の引数がある関数
VariableInfo <- function(X1, Choice1)
{
    if (Choice1 == "Zeros") # 0を含むデータを集計するときに指定する引数
    {
        D1 <- (X1 == 0)
    }
    if (Choice1 == "NAs")  # NAを含むデータの集計を指定
    {
        D1 <- is.na(X1)　
    }
    colSums(D1, na.rm = TRUE)
}

VariableInfo(Parasite, "Zeros")
VariableInfo(Parasite, "NAs")
# 結果を格納する
Results <- VariableInfo(Parasite, "Zeros")

# ありそうなエラー
VariableInfo(Parasite, "zeros") # 引数のミス
VariableInfo(Parasite)　# 二つ目の引数を指定しない

### 6.3.5 クラッシュしにくい関数
# 6.3.5.1 関数の引数にデフォルト値を設定
VariableInfo <- function(X1, Choice1 = "Zeros")
{
    if (Choice1 == "Zeros") # 0を含むデータを集計するときに指定する引数
    {
        D1 <- (X1 == 0)
    }
    if (Choice1 == "NAs")  # NAを含むデータの集計を指定
    {
        D1 <- is.na(X1)　
    }
    colSums(D1, na.rm = TRUE)
}
# 二つ目に引数を指定しないときのデフォルトをZerosに指定
VariableInfo(Parasite) # 0を含むデータが返される
VariableInfo(Parasite, "NAs")

# 6.3.5.2 タイプミス
VariableInfo <- function(X1, Choice1 = "Zeros")
{
    if (Choice1 == "Zeros") # 0を含むデータを集計するときに指定する引数
    {
        D1 <- (X1 == 0)
    }
    if (Choice1 == "NAs")  # NAを含むデータの集計を指定
    {
        D1 <- is.na(X1)　
    }
    if (Choice1 != "Zeros" & Choice1 != "NAs")
    {
        print("入力ミスがあります")
    }
    else
    {
        colSums(D1, na.rm = TRUE)
    }
}

# 二番目の引数にタイプミスがあったときに入力ミスがあると表示
VariableInfo(Parasite, "QRURO")

### 6.4 関数とif文についての補足
# 6.4.2 ステップ1：データのインポートと評価
Benthic <- read.table("RIKZ.txt", header =TRUE)
Species <- Benthic[, 2:76] # 種データを抽出
names(Benthic)
str(Benthic)
n <- dim(Species)
n # 行列数を記録

# 6.4.3 ステップ２：地点あたりの総個体数
sum(Species[1, ], na.rm = TRUE) # 地点1の種数

# 45回繰り返すのは絶望しかないのでループ処理
TA <- vector(length = n[1]) # 地点ごとの種数をベクトルデータとして格納
for(i in 1:n[1])
{
    TA[i] <- sum(Species[i, ], na.rm = TRUE)
}
TA

# forループを使わなくても観測地点ごとの種数だせる
TA <- rowSums(Species, na.rm = TRUE)
TA

# 6.4.4 地点あたりの種数
sum(Species[1, ] > 0, na.rm =TRUE)
Richeness <- vector(length = n[1])
for (i in 1:n[1])
{
    Richeness[i] <- sum(Species[i, ] > 0, na.rm =TRUE)
}
Richeness
# RichenessをrowSum()でだす
Richeness <- rowSums(Species > 0, na.rm = TRUE)
Richeness

# 6.4.5 地点あたりのシャノンの指数
RS <- rowSums(Species, na.rm = T)
prop <- Species / RS
H <- rowSums(prop * log10(prop), na.rm = T)
H

# 6.4.6 ステップ５：コードの結合
Choice <- "Richness"
if (Choice == "Richness")
{
    rowSums(Species > 0, na.rm = TRUE)
}
Choice <- "Total_Abumdance" # 空白になってるけどあまりよくないので_にかえた
if (Choice == "Total_Abumdance")
{
    rowSums(Species, na.rm = TRUE)
}

if (Choice == "Shannon")
{
    RS <- rowSums(Species, na.rm = T)
    prop <- Species / RS
    - rowSums(prop * log10(prop), na.rm = T)
}

# 6.4.7 ステップ６：コードを一つの関数にまとめる
# 作ったコードを一つの関数にまとめて計算されるようにする

Index.function <- function(Spec, Choice1)
{
    if (Choice1 == "Richness")
{
    Index <- rowSums(Species > 0, na.rm = TRUE)
}

if (Choice == "Total_Abumdance")
{
    Index <- rowSums(Species, na.rm = TRUE)
}

if (Choice == "Shannon")
{
    RS <- rowSums(Species, na.rm = T)
    prop <- Species / RS
    Index <- - rowSums(prop * log10(prop), na.rm = T)
}
list(Index = Index, MyChoice = Choice1)
}

Index.function(Species, "Shannon") # 最後にリストで結果をまとめているのでリストでかえってくる

# 該当する条件がない時にエラー警告が出るようにする
Index.function <- function(Spec, Choice1)
{
    if (Choice1 == "Richness")
    {
        Index <- rowSums(Spec > 0, na.rm = TRUE)
    }
    else if (Choice1 == "Toral_Abandance") {
        Index <- rowSums(Spec, na.rm = TRUE)
    }
    else if (Choice1 == "Shannon") {
        RS <- rowSums(Spec, na.rm =TRUE)
        prop <- Spec /RS
        Index <- -rowSums(prop*log(prop), na.rm =TRUE)
    }
    else
{
    print ("コードを見直してください")
    Index <- NA
}
list(Index = Index, MyChoice = Choice1)
}

Index.function(Species, "Shannon")

### 6.6 練習問題
# 練習問題1

dat <- read.csv(file = "Temperature.csv", header = TRUE, dec = ".")
names(dat)
str(dat)


AllStations <- unique(dat$Station)
N <- length(AllStations)
for (i in 1:N) {
  Station.i <- as.character(AllStations[i])
  print(Station.i)
  lis_Tempature <- dat[dat$Station == Station.i ,]
  YourFileName <- paste(Station.i, ".jpg", sep = "") 
  jpeg(file = YourFileName)
  plot(x = TPi$dDay2 , y = lis_Tempature$Temperature,  # y軸の値に気温を指定
       xlab = "経過日数",
       ylab = "気温", 
       main = Station.i)
  dev.off()
}


# 練習問題2
Owls <- read.table(file = "Owls.txt", header = TRUE, dec = ".")
names(Owls)
str(Owls)  # FoodTreatmentに水準が二種類あるのを確認

# 給餌量によってカテゴリを新しく作る
ifelse(Owls$FoodTreatment == "Satiaded", 
       Owls$NestNight <- paste(Owls$Nest, "1",sep = "_"),
       Owls$NestNight <- paste(Owls$Nest, "2",sep = "_"))
head(Owls)                             

AllNestsNights <- unique(Owls$NestNight)
N <- length(AllNestsNights)
for (i in 1:N) {
  NestNight.i <- as.character(AllNestsNights[i])
  print(NestNight.i)
  Owlsi <- Owls[Owls$NestNight == NestNight.i ,]
  YourFileName <- paste(NestNight.i, ".jpg", sep = "") 
  jpeg(file = YourFileName)
  plot(x = Owlsi$ArrivalTime , y = Owlsi$NegPerChick, 
       xlab = "到着時間",
       ylab = "餌請い時間",
       main = NestNight.i)
  dev.off()
}
# 練習問題3
# 例題でやったのと同じなので省略します