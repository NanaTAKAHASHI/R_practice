# 手打ちで変数に値を代入する
a <- 59
b <- 55
c <- 53.5
d <- 55
e <- 52.5

Wing1 <- 59
Wing2 <- 55
Wing3 <- 53.5
Wing4 <- 55
Wing5 <- 52.5

sqrt(Wing1)
2*Wing1
Wing1 + Wing2
Wing1 + Wing2 + Wing3 + Wing4 + Wing5
(Wing1 + Wing2 + Wing3 + Wing4 + Wing5)/5

# オブジェクトに計算結果を代入する
SQ.wing1 <- sqrt(Wing1)
Mul.W1 <- 2*Wing1
Sum.12 <- Wing1 + Wing2
SUm12345 <- Wing1 + Wing2 + Wing3 + Wing4 + Wing5
Av <- (Wing1 + Wing2 + Wing3 + Wing4 + Wing5)/5

# コマンド全体をまるかっこで囲むと入力と出力が同時にできる
(SQ.wing1 <- sqrt(Wing1))


# c()関数で複数の値を1つのオブジェクトにまとめる
Tarsus <- c(22.3, 19.7, 20.8, 20.3, 20.3, 21.5, 20.6, 21.5 )
Head <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8, 32.5, NA)
Wt <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6, 15.6, 15.7)

# cでまとめたデータへのアクセス
Wingcrd[1] # 最初の要素が表示される
Wingcrd[1:5] # 1 - 5番目の要素が表示される
Wingcrd[-2] # 2番目の要素を除外

# 基本統計量を表示する
sum(Wingcrd) # sum()関数
S.win <- sum(Wingcrd)
S.win

# 欠損値が含まれるとエラーがおこる関数がある
# 欠損値の除外
sum(Head, na.rm = T)

###　オブシェクトの結合　###
BirdData <- c(Wingcrd, Tarsus, Head, Wt)

# rep()関数による繰り返し
Id <- rep(c(1, 2, 3, 4), each = 8)
Id <- rep(1:4, each = 8)

a <- seq(from =1, to = 4, by = 1)
VarNames <- c("Wingcrd", "Tarsus", "Head", "Wt")
Id2 <- rep(VarNames, each = 8)
Id2

z <- cbind(Wingcrd, Tarsus, Head, Wt)
z

# データの次元属性をしらべる
dim(z)

z2 <- rbind(Wingcrd, Tarsus, Head, Wt)
z2

Dmat <- matrix(nrow = 8, ncol = 4)
Dmat

Dmat[, 1] <- c(59, 55, 53.5, 55, 52.5, 57.5, 53, 55)
Dmat[, 2] <- c(22.3, 19.7, 20.8, 20.3, 20.3, 21.5, 20.6, 21.5 )
Dmat[, 3] <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8, 32.5, NA)
Dmat[, 4] <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6, 15.6, 15.7)

colnames(Dmat) <- c("Wingcrd", "Tarsus", "Head", "Wt")
Dmat

Dfrm <- data.frame(WC = Wingcrd, TS = Tarsus, HD = Head, W =Wt, Wsq = sqrt(Wt))
Dfrm

### list() 関数によるデータの結合 ###
x1 <- c(1, 2, 3)
x2 <- c("a", "b", "c", "d")
x3 <- 3
x4 <- matrix(nrow = 2, ncol = 2)
x4[, 1] <- c(1, 2)
x4[, 2] <- c(3, 4)
Y <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
Y

M <- lm(WC ~ Wt, data = Dfrm)
names(M)

Alldata <- list(BirdData = BirdData, Id = Id2, z = z, VarNames = VarNames)
Alldata

### 外からデータセットを読み込む ###
Squid <- read.table(file = "C:\Users\NANA\Dropbox\Rabc\\squid.txt", header = TRUE)

# テキストデータは.区切りのデータ
Squid <- read.table(file = "C:\Users\NANA\Dropbox\Rabc\\squid.txt", header = TRUE, dec = ".")

### ほかの統計ソフトからデータを読み込む ###
library(foreign) # SAS/SPSSなどからデータを読み込む

# データベースへのアクセス
library(RODBC)
setwd("C:\Users\NANA\Dropbox\Rabc")

