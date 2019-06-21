## 2章　ワークフロー：基本
library(tidyverse)

## 2.1 コーディングの基本
# Rの基本電卓操作
1/200*30
(59+73+2)/3
sin(pi/2)

x <- 3*4

# object_name <- value
# 値をある変数に格納する

## 2.2 名前の中には何がある？
# 変数の名前は_アンダーバーでくぎる

this_is_a_really_long_name <- 2.5

# 長い変数名を入力したいときはtabキーを使う
r_rocks <- 2^3
r_rocks
R_rocks # 大文字と小文字は区別される

## 2.3 関数呼び出し
seq(1,10)
x <- "hello world"
# x <- "hello 
# 途中で不完全なコードを実行してしまうと+が表示される

y <- seq(1,10, length.out = 5)
y
(y <- seq(1,10, length.out = 5)) 
# 出力も同時にみたいときは()で囲む

## 練習問題
# 練習問題1
my_variable <- 10
my_varlable # スペルミス

# 練習問題2
# 修正前のコード
library(tidyverse)
# スペルミス
ggplot(dota = mpg)+ # o -> a 
  geom_point(mapping = aes(x=displ,y=hwy))
ggplot(data = mpg)+ # o -> a 
  geom_point(mapping = aes(x=displ,y=hwy))

# スペルミス, =が1つたりない
fliter(mpg, cyl = 8)
filter(mpg, cyl == 8)

# データセットの名前が間違ってる
filter(diamond, carat >3)
filter(diamonds, carat >3)

# 練習問題3
# ショートカットが出てくる