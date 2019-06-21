# 5章 探索的データ分析 ----
# 5.1 はじめに
# 5.1.1 準備するもの
library(tidyverse)

# 5.2 質問
# 変数においてどんな変動があるか
# 変数間にはどんな共変動があるか

# 5.3 変動----
# 5.3.1 分布の可視化
# カテゴリ変数を横軸にとる
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

# 連続値を横軸にとる
# 本文の書き方のmapping = にするとエラー帰ってくるのでぬいた
ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.5)

# deplyr::countと組み合わせて使う
# count_width()でcaratの幅を0.5ずつ変化させた度数分布表をつくる
diamonds %>%
  count(cut_width(carat, 0.5))

# ３カラット以下のダイヤに絞ってヒストグラムのカウントの幅を狭くする
smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# 複数のヒストグラムを重ねて表示する
# geom_freqployを使う
# カウント数を折れ線で表現するので重なりが見やすい
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

# ヒストグラムからデータの傾向を分析する
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# ピークよりも右側（よりカラットが大きいほう）に分布が偏ってる
# 微妙な大きさの時はカラット数を盛って繰り上げで処理してるのかも（詐欺では）

# 間欠泉のデータ
# 噴出時間には短いもの（ピークが2分くらい）と長いもの（ピークが4-5分）の二種類がざっくりありそう，という傾向がわかる
str(faithful)
ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

# 5.3.3 異常値
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# ピークが飛び出している部分があるのがわかる
# y軸の幅が大きすぎるので拡大
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# 拡大すると外れ値は0, 30, 60あたりにありそうなことがわかる

# データセットの中でも確認
(unusual <- diamonds %>%
    filter(y < 3 | y > 20) %>%
    select(price, x, y, z) %>%
    arrange(y))

# おそらくデータの入力ミスがあることが外れ値の原因
# ダイヤの大きさの割に価格が安すぎる

# 5.3 練習問題----
# 練習問題1
ggplot(data = diamonds) +
  geom_freqpoly(binwidth = 0.1, aes(x = x), color = c("#FF3030")) +
  geom_freqpoly(binwidth = 0.1, aes(x = y), color = c("#228B22")) +
  geom_freqpoly(binwidth = 0.1, aes(x = z), color  = c("#FFC125"))

# 練習問題2
# まず普通にプロット
ggplot(data = diamonds , aes(x = price)) +
  geom_histogram()

# binを広くとる
ggplot(data = diamonds , aes(x = price)) +
  geom_histogram(binwidth = 20)

# 途中で全くデータのない価格帯（商品として存在しない）ところがある
# 拡大して調べる
ggplot(data = diamonds , aes(x = price)) +
  geom_histogram(binwidth = 20) +
  coord_cartesian(xlim = c(0, 2500))

# 1500(ドル？)のダイヤはなぜか存在しない

# 練習問題3
# 0.99 カラットのダイヤ
dat_0.99 <- diamonds %>%
  filter(carat == 0.99) %>%
  count(carat) # 23こある

# 1カラットのダイヤ
dat_one <- diamonds %>%
  filter(carat == 1) %>%
  count(carat) # 1558こある

# 自然な分布に対して1カラット多すぎ
# 多分もったほうが価格が高くなる
# 自称170cm問題

# 練習問題4
# x軸を拡大
# coord_caratesian()を使う
diamonds %>%
  ggplot() +
  geom_histogram(aes(x = carat)) +
  coord_cartesian(xlim = c(0, 2))

# xlim()をつかう
diamonds %>%
  ggplot() +
  geom_histogram(aes(x = carat)) +
  xlim(c(0, 2))

# y軸の値がずれる・かつ，xlimの指定だとプロットから除外される値がある

# y軸を拡大
# coord_caratesian()を使う
diamonds %>%
  ggplot() +
  geom_histogram(aes(x = carat)) +
  coord_cartesian(ylim = c(0, 2000))

# ylim()をつかう
diamonds %>%
  ggplot() +
  geom_histogram(aes(x = carat)) +
  ylim(c(0, 2000))
# やっぱりy軸の値がずれる

# 5.4 欠損----
# データセットに異常値があるときの方略
# 値がおかしい行を削除する
(diamonds2 <- diamonds %>%
   filter(between(y, 3, 20)))

# 異常値を欠損値として置き換える
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# NAがあるよっていうエラーメッセージが返ってくる
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

# na.rm = TRUE で 解除
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

# 欠損値のある変数とそうでない変数を比較する
str(nycflights13::flights)
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    # キャンセル便と通常便の変数
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
    
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color = cancelled),
                binwidth = 1 / 4)

# キャンセルされてない便（FALSE）のほうが多いのでキャンセル便との違いを見出すのは困難

# 5.4 練習問題----
# 練習問題1
# flight データセットの欠損値の違い
# ヒストグラム
nycflights13::flights %>%
  ggplot() +
  geom_histogram(aes(x = dep_time))

# 棒グラフ
nycflights13::flights %>%
  ggplot() +
  geom_bar(aes(x = dep_time))

# たぶんbinの幅がちがう？

# 練習問題2
# mean
mean(diamonds$price)

mean(diamonds$price, na.rm = TRUE)

# sum
nycflights13::flights %>%
  group_by(dep_time) %>%
  count() %>%
  sum()

nycflights13::flights %>%
  group_by(dep_time) %>%
  count() %>%
  sum(na.rm = TRUE)

# 5.5 共変動----
# 変動
# 変数の中でのふるまい
# 共変動
# 複数の変数の間でのふるまい

# 5.5.1 カテゴリ変数と連続変数
# ダイヤモンドの価格が品質でどうかわるか
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# 価格によって全体の個数が違いすぎて比較できない
ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))
# ideal とfairの数が全く異なる

# y軸に確率密度をとる
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# 分布だけでみるとfair(普通)のダイヤモンドの価格が一番高い。。。？
# 箱ひげ図をつくる
# カットの質によって値段はかわるか
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

# 中央値で見てもidealの価格が相対的に安いことがわかる
# mpgデータ
# 高速での燃費がクラスによってかわるか
ggplot(data = mpg, mapping = aes(a = class, y = hwy)) +
  geom_boxplot()

# 中央値の順に箱を並び替える
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),
                             y = hwy))

# 変数名が長いのでならびかえる
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median),
                             y = hwy)) +
  coord_flip()

# 5.5.1  練習問題----
# 練習問題1
# 数が全然違うのでとりあえず箱ひげ
dat_cancelled <- nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    # キャンセル便と通常便の変数
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
    
  )
ggplot(data = dat_cancelled ,
       mapping = aes(x = cancelled , y = sched_dep_time)) +
  geom_boxplot()

# 練習問題2
# diamondsに含まれる変数を確認
str(diamonds)
# x, y, zは大きさに関する変数なのでcaratと共変してる可能性
# 値段に影響してるのは大きさ(carat)と質(cut)と予測
library(tidyverse)
library(GGally)

# 関係があると予測した変数のみを選ぶ
dat_exp2 <- select(diamonds, carat, cut, price, color)
# めちゃくちゃプロットに時間がかかる
ggpairs(dat_exp2, aes_string(colour = "cut", alpha = 0.5))

# どの品質でも大体同じくらいの色が含まれている
# 何が値段と関連してるのか正直よくわからない

# 練習問題5
# geom_violin
ggplot(diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin()

# geom_histogram
ggplot(diamonds, aes(x = price, fill = cut), alpha = 1 / 4) +
  geom_histogram()

# geom_freqploy
ggplot(diamonds, aes(color = cut, x = price)) +
  geom_freqpoly()

# 5.5.2 ２つのカテゴリ変数
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

# 円の大きさがデータの観測数を表す

# dplyrでデータの数を数える
diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# 5.5.2 練習問題----
# 練習問題1
library(viridis)
# cutの質によって塗分けの色相を変える
# かつ，観測値のばらつきが大きいので観測数で割った割合にする
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_fill_viridis(limits = c(0, 1))

# 練習問題2
nycflights13::flights %>%
  group_by(month, dest) %>%
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile()

# 問題点
# 目的地の数が多すぎてわからない, かつ白いタイルが多い
# ダイヤの時と同じように目的地の色分けが反映されてない
library(forcats)
nycflights13::flights %>%
  group_by(month, dest) %>%
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%
  filter(n() == 12) %>%
  ungroup() %>%
  mutate(dest = fct_reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  scale_fill_viridis()

# 練習問題3
# x = color, y = cut
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# x = cut, y = color
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = n))

# グラデーションの色が個数が少ない順に対応しているから・・・？

# 5.5.3 2つの連続変数
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

# 点が重なっているところは黒くなってしまっている
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price),
             alpha = 1 / 100)

# hexbinを使う方法
library(hexbin)
# 箱ひげ図を使う方法（データを区間として示す）
ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

# 5.5.3 練習問題----
# 練習問題1
# cut_width()をつかう
# freqployでプロットする
ggplot(data = diamonds,
       mapping = aes(x = price,
                     colour = cut_width(carat, 0.3))) +
  geom_freqpoly()

# y軸を密度に直す
ggplot(data = diamonds, 
       mapping = aes(x = price,
                     y = ..density.., 
                     colour = cut_width(carat, 0.3))) +
  geom_freqpoly()

# cut_number()をつかう
ggplot(data = diamonds, 
       mapping = aes(x = price,
                     colour = cut_number(carat, 10))) +
  geom_freqpoly()

# y軸を密度に直す
ggplot(data = diamonds, 
       mapping = aes(x = price,
                     y = ..density..,
                     colour = cut_number(carat, 10))) +
  geom_freqpoly()

# 練習問題2
library(tidyverse)

diamonds %>%
  count(carat, price) %>%  
  ggplot(aes(x= cut_number(price, 5), y=carat))+ 
  geom_boxplot()+
  coord_flip()
  
# 値段が高いほどcaratの大きいダイヤが多いことがわかる

# 練習問題3
ggplot(diamonds) +
  geom_boxplot(aes(x = cut_number(carat, 10), y = price)) +
  coord_flip()

# 基本的にはcaratが大きいほど値段が高い
# ただし，小さいダイヤは非常に値段の分散が小さい

# 練習問題4
# 練習問題3で作ったプロットをcutの質ごとに色分け
ggplot(diamonds) +
  geom_boxplot(aes(x = cut_number(carat, 10), y = price, fill=cut)) +
  coord_flip()
# すごく見づらい

# facetをcutでかける
ggplot(diamonds) +
  geom_boxplot(aes(x = cut_number(carat, 10), y = price)) +
  coord_flip()+
  facet_grid(~cut)

# 練習問題5
# 本文中のプロット
ggplot(data = diamonds)+
  geom_point(mapping = aes(x=x, y=y))+
  coord_cartesian(xlim=c(4,11), ylim= c(4,11))

# 5.6 パターンとモデル----
library(tidyverse)
# 間欠泉の噴出パターンをみる
# ２つクラスターがあるのがわかる
ggplot(data = faithful) +
  geom_point(mapping = aes(x=eruptions, y = waiting))

# 残差から各変数同士の関係を見る
# 残差：予測値と実測値の差
library(modelr)

# ダイヤモンドの大きさ(carat)から値段を予測するモデルを立てる
mod <- lm(log(price)~log(carat), data = diamonds)
summary(mod)

# 残差をプロットする
# 残差をデータセットに追加する
diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid=exp(resid))

# 残差のプロット
ggplot(data = diamonds2) +
  geom_point(mapping = aes(x=carat, y= resid))

# このモデルだと，大きさの効果（carat）を仮定している
# 残差が小さいほど実測値の値段を予測できている，ということになる
# 大きさが大きい（caratが5とか）ほど残差が小さいことがわかる
# ＝大きさがある程度大きい時は，大きさから価格を予測できる
# 大きさが小さいと，残差が大きくなっている
# つまり，大きさの効果以外にも値段に影響を与える要因が存在する，という予想が立てられる

# ダイヤの質と値段の関係をみる
ggplot(data = diamonds2) +
  geom_boxplot(mapping = aes(x=cut, y=resid))

# 5.7 ggplot2呼び出し----
# ggplotをより簡単に書く
ggplot(data=faithful, mapping = aes(x=eruptions))+
  geom_freqpoly(binwidth = 0.25)

# 引数は省略して書くこともできる
# ただしわかりにくいからあんまり省略しないほうがいいかも・・・
ggplot(faithful, aes(eruptions))+
  geom_freqpoly(binwidth = 0.25)

# ggplot2内ではパイプ演算子が使えないので注意
diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill=n))+
  geom_tile()
