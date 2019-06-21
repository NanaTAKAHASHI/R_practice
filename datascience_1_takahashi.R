## 1章 ggplot2 によるデータ可視化
## 1.1.1
# パッケージの読み込み
library(tidyverse)
# パッケージごとに関数を読みだすには以下のように表記
ggplot2::ggplot()

## 1.2 第一ステップ
## 1.2.1 mpgデータフレーム
# 車の燃費データセットを使う
head(ggplot2::mpg) # ローデータ
str(ggplot2::mpg) # 変数たくさん

## 1.2.2 ggplotをつくる
# エンジンサイズと燃費の関連をみる
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# 負の相関がありそう
# 1.2.3. data = でデータ指定，geom_hogehoge で任意のプロットがかける

## 1.3 エステティックマッピング
# 車種によって燃費が違うか？をプロットで検証
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) # 外れ値は2人乗りの車

# 車種によって丸の大きさをかえる
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class)) # 順序尺度ではないのでエラーメッセージがくる


# 濃淡で車種をくべつ
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# シンボルの形で ry)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class)) #　一度に6種類までしか使えない

# すべての点を青にする
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue") # 色の名前は文字列として指定


# シンボルの形と色は番号で指定
# 0-14: 塗りつぶしのない形
# 15-18:　塗りつぶしのある形

## 1.4 よくある不具合
# とりあえず関数のエラーは?関数名でコマンドを打つとヘルプがでる
? geom_point

## 1.5 ファセット
# 車種ごとにプロットするパネルを分ける（=ファセットする）
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap( ~ class, nrow = 2)

# 引数を複数指定
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

# 一種類の変数だけを引数にしたい場合
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

## 1.6 幾何オブジェクト
# 同じデータに対して違うプロット（幾何オブシェクト）を指定する
# 散布図
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# 平滑化
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv)) # 走行距離ごとに線をかえる

# 複数のプロットを重ねて表示する
# 一番初めのプロット
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# 走行距離ごとに線をわける（group = ）
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# 走行距離ごとに線の色をわける（color =）
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),
              show.legend = FALSE)

# 散布図と重ねて表示
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# x, yの値はggplot()でも指定できる
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

# レイヤーごとに異なる属性を表示する
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) + # 散布図だけ車種ごとに点を色分け
  geom_smooth()


# レイヤーによって表示するデータセットを変更する
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"),　# subcompactの車種だけ平滑化してプロット
              se = FALSE)

## 練習問題一部
# 練習問題1
# 折れ線グラフ
head(mpg)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_line(mapping = aes(color = class))

# ヒストグラム (車種によるエンジン重量と走行距離のちがい)
ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram(
    mapping = aes(fill = as.factor(displ)),
    binwidth = 1,
    position = "identity",
    show.legend = FALSE
  ) +
  facet_wrap(~ class)

# 箱ひげ図
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_boxplot(mapping = aes(color = class))

# 面グラフ（あんまりみたことない）
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_area(mapping = aes(group = class, fill = class))

# 練習問題6
# 左上
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)

# 右上
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 3) +
  geom_smooth(mapping = aes(group = drv), se = FALSE)

# 中央左
# どちらも走行距離によって色分けしているのでdataの属性を追加
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)

# 中央右
# 散布図のみ走行距離によって色分けしたいので属性をgeom_point内で指定
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv), size = 3) +
  geom_smooth(se = FALSE)

# 左下
# 散布図の色分け，平滑化曲線をそれぞれのgeom_内で属性を指定
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv), size = 3) +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)

# 右下
# 散布図のみ（走行距離によって色分け）
# 後に追加してる平滑化プロットのレイヤー（左下）を消してもほかのレイヤーの属性は保たれたまま
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = drv), size = 3)

## 1.7 統計変換
# diamonds data を使う
str(ggplot2::diamonds)

# 品質別に棒グラフをつくる
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

#　y軸の値はデータセットにあった変数ではなくstatによって算出されてる
? stat # どの統計値（= stat）があるかみてみる

# stat_countを使っても同じ棒グラフがかける
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

# y軸に観測数でなく実数値そのものをとる場合
demo <- tribble(~ a, ~ b,
                "bar_1", 20,
                "bar_2", 30,
                "bar_3", 40)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = a, y = b), stat = "identity")

# 全体に対する各品質のダイアの割合
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1)) # statの中に比率がすでにある


# 各品質ごとに要約統計量を出して記載
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    # x, y の値は通常通り指定
    fun.ymin = min,
    # プロットのyの最小値を指定　　　　　　　　
    fun.ymax = max,
    # プロットのyの最大値を指定
    fun.y = median　　　　　　　　# 表示する統計量を指定
  )

## 練習問題
# 練習問題1
? stat_summary
# ヘルプを読むとデフォルトはgeom_histogram ないし　geom_freqpolyになってる
# どちらもカウントデータでよく使う

# stat_summaryを使わずに書いてみる
# geom_pointrangeをつかう
? geom_pointrange
ggplot(data = diamonds) +
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    stat = "summary",
    # summaryにしないと全体の統計量になってしまうので注意
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 練習問題2
? geom_col
ggplot(data = diamonds) +
  geom_col(mapping = aes(x = cut, y = depth))
# geom_bar と違ってy軸に連続値をとれる
# geom_barではy軸は基本的に指定できない

# 練習問題5
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop..))
#　group=1を指定しないとcutのカテゴリわけがなくなってしまう

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
# 色分けすると比率が正しく計算されていないのがわかる


## 1.8 位置調整
library(tidyverse)
# 色分けはfillでもできる
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

# fillの変数に違う属性を指定することができる
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))

# alphaを変えると色の濃さが変化
# 位置調整をidentity  を指定してもあまり変わらない
ggplot(data = diamonds,
       mapping = aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 1 / 5, position = "identity")

ggplot(data = diamonds,
       mapping = aes(x = cut, color = clarity)) +
  geom_bar(fill = NA, position = "identity")

# position = fillだと積み上げグラフのようになる
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity),
           position = "fill")

# position = dodgeだとバーが重ならずに表示される
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity),
           position = "dodge")

# jitterを使うと点同士の重なりが解消される
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),
             position = "jitter")

## 練習問題
# 練習問題1
# 同じ値のデータ点が多いのでjitterをつかう
# 車種によって点を色分け
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()
ggplot(data = mpg,
       mapping = aes(
         x = cty,
         y = hwy,
         color = class,
         alpha = 1 / 5
       )) +
  geom_point(position = "jitter")

# 練習問題2
? geom_jitter
ggplot(data = mpg,
       mapping = aes(
         x = cty,
         y = hwy,
         color = class,
         alpha = 1 / 5
       )) +
  geom_jitter(width = 1) # widthで重なりを調整

# 練習問題3
# geom_jitter
ggplot(data = mpg,
       mapping = aes(
         x = cty,
         y = hwy,
         color = class,
         alpha = 1 / 5
       )) +
  geom_jitter(width = 1)

# geom_count
# データ点の数によって円の大きさが変わる
ggplot(data = mpg,
       mapping = aes(
         x = cty,
         y = hwy,
         color = class,
         alpha = 1 / 5
       )) +
  geom_count()

# 練習問題4
ggplot(data = mpg,
       mapping = aes(x = cty, y = hwy, color = class)) +
  geom_boxplot(position = "dodge")

ggplot(data = mpg,
       mapping = aes(x = cty, y = hwy, color = class)) +
  geom_boxplot(position = "identity")
? geom_boxplot

## 1.9 座標系
# cord_flip() x軸とy軸を交換
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# 地図の表示に便利なパッケージ
install.packages("maps")
library(maps)
library(tidyverse)

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

# 大きさが適切に調整される
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = FALSE,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar

# 縦軸と横軸を入れ替える
bar + coord_flip()

# 円形のグラフができる
bar + coord_polar()

## 練習問題
# 練習問題1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), width = 1)

# 円グラフに変換する
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), width=1)+coord_polar(theta = "y")

# 練習問題3
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_map()

ggplot(nz, aes(long, lat, group = group)) +
  coord_map()

# 練習問題4
ggplot(data = mpg, mapping = aes(x=cty,y=hwy))+
  geom_point()+
  geom_abline()+  # 街中と高速道路の燃費がほとんど変わらないことがわかる
  coord_fixed() 　# どちらも同じ指標（燃費）なのでcoord_fixedを使って間隔をそろえる
?coord_fixed

## 1.10 階層グラフィックス
# ggplot2の特徴：属性を書き足していくことで作図
# 作図テンプレート
ggplot(data = <DATA>)+
  <GEOM_FUNCTION>(
    mapping=aes(<MAPPINGS>),
    stat = <STAT>,
    position = <POSITION>
  )+
  <COORDINATE_FUNCTION>+
  <FACET_FUNCTION>
  
  