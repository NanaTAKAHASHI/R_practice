# 3章 dplyrによるデータ変換 ----
# 3.1
# 3.1,1 準備するもの
library(nycflights13)
library(tidyverse)

# 3.1.2 nycflights13データ
# 米国運輸統計局のデータ
flights

# 3.1.3 dplyrの基本
# 値から観測値を選び出す
filter()

# 行を並び替える
arrange()

# 名前で変数を選ぶ
select()

# 既存の変数で新しい変数をつくる
mutate()

# 多数の値から単一の要約量をつくる
summarise()

# 3.2 filter()で行にフィルタをかける

# filter を使って特定のデータを抽出する
filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

# 3.2.1 比較
# = が１つなのでエラーが返ってくる
# filter(flights, month = 1)
sqrt(2)^2 == 1
1/49*49 == 1

# 明らかに無限個の桁数は格納できないので注意
# 格納する場合は近似値として格納する
near(sqrt(2)^2, 2)
near(1/49*49, 1)

# 論理演算子
filter(flights, month == 11 | month == 12)

# 該当する条件が複数ある時は %in% をつかう(==が一回ですむ)
nov_dec <- filter(flights, month %in% c(11,12))

# 3.2.3 欠損値
# NAを論理演算子中に含めるとNAが返される
NA > 5
10 == NA
NA +10
NA / 2

NA == NA

# 値が欠損値であるかを調べる
x <- NA
is.na(x)

# filter()は条件が真の行のみを返す
# FALSE, NAを含める場合は明示的に要求
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x)| x > 1)

# 3.2練習問題----
# 練習問題１
# a.到着時間が2時間遅れ
# arr_delayの単位は分ぽい
str(flights)
(filter(flights, arr_delay > 119 & arr_delay < 121))

# b.ヒューストンへのフライト
(filter(flights, dest %in% c("IAH", "HOU") ))

# c.アメリカの航空会社のフライト
(filter(flights, carrier %in% c("UA", "AA", "DA")))

# d.7-9月のフライト
(filter(flights, month > 6 & month < 10))

# e. 到着が２時間以上遅れたが出発が遅れなかったフライト
df_e <- dplyr::mutate(flights, delay_exe = arr_time - sched_arr_time) # 到着予定時間と実際の到着時間の差をもとめる

(filter(df_e, delay_exe > 120 & dep_delay <= 0))

# f. 遅延は1時間を超えたが運航では30分以上取り返したフライト
(filter(df_e, delay_exe > 60 & air_time > 30))

# g. 深夜0字から午前6時までのフライト
str(flights)
(filter(flights, hour >= 0 & hour <= 6))

# 練習問題2
# between()を使った書き換え
# 値に範囲があるときに演算子と&でつながなくてもよい
# より直感的にわかりやすい気がする
# d, gを書き換える
(filter(flights,  between(month, 7, 9)))
(filter(flights, between(hour, 0, 6)))
?between

# 練習問題3
# dep_timeの欠損値の数
length(is.na(flights$dep_time))

# データ上でも確認
(filter(flights, is.na(dep_time)))

# ほかの列にもNAはあるか？
# データフレーム内のデータについてNAを含まない行を返す
# complete.cases()関数
# NAを含む行を見たいので!をつける
flights[!complete.cases(flights),]

# 練習問題4
NA^0
NA | TRUE
FALSE & NA

# 3.3 arrange()で行を配置する
# データのソートを行う
# 複数の引数をとると順番に前列の順序付けを反映してソート
arrange(flights, year, month, day)

# 降順はdesc()
arrange(flights, desc(arr_delay))

# 欠損値はソートした最後にくる
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

# 3.3 練習問題----
# 練習問題1
arrange(flights, desc(is.na(dep_time)), dep_time)

# 練習問題2
# 遅延が最も大きかった便
arrange(flights, desc(dep_delay))

# 一番朝時早い便
arrange(flights, air_time)

# 練習問題3
# 練習問題4
arrange(flights, desc(distance))

# 3.4 select()で列を選ぶ
# 列を名前でえらぶ
select(flights, year, month, day)

# yearからdayの間にある列をすべてえらぶ
select(flights, year:day)

# yearからdayの間の列以外すべてを選択
select(flights, -(year:day))

# selectで使えるヘルパー関数
# abcで始まる名前にマッチする
# starts_with("abc") 

# ends_with("xyz")
# xyzで終わる文字列
# contains("ijk")
# ijkを含む文字列
# matches("(.)\\1")
# 正規表現

# num_range("x", 1:3)
# X1~X3にマッチする

# 変数名を変更する
# 変更されなかった変数は
rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())

# 3.4 練習問題----
# 練習問題1
(select(flights, dep_time, dep_delay, arr_time, arr_delay))

# 練習問題2
# ２回繰り返すとその変数のみが選択される
(select(flights, dep_time, dep_time))

# 練習問題3
?one_of
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
one_of(flights, vars)

# 練習問題4
select(flights, contains("TIME"))

# 3.5 mutate()で新しい変数を追加する
flights_sml <- select(flights, year:day, ends_with("delay"),
                      distance, air_time)
mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain /hours)

# 新たな変数を保持するだけであればtranmute()
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

# 3.5.1 有用な作成関数
# モジュラー演算
transmute(flights, dep_time,
          hour = dep_time %% 100,
          minute = dep_time %% 100)

# オフセット
(x <- 1:10)

# 系列を１つ後ろにずらす
lag(x)

# 系列を１つ前にずらす
lead(x)

# 累積および回転和
# 累積和
cumsum(x)
# 移動平均
cummean(x)

# ランク付け
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)

# 値を大きい順にランク付け
min_rank(desc(y))

# その他のランク付け関数
row_number(y)
dense_rank(y)
?percent_rank
percent_rank(y) 
?cume_dist
cume_dist(y)

# 3.5 練習問題----
# 練習問題1
str(flights)
# モジュラー演算を使う
transmute(flights,
          dep_time_minutes = (dep_time %% 100) + ((dep_time %/% 100) * 60)
)

# 練習問題2
# arr_time とdep_timeの差分を作ってから，air_timeと比較
transmute(flights, 
          air_time,
          diff_arr_dep_time = arr_time - dep_time,
          diff_values = air_time - diff_arr_dep_time)

# 差分が0にならない・・・

# 練習問題3
# dep_delay は dep_timeとsched_dep_time(到着予定時刻)の差分と等しいはず
transmute(flights,
          dep_delay,
          confirm_dep_delay = dep_time - sched_dep_time)
# また全然値が合わないのだが・・・

# プロットして確認する
dat_exp3 <- mutate(flights,
                   dep_delay,
                   confirm_dep_delay = dep_time - sched_dep_time)

ggplot(dat_exp3, aes(x = dep_delay, y = confirm_dep_delay))

# 練習問題4
dat_exp4 <- mutate(flights,
       dep_delay_rank = min_rank(-dep_delay)) %>%
        arrange(dep_delay_rank) 

# 練習問題5
1:3
1:10
1:3+1:10

# 3.6 summarize()によるグループごとの集計
# データフレームを要約して1行にする
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))

# group_by と合わせて使うとカテゴリごとに統計量がでる
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

# 3.6.1 パイプで複数演算を結合する
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean (arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dest != "HNL")

# 遅延と飛行距離の関係を調べる
ggplot(data = delay, mapping = aes(x = dist, y = delay))+
        geom_point(aes(size = count), alpha = 1/3)+
        geom_smooth(se = FALSE)

# 複数の処理を格納しなおさなくても一括で実行できる
delays <- flights %>%
        group_by(dest) %>%
        summarize(
                count = n(),
                dist = mean(distance, na.rm = TRUE),
                delay = mean(arr_delay, na.rm = TRUE))%>%
        filter(count > 20, dest != "HNL")
                
# 3.6.2 欠損値
# 出力に多数の欠損値が含まれてしまう
flights %>%
        group_by(year, month, day) %>%
        summarize(mean = mean(dep_delay))

# na.rm = TRUEにすると欠損値を除いて計算してくれる
flights %>%
        group_by(year, month, day) %>%
        summarize(mean = mean(dep_delay, na.rm = TRUE))

# 元のデータセットからキャンセルされたフライトを取り除く
not_cancelled <- flights %>%
        filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
        group_by(year, month, day)%>%
        summarize(mean = mean(dep_delay))

# 3.6.3 カウント
# 平均遅延が一番大きい飛行機を探す
delays <- not_cancelled %>%
        group_by(tailnum) %>%
        summarize(
                delay = mean(arr_delay)
        )

ggplot(data = delays, mapping = aes(x = delay))+
        geom_freqpoly(binwidth = 10)


delays <- not_cancelled %>%
        group_by(tailnum) %>%
        summarize(
                delay = mean(arr_delay, na.rm = TRUE),
                n = n()
        )

# 便数と平均遅延の関係をプロット
ggplot(data = delays, mapping = aes(x = n, y = delay ))+
        geom_point(alpha = 1/10)

# 便数が少ないほど平均遅延が少ないことがわかる

delays %>%
        filter(n > 25) %>%
        ggplot(mapping = aes(x = n, y = delay))+
        geom_point(alpha = 1/10)

# スキル（ba）とヒットの機会（ab）との関連をみる
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
        group_by(playerID)%>%
        summarize(
                ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
                ab = sum(AB, na.rm = TRUE)
        )

# 実際にプロットを作って確認する
batters %>%
        filter(ab > 100) %>%
        ggplot(mapping =  aes(x = ab, y = ba))+
        geom_point() +
        geom_smooth(se = FALSE)

# スキルとヒットの機会の間には正の相関関係があることがわかる

# 成績ごとに選手を並び替える
batters %>%
        arrange(desc(ba))

# 3.6.4 便利な要約関数
not_cancelled %>%
        group_by(year, month, day) %>%
        summarize(
                # average delay:
                avg_delay1 = mean(arr_delay),
                # average positive delay :
                avg_delay2 = mean(arr_delay)[arr_delay > 0]
        )

# 散らばりの代表値
# なぜ目的値によって距離の変動が大きく異なるのか
not_cancelled %>%
        group_by(dest) %>%
        summarize(distance_sd = sd(distance)) %>%
        arrange(desc(distance_sd))

# ランクの代表値
# 四分位範囲を出す
not_cancelled %>%
        group_by(year, month, day) %>%
        summarize(
                first = min(dep_time),
                last = max(dep_time)
        )

# 位置の代表値
not_cancelled %>%
        group_by(year, month, day) %>%
        summarize(
                first_dep = first(dep_time),
                last_dep = last(dep_time)
        )

# dep_timeが早い順にソートした後にfilterで全選択する
not_cancelled %>%
        group_by(year, month, day) %>%
        mutate( r = min_rank(desc(dep_time))) %>%
        filter( r %in% range(r))


# カウント
# 航空会社の乗り入れが最も多い目的地
# n_distinct()含まれる要素の数を求める
not_cancelled %>%
        group_by(dest) %>%
        summarize(carriers = n_distinct(carrier)) %>%
        arrange(desc(carriers))

not_cancelled %>%
        count(dest)

# カウントのヘルパー関数
# wt = で指定した変数の総和をもとめる
not_cancelled %>%
        count(tailnum, wt = distance)

# 論理値のカウントと割合
# 数値関数を使うとTRUE == 1, FALSE == 0に変換されて返される
# 午前5自前より何便出発するか？
not_cancelled %>%
        group_by(year, month, day) %>%
        summarize(n_early = sum(dep_time < 500))

# 1時間以上遅延した便の割合
not_cancelled %>%
        group_by(year, month, day) %>%
        summarize(hour_perc = mean(arr_delay > 60))

# 3.6.5 複数の変数によるグループ化
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))

(per_month <- summarize(per_day, flights = sum(flights)))

(per_year <- summarize(per_month, flights = sum(flights)))

# 3.6.6 グループ解除
daily %>%
        ungroup() %>%
        summarize(flights = n())

# 3.6 練習問題----
# 練習問題1
# 練習問題2
not_cancelled <- flights %>% 
        filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest)

not_cancelled %>% count(tailnum, wt = distance)

# 練習問題3
dat_cancelled <- flights %>%
        filter(is.na(dep_delay) | is.na(arr_delay))
 
# or でdep_delayを入れてしまうとダブルカウントになってしまう
# キャンセル便でそもそも出発前にキャンセルがかかった便は除く
# ただ単に arr_delay()でデータを選択する

dat_cancelled <- flights %>%
        filter(is.na(arr_delay))

# 練習問題4
cancelled_by_day <- flights %>%
        group_by(day) %>%
        filter(is.na(arr_delay))


# 練習問題5
# 練習問題6
# 練習問題7

# 3.7 グループごとの変更（とフィルタ）
# 各グループで成績が最下位のメンバー
flights_sml <- flights %>%
        group_by(year, month,day) %>%
        filter(rank(desc(arr_delay)) < 10)

# 個数が閾値よりも大きいグループをすべて探す
popular_dests <- flights %>%
        group_by(dest) %>%
        filter(n() > 365)

# グループごとに計算を標準化する
popular_dests %>%
        filter(arr_delay > 0) %>%
        mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
        select(year:day, dest, arr_delay, prop_delay)

# 3.7 練習問題----
# 練習問題2
not_cancelled %>%
        group_by(tailnum) %>%
        summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(rank(desc(arr_delay)) <= 1)

# 練習問題3
# 遅延が一番少ない時間帯を探す
not_cancelled %>%
        group_by(hour) %>%
        summarise( mean_arr_delay = mean(arr_delay, na.rm=T) ) %>%
        ungroup() %>%
        arrange(mean_arr_delay)

# 練習問題4
# 目的地ごとに総遅延時間を分単位で計算
not_cancelled %>%  
        group_by(dest) %>%
        summarise( sum_arr_delay = sum(arr_delay) )

# 練習問題5
flights %>%
        group_by(year, month, day) %>%
        filter(!is.na(dep_delay)) %>%
        mutate(lag_delay = lag(dep_delay)) %>%
        filter(!is.na(lag_delay)) %>%
        ggplot(aes(x = dep_delay, y = lag_delay)) +
        geom_point() +
        geom_smooth()

# 練習問題6
# 問題文の日本語やば気味
flights %>%
        filter(!is.na(air_time)) %>%
        group_by(dest) %>%
        mutate(med_air_time = median(air_time),
                o_vs_e = (air_time - med_air_time) / med_air_time,
                air_time_diff = air_time - min(air_time) ) %>%
        arrange(desc(air_time_diff)) %>%
        select(air_time, o_vs_e, air_time_diff, dep_time, sched_dep_time, arr_time, sched_arr_time) %>%

# 練習問題7
not_cancelled %>% 
        group_by(dest, carrier) %>%
        count(carrier) %>%
        filter(n >= 2) %>%
        group_by(carrier) %>%
        count(sort = TRUE)


