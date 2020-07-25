# 第4章 母集団と標本

# 4.3 点推定
身長 <- c(165.2, 175.9, 161.7, 174.2, 172.1, 163.3, 170.9, 170.6, 168.4, 171.3)
mean(身長)
var(身長)

# 4.4 推定値がどれくらい当てはまるのかを調べる方法
set.seed(1)
サイコロ6 <- ceiling(runif(n=6, min=0, max=6))  # runif：一様分布に従うデータを発生
table(サイコロ6)
set.seed(1)
サイコロ600万 <- ceiling(runif(n=6 * 10^6, min=0, max=6))  # runif：一様分布に従うデータを発生
table(サイコロ600万)
barplot(c(2/3, 1/3), names.arg=c("男性", "女性"))
# 4.4.5 正規分布
curve(dnorm(x, mean=0, sd=1), from=-4, to=4)
curve(dnorm(x, mean=1, sd=1), add=TRUE)
curve(dnorm(x, mean=0, sd=2), add=TRUE)
dnorm(2, mean=0, sd=1)  # 平均0，標準偏差1の確率密度関数のx=2での値
# 4.4.7 正規母集団から単純無作為抽出を行う
標本 <- rnorm(n=5, mean=50, sd=10)
標本
hist(標本)
大標本 <- rnorm(n=10000, mean=50, sd=10)
大標本
hist(大標本)

# 4.5 標本分布
# 4.5.3 正規母集団の母平均の推定
標本 <- rnorm(n=10, mean=50, sd=10)
標本
mean(標本)
標本 <- rnorm(n=10, mean=50, sd=10)
標本
mean(標本)
標本 <- rnorm(n=10, mean=50, sd=10)
標本
mean(標本)
# 4.5.4 標本分布を求める
標本平均 <- numeric(length=10000)  # 推定値を格納する場所を予約
for(i in 1:10000){  # 中括弧に囲まれた処理を10000回繰り返す（モンテカルロシミュレーション），閉区間（10000含む）
	標本 <- rnorm(n=10, mean=50, sd=10)  # N(50, 10^2)からn=10の標本を抽出
	標本平均[i] <- mean(標本)  # 標本平均を計算する
}
hist(標本平均)
誤差絶対値5以下 <- ifelse(abs(標本平均 - 50) <= 5, 1, 0)
table(誤差絶対値5以下)
mean(標本平均)
var(標本平均)
curve(dnorm(x, mean=50, sd=sqrt(10)), add=TRUE)
dnorm(50, mean=50, sd=sqrt(10))
hist(標本平均, freq=FALSE)  # ヒストグラム全体の面積が1になるように調整
curve(dnorm(x, mean=50, sd=sqrt(10)), add=TRUE)
標本平均 <- numeric(length=10000)  # 推定値を格納する場所を予約
# 4.5.6 標準誤差
for(i in 1:10000){  # 中括弧に囲まれた処理を10000回繰り返す（モンテカルロシミュレーション），閉区間（10000含む）
	標本 <- rnorm(n=100, mean=50, sd=10)  # N(50, 10^2)からn=100の標本を抽出
	標本平均[i] <- mean(標本)  # 標本平均を計算する
}
var(標本平均)
hist(標本平均)

# 4.6 標本平均以外の標本分布
# 4.6.1 標本分散と不偏分散の標本分布
標本分散 <- numeric(length=10000)
不偏分散 <- numeric(length=10000)
for(i in 1:10000){
	標本 <- rnorm(n=10, mean=50, sd=10)
	標本分散[i] <- mean((標本 - mean(標本))^2)
	不偏分散[i] <- var(標本)
}
mean(標本分散)
mean(不偏分散)  # 一般に，こちらの方が母分散の値に近くなる
sd(標本分散)
sd(不偏分散)
hist(標本分散, breaks=seq(0, 500, 10))
hist(不偏分散, breaks=seq(0, 500, 10))
標本分散誤差100以上 <- ifelse(標本分散>=200, 1, 0)
不偏分散誤差100以上 <- ifelse(不偏分散>=200, 1, 0)
table(標本分散誤差100以上)
table(不偏分散誤差100以上)
mean(sqrt(不偏分散))  # 不偏推定量ではない，「不偏標準偏差」とは言わない
# 4.6.2 中央値の標本分布
標本平均 <- numeric(length=10000)
標本中央値 <- numeric(length=10000)
for(i in 1:10000){
	標本 <- rnorm(n=10, mean=50, sd=10)
	標本平均[i] <- mean(標本)  # 平均を計算
	標本中央値[i] <- median(標本)  # 中央値を計算
}
mean(標本平均)
mean(標本中央値)
sd(標本平均)
sd(標本中央値)  # 一般に，こっちのほうがばらつきが大きい
hist(標本平均)
hist(標本中央値)

# 練習問題
# (1)

# (2)
