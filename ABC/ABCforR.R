
############################################################
#                R 初級ユーザーのための ABC                 #
#      石田基広・石田和枝 訳 シュプリンガー・ジャパン        #
#               ishida.motohiro @ gmail.com                #
#                                                          #
############################################################
#
#           これは邦訳版読者用のスクリプトです．             #
#     原著に掲載されたコードとは異なる場合があります．        #
############################################################

#   
#    A Beginner's Guide to R (2009)
#    Zuur, Ieno, Meesters.    Springer
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



############################################################
#                 コードに問題がある場合は                  #
#               ishida.motohiro @ gmail.com                #
#                 までお問い合わせください                  #
############################################################

setwd("C:/RBook/")# 

ISIT <- read.table("ISIT.txt",header=TRUE)
library(lattice) # lattice ライブラリをロードする
#
# プロット行うコード
# データを SampleDepth の関数として描画する
# その際，観測地点ごとにパネルを利用する
# 色彩は黒を指定 (col=1) し， x 軸と y 軸のラベル
# (xlab, ylab) を指定
# 描画される矩形の背景色は白
# 描画領域には観測地点のラベルを加える


xyplot(Sources ~ SampleDepth|factor(Station), data = ISIT,
  xlab ="標本の深度 (Depth)",  ylab = "密度 (Sources)",
  strip = function(bg = 'white', ...)
  strip.default(bg = 'white', ...),
  panel = function(x, y) {
    # グリッド線を加え
    # プロットが分かりにくくなるのを避けるため
    # データを（黒い）線分でつないでいる
        panel.grid(h=-1, v= 2)
        I1<-order(x)
        llines(x[I1], y[I1],col=1)})  



setwd("C:/RBook/")# setwd("RBook/")
Data <- read.table("Antarcticbirds.txt", header = T)
attach(Data)
install.packages("rgdal")

library(rgdal)
library(pixmap)
penguin2 <- read.pnm("penguin5.ppm")
plot(Year,LayingEP,type="n",xlab="Year",ylab="Laying day")
addlogo(penguin2, c(1950,2005), c(38,52))
lines(Year,LayingEP,lwd=4,col="white")





boxplot(count ~ spray, data = InsectSprays,  col = "lightgray")



########################################################
#
#                          第 2 章
#
########################################################



#2.1.2 
Wingcrd <- c(59, 55, 53.5, 55, 52.5, 57.5, 53, 55)
Wingcrd[1]

S.win <- sum(Wingcrd)
S.win


Tarsus <- c(22.3, 19.7, 20.8, 20.3, 20.8, 21.5, 20.6, 21.5)
Head <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8, 32.5, NA)
Wt <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6, 15.6, 15.7)

sum(Head, na.rm = TRUE)


# 2.1.3
BirdData <- c(Wingcrd, Tarsus, Head, Wt)
Id <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
     2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4)


rep(c(1, 2, 3, 4), each = 8)
rep(1 : 4, each = 8)
a <- seq(from = 1, to = 4, by = 1)
a

a <- seq(from = 1, to = 4, by = 1)
rep(a, each = 8)



VarNames <- c("Wingcrd", "Tarsus", "Head", "Wt")
Id <- rep(VarNames, each = 8)


Z <- cbind(Wingcrd, Tarsus, Head, Wt)
n <- dim(Z)
n
n <- dim(Z)[1]
n

W <- vector(length = 8)
W[1] <- 59
W[2] <- 55
W[3] <- 53.5
W[4] <- 55
W[5] <- 52.5
W[6] <- 57.5
W[7] <- 53
W[8] <- 55

# 2.1.5

Dmat <- matrix(nrow = 8, ncol = 4)
Dmat

Dmat[, 1] <- c(59, 55, 53.5, 55, 52.5, 57.5, 53, 55)
Dmat[, 2] <- c(22.3, 19.7, 20.8, 20.3, 20.8, 21.5,
                 20.6, 21.5)
Dmat[, 3] <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8,
                 32.5, NA)
Dmat[, 4] <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6,
                 15.6, 15.7)

Dmat

colnames(Dmat) <- c("Wingcrd", "Tarsus", "Head", "Wt")
Dmat

Dmat2 <- as.matrix(cbind(Wingcrd,Tarsus,Head, Wt))


# 2.1.6

Dfrm <- data.frame(WC = Wingcrd, TS = Tarsus,
                HD = Head, W = Wt)
Dfrm

# 2.1.7
 x1 <- c(1, 2, 3)
 x2 <- c("a", "b", "c", "d")
 x3 <- 3
 x4 <- matrix(nrow = 2, ncol = 2)
 x4[, 1] <- c(1, 2)
 x4[, 2] <- c( 3, 4)
 Y <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)


M <- lm(WC ~ Wt, data = Dfrm)


AllData <- list(BirdData = BirdData, Id = Id, Z = Z,
                VarNames = VarNames)

AllData <- list(BirdData, Id, Z, VarNames)


# 2.2.1.3

setwd("C:\\RBook\\")
Squid <- read.table(file = "squidGSI.txt",header = TRUE)


Sq1 <- read.table(file = "C:/RBook/squid1.txt", header = T)
Sq2 <- read.table(file = "C:/RBook/squid2.txt", header = T)

ZMerged <- merge(Sq1, Sq2, by = "Sample")

write.table(ZMerged, file = "C:/RBook/MergedSquid.txt",
            sep = " ",quote=FALSE,append=FALSE,
            na="NA")


########################################################
#
#                       第 3 章
#
########################################################


# 3.1 データの読み込み

setwd("C:/RBook")
Squid <- read.table(file = "squidGSI.txt",
                  header = TRUE)

# 変数名の確認
names(Squid)
#"Sample"   "Year"     "Month"    "Location" "Sex"      "GSI"

# 3.1.1 
str(Squid)

    # 小数点の指定を間違った場合
    setwd("C:/RBook")
    Squid2 <- read.table(file = "squidGSI.txt",
                  dec = ",", header = TRUE)
    str(Squid2)
    boxplot(Squid2$GSI)


# 3.1.2
M1 <- lm(GSI ~ factor(Location) + factor(Year), data = Squid)
mean(GSI, data = Squid)
boxplot(GSI ~ factor(Location), data = Squid)
boxplot(GSI, data = Squid)

# 3.1.3
Squid$GSI
mean(Squid$GSI)

# 3.1.4 
attach(Squid)
GSI
boxplot(GSI)
mean(GSI)

detach(Squid)



##### 3.2 

Squid$Sex
Se1 <- Squid$Sex == 1
SquidF <- Squid[Se1, ]

SquidM <- Squid[Squid$Sex == 2, ]

SquidF.OR.1 <- Squid[Squid$Sex == 1 &
                   Squid$Location == 1,]

SquidF <- Squid[Squid$Sex == 1, ]
SquidF1 <- SquidF[Squid$Location == 1, ]


# 3.2.1
Ord1 <- order(Squid$Month)
Squid[Ord1, ]
Squid

Squid$fLocation <- factor(Squid$Location)
Squid$fSex <- factor(Squid$Sex,labels=c("M","F"))
Squid$fSex

names(Squid)

Squid$fLocation <- factor(Squid$Location,
                    levels = c(2, 3, 1, 4))
Squid$fLocation



# 3.3

setwd("C:/RBook")
Sq1 <- read.table(file = "squid1.txt", header = TRUE)
Sq2 <- read.table(file = "squid2.txt", header = TRUE)
SquidMerged <- merge(Sq1, Sq2, by = "Sample")


SquidMerged <- merge(Sq1, Sq2, by = "Sample", all= TRUE)
SquidMerged[1:14,]

Squid$fSex <- factor(Squid$Sex, labels = c("M", "F"))
Squid$fLocation <- factor(Squid$Location)
str(Squid)

Squid[Squid$Location == 1 & Squid$Year == 1, ]


# 3.4

SquidM <- Squid[Squid$Sex == 1, ]
write.table(SquidM,
     file = "MaleSquid.txt",
     sep=" ", quote = FALSE, append = FALSE, na = "NA")
     
write.table(SquidM,
     file = "MaleSquid.txt",
     sep=",", quote = TRUE, append = FALSE, na = "NA")

write.table(SquidM,
     file = "MaleSquid.txt",
     sep=" ", quote = TRUE, append = TRUE, na = "NA")

# 3.5
str(Squid)
Squid$fLocation <- factor(Squid$Location)
Squid$fSex <- factor(Squid$Sex)

Squid$fSex <- factor(Squid$Sex, levels = c(1, 2),
labels = c("M", "F"))
Squid$fSex

boxplot(GSI ~ fSex, data = Squid) #Result not shown
 M1 <- lm(GSI ~ fSex + fLocation, data = Squid)
summary(M1)
M2 <- lm(GSI ~ factor(Sex) + factor(Location),
data = Squid)
summary(M2)

Squid$fLocation

boxplot(GSI ~ fLocation, data = Squid)


SquidM <- Squid[Squid$Sex == 1, ]

SquidM <- Squid[Squid$fSex == "1", ]


Squid$fSex <- factor(Squid$Sex, labels = c("M", "F"))
Squid$fLocation <- factor(Squid$Location)
str(Squid)


########################################################
#
#                       第 4 章
#
########################################################




setwd("c:/RBook/")
Veg <- read.table(file="Vegetation2.txt",
    header =TRUE)
names(Veg)
str(Veg)

# 4.4.1

m <- mean(Veg$R)
m1 <- mean(Veg$R[Veg$Transect == 1])
m2 <- mean(Veg$R[Veg$Transect == 2])
m3 <- mean(Veg$R[Veg$Transect == 3])
m4 <- mean(Veg$R[Veg$Transect == 4])
m5 <- mean(Veg$R[Veg$Transect == 5])
m6 <- mean(Veg$R[Veg$Transect == 6])
m7 <- mean(Veg$R[Veg$Transect == 7])
m8 <- mean(Veg$R[Veg$Transect == 8])
c(m, m1, m2, m3, m4, m4, m5, m6, m7, m8)


# 4.1.2

Me <- tapply(Veg$R, Veg$Transect, mean)
Sd <- tapply(Veg$R, Veg$Transect, sd)
Le <- tapply(Veg$R, Veg$Transect, length)
cbind(Me, Sd, Le)


#4.2

sapply(Veg[, 5:10], FUN = mean)


sapply(cbind(Veg$R, Veg$ROCK, Veg$LITTER, Veg$ML,
               Veg$BARESOIL), FUN = mean)

# 4.3

Z <-cbind(Veg$R, Veg$ROCK, Veg$LITTER, Veg$ML)
colnames(Z) <- c("R","ROCK","LITTER","ML")
summary(Z)

names(Veg)

# [1] "TransectName" "Samples"      "Transect"
# [4] "Time"         "R"            "ROCK"
# [7] "LITTER"       "ML"           "BARESOIL"
#[10] "FallPrec"     "SprPrec"      "SumPrec"
#[13] "WinPrec"      "FallTmax"     "SprTmax"
#[16] "SumTmax"      "WinTmax"      "FallTmin"
#[19] "SprTmin"      "SumTmin"      "WinTmin"
#[22] "PCTSAND"      "PCTSILT"      "PCTOrgC"

summary(Veg[,c("R","ROCK","LITTER","ML")])
summary(Veg[,c(5,6,7,8)])



# 4.4

setwd("c:/RBook/")
Deer <- read.table(file="Deer.txt", header =TRUE)
names(Deer)
str(Deer)

table(Deer$Farm)
table(Deer$Sex, Deer$Year)




########################################################
#
#                       第 5 章
#
########################################################


setwd("c:/RBook/")
Veg <- read.table(file="Vegetation2.txt",     header =TRUE)


plot(Veg$BARESOIL, Veg$R)

plot(Veg$R, Veg$BARESOIL)

plot(x = Veg$BARESOIL, y = Veg$R)

plot(BARESOIL, R, data = Veg)

plot(R ~ BARESOIL, data = Veg)

## x=1:25
## y=1:5
## z=rep(1,5)

## par(mar = c(2,0,1,2))
## plot(z, y, pch = x[1:5], axes = FALSE, xlim = c(0,5), xlab = "", ylab = "")
## text(z-0.3, y, x[1:5], cex = 0.8)

## z1 <- rep(2,5)
## points(z1, y, pch = x[6:10])
## text(z1-0.3, y, x[6:10], cex = 0.8)

## z2 <- rep(3,5)
## points(z2, y, pch = x[11:15])
## text(z2-0.3, y, x[11:15], cex = 0.8)

## z3 <- rep(4,5)
## points(z3, y, pch = x[16:20])
## text(z3-0.3, y, x[16:20], cex = 0.8)

## z4 <- rep(5,5)
## points(z4, y, pch = x[21:25])
## text(z4-0.3, y, x[21:25], cex = 0.8)

xlim <- c(min(Veg$BARESOIL), max(Veg$BARESOIL))
xlim <- c(min(Veg$BARESOIL, na.rm = TRUE),
max(Veg$BARESOIL, na.rm = TRUE))
#


# 5.2

#####シンボル一覧
ncols <- 5
nrows <- 5
for (i in 1:ncols) {
  for (j in 1:nrows) {
    x <- grid::unit(j/(ncols+1), "npc")
    y <-  grid::unit(i/(nrows + 1), "npc")
    pch <- (j - 1)*nrows + i #- 1
    grid.points( x +  grid::unit(3, "mm"), y, 
      pch=pch)
    grid.text(pch,  x -  grid::unit(3, "mm"), y)
  }
}
### 



par(mfrow = c(2,2))

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",
     xlim = c(0, 45), ylim = c(4, 19)) 
# text(1, 18, "A", cex = 1.5)
Veg$Transect
 
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",
     xlim = c(0, 45), ylim = c(4, 19), pch = 16)
# text(1, 18, "B", cex = 1.5)
     

plot(x = Veg$BARESOIL, y = Veg$R,
     ## xlab = "Exposed soil",
     ## ylab = "Species richness", main = "Scatter plot",
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",     
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Transect )
# text(1, 18, "C", cex = 1.5)



Veg$Time2 <- Veg$Time


Veg$Time2[Veg$Time <= 1974] <- 1
Veg$Time2[Veg$Time > 1974] <- 16

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",         
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time2)
# text(1, 18, "D", cex = 1.5)


plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",         
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time)
warnings()


# 5.2.2 

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",
     xlim = c(0, 45), ylim = c(4, 19),
     col = 2)

x <- 1:8
plot(x, col = x)


###############
Veg$Time2 <- Veg$Time
Veg$Time2[Veg$Time <= 1974] <- 15
Veg$Time2[Veg$Time > 1974] <- 16

Veg$Col2 <- Veg$Time
Veg$Col2[Veg$Time <= 1974] <- 1
Veg$Col2[Veg$Time > 1974] <- 2

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",     
     xlim = c(0, 45), ylim = c(4, 19),
     pch = Veg$Time2, #col = c("black", "grey") )
      col = Veg$Col2)
#  text(1, 18, "A", cex = 1.5)
    

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",     
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 16, cex = 1.5)
# text(1, 18, "B", cex = 1.5)

Veg$Cex2 <- Veg$Time
Veg$Cex2[Veg$Time == 2002] <- 2
Veg$Cex2[Veg$Time != 2002] <- 1



# 5.3

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",     
     xlim = c(0, 45), ylim = c(4, 19),
     pch = 16, cex = Veg$Cex2 )
# text(1, 18, "C", cex = 1.5)




M.Loess <- loess(R ~ BARESOIL, data = Veg)
M.Loess
Fit <- fitted(M.Loess)


nf <- layout(rbind(c(1,1,2,2), c(0,3,3,0)),
            heights = c(1, 1) , respect= F )


plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌 BARESOIL",
     ylab = "種数 R", main = "散布図",       
     xlim = c(0, 45), ylim = c(4, 19) )
text(1, 18, "A", cex = 1.5)
lines(Veg$BARESOIL, Fit)




Ord1 <- order(Veg$BARESOIL)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "露出土壌",
     ylab = "種数", main = "散布図",       
     xlim = c(0, 45), ylim = c(4, 19) )
text(1, 18, "B", cex = 1.5)

lines(Veg$BARESOIL[Ord1], Fit[Ord1], lwd = 3, lty = 2)




M1 <- lm(R~BARESOIL * factor(Transect), data = Veg)
summary(M1)

x <- factor(c("A", "B","C"))#, order = 1:3)
x <- ordered(lev = x, 1:3)


max(x, order = T)


########################################################
#
#                       第 6 章
#
########################################################


# 6.3

setwd("C:/RBook/")# setwd("RBook/")
Owls <- read.table(file = "Owls.txt", header = TRUE)
names(Owls)
str(Owls)

#[1] "Nest"               "FoodTreatment"
#[3] "SexParent"          "ArrivalTime"
#[5] "SiblingNegotiation" "BroodSize"
#[7] "NegPerChick"


# 6.2.3

unique(Owls$Nest)

# [1] AutavauxTV      Bochet          Champmartin
# [4] ChEsard         Chevroux        CorcellesFavres
# [7] Etrabloz        Forel           Franex
#[10] GDLV            Gletterens      Henniez
#[13] Jeuss           LesPlanches     Lucens
#[16] Lully           Marnand         Moutet
#[19] Murist          Oleyes          Payerne
#[22] Rueyes          Seiry           SEvaz
#[25] StAubin         Trey            Yvonnand
#27 Levels: AutavauxTV Bochet Champmartin ... Yvonnand


Owls.ATV <- Owls[Owls$Nest == "AutavauxTV",]


plot(x = Owls.ATV$ArrivalTime, y = Owls.ATV$NegPerChick,
     xlab = "到着時間", ylab = "餌請い行動",
     main =  "AutavauxTV")


Owls.Bot  <- Owls[Owls$Nest == "Bochet",]

plot(x = Owls.Bot$ArrivalTime, y = Owls.Bot$NegPerChick,
     xlab = "到着時間", ylab = "餌請い行動",
     main =  "Bochet") 


Nest.i <- "Bochet"
Owls.i <- Owls[Owls$Nest == Nest.i, ]
YourFileName <- paste(Nest.i,".jpg",sep="")
jpeg(file=YourFileName)

plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
     xlab = "到着時間", ylab = "餌請い行動",
     main =  Nest.i)


# 6.2.5

paste(Nest.i, ".jpg", sep = "")

setwd("C:/AllGraphs/")

Nest.i <- "Bochet"
Owls.i <- Owls[Owls$Nest == Nest.i, ]
YourFileName <- paste(Nest.i, ".jpg", sep="")

jpeg(file = YourFileName)
  plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
  xlab = "到着時間", main = Nest.i,
  ylab = "餌請い行動")
dev.off()

AllNests <- unique(Owls$Nest)
for (i in 1:27){
   Nest.i <- AllNests[i]
   Owls.i <- Owls[Owls$Nest == Nest.i, ]
   YourFileName <- paste(Nest.i,".jpg",sep="")
   jpeg(file=YourFileName)
   plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
     xlab = "到着時間", ylab = "餌請い行動",
     main =  Nest.i)
   dev.off()
}


# 6.3.1
setwd("c:/RBook/")
Veg <- read.table(file = "Vegetation2.txt",   header =TRUE)
names(Veg)


NAPerVariable <- function(X1) {
  D1 <- is.na(X1)
  colSums(D1)
}
NAPerVariable(Veg[,5:24])

# 6.3.2
H <- NAPerVariable(Veg[ , 4 : 24])


## MyPlot <- function(N.i, Z){
##    Z.i <- Z[Z$Nest == N.i, ]
##    YFName <- paste(N.i,".jpg",sep="")
##    jpeg(file=YFName)
##    plot(x = Z.i$ArrivalTime, y = Z.i$NegPerChick,
##      xlab = "到着時間", ylab = "餌請い行動",
##      main =  N.i)
##    dev.off()
## }

## for (i in 1:27){
##    Nest.i <- AllNests[i]
##    MyPlot(Nest.i,Owls)
## }


#### 6.3.3

setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt",
                    header = TRUE)

names(Parasite)

NAPerVariable<-function(X1) {
  D <- is.na(X1)
  colSums(D)
}

NAPerVariable(Parasite)

ZerosPerVariable<-function(X1) {
  D1 <- X1 == 0
  colSums(D1)
}

ZerosPerVariable(Parasite)

ZerosPerVariable<-function(X1) {
  D1 <- X1 == 0
  colSums(D1, na.rm = TRUE)
}

ZerosPerVariable(Parasite)

# 6.3.4 
VariableInfo<-function(X1,Choice1) {
  if (Choice1 =="Zeros"){ D1 = X1 == 0 }
  if (Choice1 =="NAs")  { D1 <- is.na(X1)}
  colSums(D1, na.rm = TRUE)
}


VariableInfo(Parasite,"Zeros")
VariableInfo(Parasite,"NAs")
VariableInfo(Parasite,"zeroos")
VariableInfo(Parasite)

#Add default value
VariableInfo<-function(X1,Choice1 = "Zeros") {
  if (Choice1 =="Zeros"){
    D1 <- X1 == 0 }
  if (Choice1 =="NAs")  {
    D1 <- is.na(X1)}
  colSums(D1, na.rm = TRUE)
}

VariableInfo(Parasite)


VariableInfo <- function(X1,Choice1 = "Zeros") {
  switch(Choice1,
       "Zeros" { D1 <- (X1 == 0) }
       "NAs"   { D1 <- is.na(X1)})

  colSums(D1, na.rm = TRUE)
}

VariableInfo(Parasite)


#
VariableInfo<-function(X1, Choice1 = "Zeros") {
  if (Choice1 =="Zeros"){
    D1 <- X1 == 0 }
  if (Choice1 =="NAs")  {
    D1 <- is.na(X1)}
  if (Choice1 != "Zeros" & Choice1 != "Nas") {
    print("入力ミスがあります")
  } else {
  colSums(D1, na.rm = TRUE)}
}

VariableInfo(Parasite,"abracadabra")

ifelse(Choice1 == "Zeros", D1 <- (X1 == 0),
       D1 <- is.na(X1))


# 6.4

setwd("C:/RBook/")
Benthic <- read.table("RIKZ.txt",header=T)
Species <- Benthic[,2:76]
n <- dim(Species)


# 6.4.3 
sum(Species[1, ], na.rm = TRUE)
sum(Species[2, ], na.rm = TRUE)

TA <- vector(length = n[1])
for (i in 1:n[1]){
    TA[i] <- sum(Species[i,], na.rm = TRUE)
  }


TA <- rowSums(Species, na.rm = TRUE)



# 6.4.4

sum(Species[1, ] > 0, na.rm = TRUE)

sum(Species[2, ] > 0, na.rm = TRUE)


Richness <- vector(length = n[1])
for (i in 1:n[1]){
   Richness[i] <- sum(Species[i, ] > 0, na.rm = TRUE)
  }
Richness


# 6.4.5

Richness <- rowSums(Species > 0, na.rm = TRUE)

RS <- rowSums(Species, na.rm = TRUE)
prop <- Species / RS
H <- -rowSums(prop * log10(prop), na.rm = TRUE)
H

## library(vegan)
## H <- diversity(Species)
## H

# 6.4.6 
Choice <- "Richness"

if (Choice == "Richness") {
     rowSums(Species >0 , na.rm = TRUE)}
if (Choice == "Total Abundance") {
      rowSums(Species, na.rm = TRUE) }
if (Choice=="Shannon") {
     RS <- rowSums(Species, na.rm = TRUE)
     prop <- Species / RS
     -rowSums(prop * log10(prop), na.rm = TRUE)}

# 6.4.7 

Index.function <- function(Spec, Choice){
  if (Choice == "Richness") {
     Index <- rowSums(Spec > 0, na.rm = TRUE)
   }
  if (Choice == "Total Abundance") {
      Index <- rowSums(Spec, na.rm = TRUE)
    }
  if (Choice=="Shannon") {
    RS <- rowSums(Species, na.rm = TRUE)
    prop <- Species / RS
    Index <- -rowSums(prop * log10(prop), na.rm = TRUE)
  }
  list(Index = Index, MyChoice = Choice)
}


Index.function(Species, "total abundance")


Index.function <- function(Spec,Choice1){
  if (Choice1 == "Richness") {
    Index <- rowSums(Spec > 0, na.rm = TRUE)
  } else
  if (Choice1 == "Total Abundance") {
    Index <- rowSums(Spec, na.rm = TRUE)
  } else if (Choice1 == "Shannon") {
    RS <- rowSums(Spec, na.rm = TRUE)
    prop <- Spec / RS
    Index <- -rowSums(prop*log(prop),na.rm=TRUE)
  } else {
    print("コードを見直してください")
    Index <- NA
  }
  list(Index = Index, MyChoice = Choice1)
}

Index.function(Species, "total abundance")









########################################################
#
#                       第  7  章
#
########################################################


# 7.1.1

setwd("C:/RBook/")
BFCases <- read.table(file="BirdFluCases.txt", header = TRUE)
                                                                      

names(BFCases)
str(BFCases)


Cases  <- rowSums(BFCases[,2:16])
names(Cases ) <- BFCases[,1]
Cases

# 7.1.2

op <- par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
pie(Cases , main = "通常のパイチャート", col = gray(seq(0.4,1.0,length=6)) )
# text(-1,1,"A",cex = 1.5)
pie(Cases , col = gray(seq(0.4,1.0,length=6)),    clockwise=TRUE, main = "グレースケール")
# text(-1,1,"B",cex = 1.5)
pie(Cases ,  col =  gray(seq(0.4,1.0,length=6)), clockwise = TRUE, main = "虹色を利用")
# text(-1,1,"C",cex = 1.5)
# install.pacakges("plotrix"); library(plotrix)
pie3D(Cases , labels = names(Cases ),  col =  gray(seq(0.4,1.0,length=6)), explode = 0.1,  main = "3D パイチャート", labelcex = 0.6)
# text(-1,1,"D", cex = 1.5)

par(op)

  
# 7.1.2 
BFDeaths <- read.table(file="BirdFluDeaths.txt", header = TRUE)
head(BFCases)
Deaths <- rowSums(BFDeaths[,2:16])
names(Deaths) <- BFDeaths[,1]
Deaths

Counts <- cbind(Cases, Deaths)
Counts


par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
barplot(Cases , main = "鳥インフルエンザ患者数")
# text(1,100, "A", cex = 1.5)
barplot(Counts, names = c("罹患者数", "死者数"))
# text(2,350, "B", cex = 1.5)
barplot(t(Counts), col = gray(c(0.5,1)))
# text(1,170, "C", cex = 1.5)
barplot(t(Counts), beside = TRUE)
# text(18,110, "D", cex = 1.5)


t(Counts)

# 7.2.2
setwd("C:/RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header = TRUE)

Bent.M <- tapply(Benthic$Richness, INDEX=Benthic$Beach, FUN = mean)
Bent.sd <- tapply(Benthic$Richness, INDEX=Benthic$Beach, FUN = sd)
MSD <- cbind(Bent.M, Bent.sd)

barplot(Bent.M)

barplot(Bent.M, xlab = "ビーチ", ylim = c(0, 20), ylab = "種数", col = rainbow(9))



bp <- barplot(Bent.M, xlab = "ビーチ", ylab = "種数",
              col = gray(seq(0.4, 1.0, length = 9)),
              ylim = c(0,20))
# text(1, 18, "A", cex = 1.5)
arrows(bp, Bent.M, bp, Bent.M + Bent.sd, lwd = 1.5, angle=90,length=0.1)
box()
bp


# 7.2.3

Benth.le <- tapply(Benthic$Richness, INDEX = Benthic$Beach, FUN = length)
Bent.se <- Bent.sd / sqrt(Benth.le)


stripchart(Benthic$Richness ~ Benthic$Beach, vert = TRUE,
           pch=1, method = "jitter", jit = 0.05, xlab = "ビーチ",
   	       ylab = "種数")
points(1:9,Bent.M, pch = 16, cex = 1.5)


arrows(1:9, Bent.M,
       1:9, Bent.M + Bent.se, lwd = 1.5,
       angle=90, length=0.1)
arrows(1:9, Bent.M,
       1:9, Bent.M - Bent.se, lwd = 1.5,
       angle=90, length=0.1)
# text(1, 20, "B", cex = 1.5)





# 7.3.1
setwd("C:/RBook/")

Owls <- read.table(file = "Owls.txt", header= TRUE)

#
boxplot(Owls$NegPerChick, main = "餌請い行動")




par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
## Owls$LogNeg <- log10(Owls$NegPerChick + 1)
boxplot(NegPerChick~SexParent, names = c("メス","オス"), data = Owls, label = c("",""))
# text(.6, 8 ,"A")
boxplot(NegPerChick~FoodTreatment, names = c("餌が欠乏","餌が充分"), data = Owls)
# text(.6, 8 ,"B")
boxplot(NegPerChick~SexParent * FoodTreatment, names = c("雌・欠","雄・欠", "雌・充","雄・充"),   data = Owls)
# text(.6, 8 ,"C")
boxplot(NegPerChick~SexParent * FoodTreatment, names = c("雌・欠","雄・欠", "雌・充","雄・充"), data = Owls)
# text(.6, 8 ,"D")



boxplot(NegPerChick~Nest, data = Owls)


par(mar = c(2,2,3,3))
boxplot(NegPerChick~Nest, data = Owls, axes = FALSE, ylim = c (-3.5, 9))
axis(2, at = c(0, 2, 4, 6, 8))
text(x = 1:27, y = -2, labels = levels(Owls$Nest),
     cex = 0.75, srt = 65)


# 7.3.2 
setwd("C:/RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header= TRUE)
Bentic.n <- tapply(Benthic$Richness, Benthic$Beach, FUN =length)
Bentic.n

boxplot(Richness ~ Beach, data = Benthic, col = "grey",
       xlab = "ビーチ", ylab = "種数")

BP.info <- boxplot(Richness ~ Beach, data = Benthic, col = "grey",
       xlab = "ビーチ", ylab = "種数")

BP.mid <- BP.info$stats[2, ] + (BP.info$stats[4,] - BP.info$stats[2,]) / 2
text(1:9, BP.mid, Bentic.n, col = "white", font = 2)
####



# 7.4 Dotplot



setwd("C:/RBook/")
Dear <-read.table("Deer.txt", header = TRUE )


##### par(mfrow=c(1,2))
dotchart(Dear$LCT, xlab="長さ (cm)", ylab = "観測数")

#dotchart(Dear$LCT, groups = factor(Dear$Sex))
Isna <- is.na(Dear$Sex)


dotchart(Dear$LCT[!Isna], groups = factor(Dear$Sex[!Isna]),
         xlab = "長さ (cm)", ylab = "性別観測数")


# #dotchart(Owls$NegPerChick, xlab = "Negotiation per check", ylab = "Order of the data")
#############

# 7.4.1
setwd("C:/RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header = TRUE, sep = "\t")
Benthic$fBeach <- factor(Benthic$Beach)

# par(mfrow=c(1,2))
dotchart(Benthic$Richness,groups=Benthic$fBeach,
   xlab="種数", ylab = "ビーチ")

Bent.M <- tapply(Benthic$Richness,Benthic$Beach,FUN = mean)


dotchart(Benthic$Richness,groups=Benthic$fBeach,
	       gdata = Bent.M, gpch=19, xlab="種数", ylab="ビーチ")

legend("bottomright", c("観測値", "平均値"), pch = c(1, 19), bg = "white")
###


# 7.5.1
methods(plot)

#plot
setwd("C:/RBook/")# setwd("RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header= TRUE,sep="\t")
Benthic$fBeach <- factor(Benthic$Beach)
plot(Benthic$Richness ~ Benthic$fBeach)


# 7.5.1


par(mfrow = c(2,2), mar = c(4,4,2,2) +.5)
plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "平均満潮レベル (m)", ylab = "種数",
     main = "水底生物データ")
M0 <- lm(Richness ~ NAP, data = Benthic)
abline(M0)

plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "平均満潮レベル (m)", ylab = "種数",
     xlim = c(-3, 3), ylim = c (0,20))

plot(y = Benthic$Richness, x = Benthic$NAP,
     type = "n", axes = FALSE,
     xlab = "平均満潮レベル", ylab = "種数")
points(y = Benthic$Richness, x = Benthic$NAP)


plot(y = Benthic$Richness, x = Benthic$NAP,
     type = "n", axes = FALSE,
     xlab = "平均満潮レベル", ylab = "種数",
     xlim = c(-1.75,2), ylim = c(0,20))
points(y = Benthic$Richness, x = Benthic$NAP)
axis(2, at = c(0, 10, 20), tcl = -1)
axis(1, at = c(-1.75,0,2), labels = c("海側","喫水線","砂浜") )
#

# 7.5.5 
setwd("C:/RBook/")
Birds <- read.table(file="loyn.txt", header= TRUE, sep="\t")
Birds$LOGAREA<- log10(Birds$AREA)
Birds$fGRAZE <- factor(Birds$GRAZE)

M0 <- lm(ABUND ~ LOGAREA + fGRAZE, data = Birds)
summary(M0)

plot(x = Birds$LOGAREA, y = Birds$ABUND,
     xlab = "対数変換された区域の面積", ylab = "鳥の個体数")

   M0$coefficients[1] + M0$coefficients[3:6]

# LAR <-seq(-1, 3, by = 0.1)
LAR <-seq(-1, 3, by = 1)


plot(x = Birds$LOGAREA, y = Birds$ABUND,
     xlab="対数変換された区域の面積", ylab="鳥の個体数")

ABUND1 <- 15.7 +  7.2 * LAR
ABUND2 <- 16.1 +  7.2 * LAR
ABUND3 <- 15.5 +  7.2 * LAR
ABUND4 <- 14.1 +  7.2 * LAR
ABUND5 <- 3.8 +  7.2 * LAR

 lines(LAR, ABUND1, lty = 1, lwd = 1, col =1)
 lines(LAR, ABUND2, lty = 2, lwd = 2, col =2)
 lines(LAR, ABUND3, lty = 3, lwd = 3, col =3)
 lines(LAR, ABUND4, lty = 4, lwd = 4, col =4)
 lines(LAR, ABUND5, lty = 5, lwd = 5, col =5)


legend.txt <- c("Graze 1","Graze 2","Graze 3","Graze 4","Graze 5")
legend("topleft",
      legend = legend.txt,
       col = c(1,2,3,4,5),
       lty = c(1,2,3,4,5),
       lwd = c(1,2,3,4,5),
       bty = "o",
       cex = 0.8
       )

title("あてはめられたモデル", cex.main = 2, family = "serif", font.main = 1)


# 7.5.6 

## setwd("C:/RBook/")


## Whales <- read.table(file="TeethNitrogen.txt", header= TRUE)
## N.Moby <- Whales$X15N[Whales$Tooth == "Moby"]
## Age.Moby <- Whales$Age[Whales$Tooth == "Moby"]


## plot(x = Age.Moby, y = N.Moby, xlab = "年齢",
##      ylab = expression(paste(delta^{15}, "N")))



## #Spagethi
## par(mfrow=c(1,3))
## plot(x = Benthic$NAP, y = Benthic$Richness, type = "b",
##      xlab="NAP", ylab="Richness")
## Iord <- order(Benthic$NAP)
## plot(x = Benthic$NAP[Iord], y = Benthic$Richness[Iord], type="b",
##      xlab = "NAP", ylab = "Richness")
## plot(x = Benthic$NAP[Iord],Benthic$Richness[Iord],
##      panel.first=lines(lowess(Benthic$NAP[Iord],
##      Benthic$Richness[Iord])),xlab="NAP",
##      ylab="Richness")

## #Identify
plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "平均満潮レベル(m)", ylab = "種数",
     main = "低性生物データ")

identify(y = Benthic$Richness, x = Benthic$NAP)

identify(y = Benthic$Richness, x = Benthic$NAP, LETTERS[seq(Benthic$Richness)])

#7.5.7
title("鳥の個体数", cex.main = 2,
family = "serif", font.main = 1)


#7.5.8

setwd("C:/RBook/")
Whales <- read.table(file="TeethNitrogen.txt", header= TRUE)
N.Moby <- Whales$X15N[Whales$Tooth == "Moby"]
Age.Moby <- Whales$Age[Whales$Tooth == "Moby"]


plot(x = Age.Moby, y = N.Moby, xlab = "年齢",
     ylab = expression(paste(delta^{15}, "N")))



#Spagethi
par(mfrow=c(1,3))

# 7.6 pairs

setwd("C:/RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header= TRUE)
plot(Benthic[,2:9])


pairs(Benthic[, 2:9])

# 7.7 Coplot
setwd("C:/RBook/")# setwd("RBook/")
Benthic <- read.table(file = "RIKZ2.txt", header = TRUE)


coplot(Richness ~ NAP | as.factor(Beach), pch = 19, data = Benthic, ylab = "種数 Richness")
#

#
coplot(Richness ~ NAP | grainsize, pch=19, data = Benthic, ylab = "種数 Richness")




panel.lm <- function(x, y, ...) {
  tmp <- lm(y~x,na.action=na.omit)
  abline(tmp, lwd = 2)
  points(x,y, ...)}


coplot(Richness ~ NAP | as.factor(Beach), pch = 19, ylab = "種数 Richness",  panel = panel.lm, data=Benthic)


coplot(Richness ~ NAP | as.factor(Beach), pch=19, span =1,
panel = panel.smooth, data=Benthic)

# 7.7.2
setwd("C:/RBook/")# setwd("RBook/")
pHEire <- read.table(file="SDI2003.txt",  header=TRUE, sep="\t")

pHEire$LOGAlt <- log10(pHEire$Altitude)
pHEire$fForested <- factor(pHEire$Forested)


coplot(pH ~ SDI | LOGAlt * fForested, panel = panel.lm, data = pHEire)



coplot(pH ~ SDI | LOGAlt * fForested,
	 panel = panel.lm, data = pHEire, number = 3)


# 7.7.3

pHEire$Temp2 <- cut(pHEire$Temperature, breaks = 2)
pHEire$Temp2.num <- as.numeric(pHEire$Temp2)

panel.lm2 <- function(x,y,col.line="black",lwd=par("lwd"),
	lty = par("lwt"), ...){
	tmp <- lm(y ~ x, na.action = na.omit)
	points(x,y, ...)
	abline(tmp, col = col.line, lwd = lwd, lty = lty)}


CI <- co.intervals(pHEire$LOGAlt,3)
GV <- list(CI,c(2,1))

pHEire$Temperature
pHEire$Temp2 <- cut(pHEire$Temperature, breaks = 2)
pHEire$Temp2.num <- as.numeric(pHEire$Temp2)
pHEire$Temp2.num

#
 coplot(pH ~ SDI | LOGAlt * fForested,
	 panel = panel.lm, data = pHEire,
   given.values = GV,
	 cex = 1.5, pch = 19,
	 col = gray(pHEire$Temp2.num/3))


#7.8 
MyLayOut <- matrix(c(2,0,1,3), nrow = 2, ncol=2, byrow = TRUE)
MyLayOut


#
nf <- layout(mat = MyLayOut, widths = c(3, 1),
           heights = c(1, 3), respect = TRUE)
layout.show(nf)


#
xrange <- c(min(Benthic$NAP), max(Benthic$NAP))
yrange <- c(min(Benthic$Richness), max(Benthic$Richness))


nf <- layout(mat = MyLayOut,widths = c(3, 1),
           heights = c(1, 3), respect = TRUE)

###################
par(mar=c(4,4,2,2))
plot(Benthic$NAP, Benthic$Richness, xlim = xrange, ylim = yrange,
     xlab =  "NAP", ylab="種数")

par(mar=c(0,3,1,1))
boxplot(Benthic$NAP, horizontal = TRUE,axes = F,
	frame.plot = F, ylim = xrange, space = 0)

par(mar=c(3,0,1,1))
boxplot(Benthic$Richness, axes = F, ylim = yrange, space = 0, horiz = TRUE)
#####



########################################################
#
#                       第 8 章
#
########################################################



# 8.2
setwd("C:/RBook")
Env <- read.table(file="RIKZENV.txt",header = TRUE)
names(Env)

## NAs?
  str(Env)
  sum(is.na(Env))  #2975 NAs

library(lattice)

Env$MyTime <- Env$Year+Env$dDay3/365


  xyplot(SAL ~ MyTime | factor(Station), type="l",
      strip = function(bg, ...)
           strip.default(bg = 'white', ...),
      col.line=1,
      data = Env)

xyplot(SAL ~ MyTime | factor(Station), data = Env)

xyplot(SAL ~ MyTime | factor(Station), type="l",
      strip = TRUE,  col.line = 1,    data = Env)


xyplot(SAL ~ MyTime | factor(Station), type="l",
      strip = FALSE, col.line = 1,
      data = Env)

# 8.3 
setwd("C:/RBook")
Env <- read.table(file="RIKZENV.txt",header = TRUE)
bwplot(SAL ~ factor(Month) | Area,
   strip = strip.custom(bg = 'white'),
   cex = .5, layout = c(2, 5),
   data = Env, xlab = "月", ylab = "塩分",
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)))


bwplot(SAL ~ factor(Month) | Area,
   layout = c(2, 5), data = Env)



# 8.4 
dotplot( factor(Month) ~ SAL  | Station,
    subset = Area=="OS", jitter.x = TRUE,
    data = Env, strip = strip.custom(bg = 'white'),
    col = 1, cex = 0.5, ylab = "月",
    xlab = "塩分")


# 8.5 

histogram( ~ SAL | Station, data = Env,
          subset = Area=="OS", layout = c(1,4),
          nint = 30, xlab = "塩分", ylab = "頻度",
          strip = FALSE,
          strip.left = strip.custom(bg = 'white'),
          col = "grey" )

# 8.6.1 
xyplot(SAL ~ Month | Year, data = Env,
     type = c("p"), subset = (Station =="GROO"),
     xlim = c(0, 12), ylim = c(0, 30), pch = 19,
     strip = strip.custom(bg = 'white', fg = 'grey'),
     panel = function (...){
     panel.xyplot(..., col = 1)
     panel.grid(..., h = -1, v = -1)
     panel.loess(..., col = 1)})

xyplot(SAL ~ Month | Year, data = Env,
       subset = (Station == "GROO"), pch = 19,
       xlim = c(0, 12), ylim = c(0, 30),
       type = c("p", "g", "smooth"))




# 8.6.2 


setwd("C:/RBook")
library(lattice)
Env <- read.table(file="RIKZENV.txt",header = TRUE)

 dotplot(factor(Month) ~ SAL | Station, pch = 16,
    subset = (Area=="OS"), data = Env,
    ylab = "月", xlab = "塩分",
    panel = function(x, y, ...) {
      Q <- quantile(x, c(0.25, 0.5, 0.75) , 
                    na.rm = TRUE)
      R <- Q[3] - Q[1]
      L <- Q[2] - 3 * (Q[3] - Q[1])
      MyCex <- rep(0.4, length(y))
      MyCol <- rep(1, length(y))
      MyCex[x < L] <- 1.5
      MyCol[x < L] <- 2
      panel.dotplot(x, y, cex = MyCex, 
                    col = MyCol, ...)})

#8.6.3
setwd("C:/RBook")
Sparrows <- read.table(file = "Sparrows.txt", header = TRUE)
names(Sparrows)
head(Sparrows)

#[1] "Species"  "Sex"      "Wingcrd"  "Tarsus"   "Head"     "Culmen"
#[7] "Nalospi"  "Wt"       "Observer" "Age"
 
library(lattice)

#####
 xyplot(Wingcrd ~ Tarsus | Species * Sex,
     xlab = "軸 1", ylab = "軸 2", data = Sparrows,
     xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
             strip = strip.custom(bg = 'white'),
     panel = function(subscripts, ...){
       zi <- Sparrows[subscripts, 3:8]
       di <- princomp(zi, cor = TRUE)
       Load <- di$loadings[, 1:2]
       Scor <- di$scores[, 1:2]
       panel.abline(a = 0, b = 0, lty = 2, col = 1)
       panel.abline(h = 0, v = 0, lty = 2, col = 1)
       for (i in 1:6){
           llines(c(0, Load[i, 1]), c(0, Load[i, 2]), 
                  col = 1, lwd = 2)
           ltext(Load[i, 1], Load[i, 2], 
                 rownames(Load)[i], cex = 0.7)}
       sc.max <- max(abs(Scor))
       Scor <- Scor / sc.max
       panel.points(Scor[, 1], Scor[, 2], pch = 1,
                   cex = 0.5, col = 1)
      })

    
# 8.7

cloud(CHLFa ~ T * SAL | Station, data = Env,
#       strip = strip.custom(bg = 'white'),col = 1,
    screen = list(z = 105, x = -70),
    ylab = "Sal", xlab = "T", zlab = "Chl. a",
    ylim = c(26,33),
    subset = (Area=="OS"),
    scales = list(arrows = FALSE)
      )
#

# 8.8.1
setwd("C:/RBook")
Hawaii <- read.table("waterbirdislandseries.txt", header = TRUE)
library(lattice)

names(Hawaii)
Birds <- as.vector(Hawaii[,2:9])

Birds <- as.vector(as.matrix(Hawaii[,2:9]))


Time <- rep(Hawaii$Year, 8)
MyNames <- c("Stilt_Oahu","Stilt_Maui","Stilt_Kauai_Niihau",
             "Coot_Oahu","Coot_Maui","Coot_Kauai_Niihau",
             "Moorhen_Oahu","Moorhen_Kauai")
ID <- rep(MyNames, each = 48)


   
xyplot(Birds ~ Time|ID, ylab = "鳥の個体数",
      # strip = strip.custom(bg = 'white'),
       layout = c(3, 3), type = "l", col = 1)

ID2 <- factor(ID,
             levels=c(
             "Stilt_Oahu",
             "Stilt_Kauai_Niihau",
             "Stilt_Maui",
             "Coot_Oahu",
             "Coot_Kauai_Niihau",
             "Coot_Maui",
             "Moorhen_Oahu",
             "Moorhen_Kauai"))

# 8.8.2
 
xyplot(Birds ~ Time|ID2,
       ylab = "鳥の個体数", 
       layout = c(3, 3), type = "l", col = 1,
       scales = list(x = list(relation = "same"),
                     y = list(relation = "free") ))

xyplot(Birds ~ Time|ID2,
       ylab = "鳥の個体数",strip = strip.custom(bg = 'white'),
       # ylab = "Bird abundance",
       layout = c(3, 3), type = "l", col = 1,
       scales = list(x = list(relation = "same"),
                     y = list(relation = "free"),
                     tck=-1))

# 8.8.3

Species <-rep(c("Stilt","Stilt","Stilt",
            "Coot","Coot","Coot",
            "Moorhen","Moorhen"), each = 48)

 
xyplot(Birds ~ Time|Species, ylab = "鳥の個体数",
        strip = strip.custom(bg = 'white'),
       layout = c(2, 2), type = "l", col = 1,
       scales = list(x = list(relation = "same"),
                     y = list(relation = "free")),
       groups = ID, lwd = c(1,2,3))
 


# 8.8.4

setwd("C:/RBook")
Env <- read.table(file ="RIKZENV.txt", header = TRUE)
library(lattice)

AllAreas <- levels(unique(Env$Area))
for (i in AllAreas  ){ 
     Env.i <- Env[Env$Area==i,]
     dotplot(factor(Month)~SAL | Station, 
       data = Env.i)
     win.graph()
      }
           
           

# 8.8.5
Env$MyTime<-Env$Year+Env$dDay3/365

 MyPlot <- xyplot(SAL ~ MyTime | Station,
        type = "l", data = Env)

 print(MyPlot)
 update(MyPlot, layout = c(10, 3))











########################################################
#
#                       第 9 章
#
########################################################



# 9.1.2

setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt", header = TRUE, dec =",")
names(Parasite)
str(Parasite)

mean(Parasite$Intensity)
boxplot(Parasite$Intensity)

mean(Parasite$Weight)
boxplot(Parasite$Weight)


#9.2.1  Attach section
setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt", header = TRUE)
attach(Parasite)
Prrrarassite

Parasite <- read.table(file="CodParasite.txt", header = TRUE)
attach(Parasite)

detach(Parasite)


# 9.2.2
setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt", header = TRUE)
Squid <- read.table(file="squid.txt", header = TRUE)
names(Parasite)
names(Squid)
attach(Parasite)
attach(Squid)

boxplot(Intensity~Sex)
lm(Intensity~Sex)



# 9.2.4
setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt", header = TRUE)
Parasite$fSex <- factor(Parasite$Sex)
Parasite$fSex
attach(Parasite)
fSex
Parasite$fArea <- factor(Parasite$Area)
fArea



#Section 9.3 

setwd("c:/RBook/")
Parasite <- read.table(file="RBook/CodParasite.txt", header = TRUE)

M0 <- lm(Parasite$Intensity ~ Parasite$Length * factor(Parasite$Sex))
library(nlme)
M1 <- gls(Parasite$Intensity ~ Parasite$Length * factor(Parasite$Sex))


# 9.4
setwd("c:/RBook/")
Parasite <- read.table(file="CodParasite.txt", header = TRUE)

Parasite$LIntensity <- log(Parasite$Intensity)
Parasite$L1Intensity <- log(Parasite$Intensity + 1)
### 
boxplot(Parasite$LIntensity, Parasite$L1Intensity,
        names = c("log(Intensity)","log(Intensity+1)"))
#


M0 <- lm(LIntensity ~ Length * factor(Sex), data = Parasite)


## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
##         NA/NaN/Inf in foreign function call (arg 4)
        
        
        
        
        
        
# 9.5.1 
x <- seq(1,10)
plot(x, type = "l") 
plot(x, type = "1")


# 9.5.2
par(mfrow = c(2,1), mar = c(3,3,2,1))
dotchart(Parasite$Depth)
dotchart(Parasite$Depth, col = Parasite$Prevalence )
#




# 9.6
Owls <- read.table(file = "C:/RBook/Owls.txt", header= TRUE)
attach(Owls)
ls()

########################### 以上 #############################

############################################################
#                 コードに問題がある場合は                  #
#               ishida.motohiro @ gmail.com                #
#                 までお問い合わせください                  #
############################################################

