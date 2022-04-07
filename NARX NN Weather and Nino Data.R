##PACKAGES
library(forecast) 
library(tseries) 
library(nnfor) 
library(TSstudio)
library(Rtools)
library(neuralnet)
library(bruceR) #
install.packages("bruceR")
install.packages("RTools")

##INPUT DATA, SCALING, TRANSFORMASI TIME SERIES
data=read.csv(file.choose(),sep=";",dec=",",header=T)
data
View(data)
rainfall = (data[,2])
nino34 = (data[,3])
#norm_minmax <- function(x){
#  (x- min(x)) /(max(x)-min(x))}
#rainfall <- as.data.frame(lapply(rainfall, norm_minmax))
#nino34 <- as.data.frame(lapply(nino, norm_minmax))
rainfall = scaler(data[,2], min=-0.8, max=0.8)
nino34 = scaler(data[,3], min=-0.8, max=0.8)
rainfall.all = ts(rainfall,frequency = 12,start = c(2001,1),end = c(2021,12)) 
nino34.all = ts(nino34,frequency = 12,start = c(2001,1),end = c(2021,12))

###MODELLING DATA RAINFALL
##PEMBAGIAN DATA Y
Train1y=ts(rainfall[1:201],frequency = 12,start = c(2001,1)) 
Train2y=ts(rainfall[1:202],frequency = 12,start = c(2001,1)) 
Train3y=ts(rainfall[1:203],frequency = 12,start = c(2001,1)) 
Train4y=ts(rainfall[1:204],frequency = 12,start = c(2001,1)) 
Train5y=ts(rainfall[1:205],frequency = 12,start = c(2001,1)) 
Train6y=ts(rainfall[1:206],frequency = 12,start = c(2001,1)) 
Train7y=ts(rainfall[1:207],frequency = 12,start = c(2001,1)) 
Train8y=ts(rainfall[1:208],frequency = 12,start = c(2001,1)) 
Train9y=ts(rainfall[1:209],frequency = 12,start = c(2001,1))
Train10y=ts(rainfall[1:210],frequency = 12,start = c(2001,1)) 
Train11y=ts(rainfall[1:211],frequency = 12,start = c(2001,1)) 
Train12y=ts(rainfall[1:212],frequency = 12,start = c(2001,1)) 
Train13y=ts(rainfall[1:213],frequency = 12,start = c(2001,1)) 
Train14y=ts(rainfall[1:214],frequency = 12,start = c(2001,1)) 
Train15y=ts(rainfall[1:215],frequency = 12,start = c(2001,1)) 
Train16y=ts(rainfall[1:216],frequency = 12,start = c(2001,1)) 
Train17y=ts(rainfall[1:217],frequency = 12,start = c(2001,1)) 
Train18y=ts(rainfall[1:218],frequency = 12,start = c(2001,1)) 
Train19y=ts(rainfall[1:219],frequency = 12,start = c(2001,1)) 
Train20y=ts(rainfall[1:220],frequency = 12,start = c(2001,1)) 
Train21y=ts(rainfall[1:221],frequency = 12,start = c(2001,1)) 
Train22y=ts(rainfall[1:222],frequency = 12,start = c(2001,1)) 
Train23y=ts(rainfall[1:223],frequency = 12,start = c(2001,1)) 
Train24y=ts(rainfall[1:224],frequency = 12,start = c(2001,1)) 
Train25y=ts(rainfall[1:225],frequency = 12,start = c(2001,1)) 
Train26y=ts(rainfall[1:226],frequency = 12,start = c(2001,1)) 
Train27y=ts(rainfall[1:227],frequency = 12,start = c(2001,1)) 
Train28y=ts(rainfall[1:228],frequency = 12,start = c(2001,1)) 
Train29y=ts(rainfall[1:229],frequency = 12,start = c(2001,1))
Train30y=ts(rainfall[1:230],frequency = 12,start = c(2001,1))
Train31y=ts(rainfall[1:231],frequency = 12,start = c(2001,1)) 
Train32y=ts(rainfall[1:232],frequency = 12,start = c(2001,1)) 
Train33y=ts(rainfall[1:233],frequency = 12,start = c(2001,1)) 
Train34y=ts(rainfall[1:234],frequency = 12,start = c(2001,1)) 
Train35y=ts(rainfall[1:235],frequency = 12,start = c(2001,1)) 
Train36y=ts(rainfall[1:236],frequency = 12,start = c(2001,1)) 
Train37y=ts(rainfall[1:237],frequency = 12,start = c(2001,1)) 
Train38y=ts(rainfall[1:238],frequency = 12,start = c(2001,1)) 
Train39y=ts(rainfall[1:239],frequency = 12,start = c(2001,1))
Train40y=ts(rainfall[1:240],frequency = 12,start = c(2001,1)) 
Train41y=ts(rainfall[1:241],frequency = 12,start = c(2001,1)) 
Train42y=ts(rainfall[1:242],frequency = 12,start = c(2001,1)) 
Train43y=ts(rainfall[1:243],frequency = 12,start = c(2001,1)) 
Train44y=ts(rainfall[1:244],frequency = 12,start = c(2001,1)) 
Train45y=ts(rainfall[1:245],frequency = 12,start = c(2001,1)) 
Train46y=ts(rainfall[1:246],frequency = 12,start = c(2001,1)) 
Train47y=ts(rainfall[1:247],frequency = 12,start = c(2001,1)) 
Train48y=ts(rainfall[1:248],frequency = 12,start = c(2001,1)) 
Train49y=ts(rainfall[1:249],frequency = 12,start = c(2001,1))
Train50y=ts(rainfall[1:250],frequency = 12,start = c(2001,1)) 
Train51y=ts(rainfall[1:251],frequency = 12,start = c(2001,1)) 
datatrain.y = cbind(Train1y,Train2y,Train3y,Train4y,Train5y,Train6y, 
                    Train7y,Train8y,Train9y, Train10y, Train11y, Train12y,
                    Train13y, Train14y, Train15y, Train16y, Train17y, Train18y,
                    Train19y, Train20y, Train21y, Train22y, Train23y, Train24y,
                    Train25y, Train26y, Train27y, Train28y, Train29y, Train30y,
                    Train31y, Train32y, Train33y, Train34y, Train35y, Train36y,
                    Train37y, Train38y, Train39y, Train40y, Train41y, Train42y,
                    Train43y, Train44y, Train45y, Train46y, Train47y, Train48y,
                    Train49y, Train50y, Train51y) 
datatest.y = cbind(rainfall[202:252])
xreg = cbind(nino34)
dim(datatrain.y)
dim(datatest.y)
dim(xreg)

##TRAINING, TESTING, EVALUASI Y
rollingy = function(datatrain,datatest,xreg,hd=NULL,lags=NULL,x.lags=NULL){
  k=ncol(datatrain)
  e=c()
  mape=c()
  forecast=c()
  for (i in 1:k) {
    model=mlp(na.omit(datatrain[,i]),hd=hd,lags=lags,sel.lag=F, 
              allow.det.season=F,difforder=0,xreg=xreg,xreg.lags=x.lags, 
              act.fct="tanh")
    frc=forecast(model,xreg=xreg,1)
    forecast[i]=frc$mean
    e[i]=datatest[i]-forecast[i]
    mape[i]=accuracy(frc)[5] }
  mape.train=mean(mape)
  mape.test=mean(abs(e/datatest))*100
  hasil=data.frame(datatest,forecast)
  colnames(hasil)=c("datatest","forecast")
  print(list(mape.train=mape.train,mape.test=mape.test,hasil=hasil))
}

model1y=mlp(Train1y,hd=1,lags = c(1:12),sel.lag=F,
            allow.det.season=F,difforder=0,xreg=xreg,
            act.fct="tanh")
frc1y=forecast(model1y,xreg=xreg,1)
a=datatest.y[1,]
e=a-frc1y$mean
mape.testy1=mean(abs(e/a))*100;mape.testy1
rmse.testy1=sqrt(mean(e^2));rmse.testy1
mse.testy1=mean(e^2);mse.testy1

#COBA TRIAL AND ERROR HIDDEN LAYER
rolling.y1 = rollingy(datatrain.y,datatest.y,xreg,hd = 1,lags = c(1:12), x.lags = c(0))
rolling.y2 = rollingy(datatrain.y,datatest.y,xreg,hd = 2,lags = c(1:12), x.lags = c(0))
rolling.y3 = rollingy(datatrain.y,datatest.y,xreg,hd = 3,lags = c(1:12), x.lags = c(0))
rolling.y4 = rollingy(datatrain.y,datatest.y,xreg,hd = 4,lags = c(1:12), x.lags = c(0))
rolling.y5 = rollingy(datatrain.y,datatest.y,xreg,hd = 5,lags = c(1:12), x.lags = c(0))
rolling.y6 = rollingy(datatrain.y,datatest.y,xreg,hd = 6,lags = c(1:12), x.lags = c(0))
rolling.y7 = rollingy(datatrain.y,datatest.y,xreg,hd = 7,lags = c(1:12), x.lags = c(0))

ts.plot(ts(rolling.y$hasil$datatest,frequency = 12), 
        ts(rolling.y$hasil$forecast,frequency = 12), 
        lty=c(1,3),col=c("blue","red"),type="o", 
        main="Plot Hasil Peramalan Training Y dan Data Testing Y")

###MODELLING DATA NINO 3.4
##PEMBAGIAN DATA X
Train1x=ts(nino34[1:201],frequency = 12,start = c(2001,1)) 
Train2x=ts(nino34[1:202],frequency = 12,start = c(2001,1)) 
Train3x=ts(nino34[1:203],frequency = 12,start = c(2001,1)) 
Train4x=ts(nino34[1:204],frequency = 12,start = c(2001,1)) 
Train5x=ts(nino34[1:205],frequency = 12,start = c(2001,1)) 
Train6x=ts(nino34[1:206],frequency = 12,start = c(2001,1)) 
Train7x=ts(nino34[1:207],frequency = 12,start = c(2001,1)) 
Train8x=ts(nino34[1:208],frequency = 12,start = c(2001,1)) 
Train9x=ts(nino34[1:209],frequency = 12,start = c(2001,1))
Train10x=ts(nino34[1:210],frequency = 12,start = c(2001,1)) 
Train11x=ts(nino34[1:211],frequency = 12,start = c(2001,1)) 
Train12x=ts(nino34[1:212],frequency = 12,start = c(2001,1)) 
Train13x=ts(nino34[1:213],frequency = 12,start = c(2001,1)) 
Train14x=ts(nino34[1:214],frequency = 12,start = c(2001,1)) 
Train15x=ts(nino34[1:215],frequency = 12,start = c(2001,1)) 
Train16x=ts(nino34[1:216],frequency = 12,start = c(2001,1)) 
Train17x=ts(nino34[1:217],frequency = 12,start = c(2001,1)) 
Train18x=ts(nino34[1:218],frequency = 12,start = c(2001,1)) 
Train19x=ts(nino34[1:219],frequency = 12,start = c(2001,1)) 
Train20x=ts(nino34[1:220],frequency = 12,start = c(2001,1)) 
Train21x=ts(nino34[1:221],frequency = 12,start = c(2001,1)) 
Train22x=ts(nino34[1:222],frequency = 12,start = c(2001,1)) 
Train23x=ts(nino34[1:223],frequency = 12,start = c(2001,1)) 
Train24x=ts(nino34[1:224],frequency = 12,start = c(2001,1)) 
Train25x=ts(nino34[1:225],frequency = 12,start = c(2001,1)) 
Train26x=ts(nino34[1:226],frequency = 12,start = c(2001,1)) 
Train27x=ts(nino34[1:227],frequency = 12,start = c(2001,1)) 
Train28x=ts(nino34[1:228],frequency = 12,start = c(2001,1)) 
Train29x=ts(nino34[1:229],frequency = 12,start = c(2001,1))
Train30x=ts(nino34[1:230],frequency = 12,start = c(2001,1))
Train31x=ts(nino34[1:231],frequency = 12,start = c(2001,1)) 
Train32x=ts(nino34[1:232],frequency = 12,start = c(2001,1)) 
Train33x=ts(nino34[1:233],frequency = 12,start = c(2001,1)) 
Train34x=ts(nino34[1:234],frequency = 12,start = c(2001,1)) 
Train35x=ts(nino34[1:235],frequency = 12,start = c(2001,1)) 
Train36x=ts(nino34[1:236],frequency = 12,start = c(2001,1)) 
Train37x=ts(nino34[1:237],frequency = 12,start = c(2001,1)) 
Train38x=ts(nino34[1:238],frequency = 12,start = c(2001,1)) 
Train39x=ts(nino34[1:239],frequency = 12,start = c(2001,1))
Train40x=ts(nino34[1:240],frequency = 12,start = c(2001,1)) 
Train41x=ts(nino34[1:241],frequency = 12,start = c(2001,1)) 
Train42x=ts(nino34[1:242],frequency = 12,start = c(2001,1)) 
Train43x=ts(nino34[1:243],frequency = 12,start = c(2001,1)) 
Train44x=ts(nino34[1:244],frequency = 12,start = c(2001,1)) 
Train45x=ts(nino34[1:245],frequency = 12,start = c(2001,1)) 
Train46x=ts(nino34[1:246],frequency = 12,start = c(2001,1)) 
Train47x=ts(nino34[1:247],frequency = 12,start = c(2001,1)) 
Train48x=ts(nino34[1:248],frequency = 12,start = c(2001,1)) 
Train49x=ts(nino34[1:249],frequency = 12,start = c(2001,1))
Train50x=ts(nino34[1:250],frequency = 12,start = c(2001,1)) 
Train51x=ts(nino34[1:251],frequency = 12,start = c(2001,1)) 
datatrain.x = cbind(Train1x,Train2x,Train3x,Train4x,Train5x,Train6x, 
                    Train7x,Train8x,Train9x, Train10x, Train11x, Train12x,
                    Train13x, Train14x, Train15x, Train16x, Train17x, Train18x,
                    Train19x, Train20x, Train21x, Train22x, Train23x, Train24x,
                    Train25x, Train26x, Train27x, Train28x, Train29x, Train30x,
                    Train31x, Train32x, Train33x, Train34x, Train35x, Train36x,
                    Train37x, Train38x, Train39x, Train40x, Train41x, Train42x,
                    Train43x, Train44x, Train45x, Train46x, Train47x, Train48x,
                    Train49x, Train50x, Train51x) 
datatest.x = nino34[202:252]

rollingx = function(datatrain,datatest,xreg,hd=NULL,lags=NULL,x.lags=NULL){
  k=ncol(datatrain)
  e=c()
  mape=c()
  forecast=c()
  for (i in 1:k) {
    model=mlp(na.omit(datatrain[,i]),hd=hd,lags=lags,sel.lag=F, 
              allow.det.season=F,difforder=0,xreg=xreg,xreg.lags=x.lags, 
              act.fct="tanh")
    frc=forecast(model,xreg=xreg,1)
    forecast[i]=frc$mean
    e[i]=datatest[i]-forecast[i]
    mape[i]=accuracy(frc)[5] }
  mape.train=mean(mape)
  mape.test=mean(abs(e/datatest))*100
  hasil=data.frame(datatest,forecast)
  colnames(hasil)=c("datatest","forecast")
  print(list(mape.train=mape.train,mape.test=mape.test,hasil=hasil))
}

rolling.x1 = rollingx(datatrain.x,datatest.x,xreg,hd = 1,lags = c(1:12), x.lags = c(0))
rolling.x2 = rollingx(datatrain.x,datatest.x,xreg,hd = 2,lags = c(1:12), x.lags = c(0))
rolling.x3 = rollingx(datatrain.x,datatest.x,xreg,hd = 3,lags = c(1:12), x.lags = c(0))
rolling.x4 = rollingx(datatrain.x,datatest.x,xreg,hd = 4,lags = c(1:12), x.lags = c(0))
rolling.x5 = rollingx(datatrain.x,datatest.x,xreg,hd = 5,lags = c(1:12), x.lags = c(0))
rolling.x6 = rollingx(datatrain.x,datatest.x,xreg,hd = 6,lags = c(1:12), x.lags = c(0))
rolling.x7 = rollingx(datatrain.x,datatest.x,xreg,hd = 7,lags = c(1:12), x.lags = c(0))

ts.plot(ts(rolling.x5$hasil$datatest,frequency = 12),
        ts(rolling.x5$hasil$forecast,frequency = 12),
        lty=c(1,3),col=c("blue","red"),type="o",
        main="Plot Hasil Peramalan Training X dan Data Testing X")

###PENERAPAN MODEL FFNN TERBAIK PADA SELURUH DATA X / NINO34
#hx = rolling.x$hd.best
model.x = mlp(nino34.all,hd = 4,lags = c(1:12),sel.lag = F,difforder=0,
              allow.det.season = F,outplot = T,act.fct="tanh")
ts.plot(nino34.all,fitted(model.x),lty=c(1,3),col=c("blue","red"),
        main="Plot Aktual dan Prediksi Nino 3.4")

AdjRsq=function(data,model,input,lag){
  y=data[-c(1:lag)]
  T=length(y)
  k=input
  yhat=fitted(model)
  Rsq = sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  AdjRsq = 1-((1-Rsq)*((T-1)/(T-k-1)))
  print(AdjRsq*100)}
AdjRsq.x = AdjRsq(nino34,model.x,12,12)

##PERAMALAN X
frcx = forecast(model.x,h=6)
plot(frcx)
summary(frcx)
round(exp(frcx$mean))
##GABUNGAN DATA X
frcxx = ts_reshape(frcx$mean,type = "long")
baris = length(nino34)+length(frcx$mean)
x0 = matrix(c(nino34,frcxx$value),baris,1)
x.gab = cbind(x0) #data gabungan nino 34

### PENERAPAN MODEL NARX NN TERBAIK PADA SELURUH DATA Y / NINO 34
#hy = rolling.y$hd.best 
model.y = mlp(rainfall.all,hd = 9,lags = c(1:12),sel.lag = F,difforder=0, 
              allow.det.season = F,xreg = x.gab,xreg.lags = c(0), outplot = T, 
              act.fct="tanh") 
ts.plot(rainfall.all,fitted(model.y),lty=c(1,3),col=c("blue","red"),
        main="Plot Aktual dan Prediksi Curah Hujan")
AdjRsq.y = AdjRsq(rainfall,model.y,13,12)

##PERAMALAN Y
frcy = forecast(model.y,h=6,xreg=x.gab)
plot(frcy)
summary(frcy)
exp(frcy$mean)
frcyy = ts_reshape(frcy$mean,type = "long")
baris = length(rainfall)+length(frcy$mean)
y0 = matrix(c(rainfall,frcyy$value),baris,1)

### PLOT PERAMALAN CURAH HUJAN & NINO
aktualX = ts(scale(x0)[1:252,],frequency = 12, start = c(2001,1))
aktualY = ts(scale(y0)[1:252,],frequency = 12, start = c(2001,1))
forecastX = ts(scale(x0)[253:258,],frequency = 12, start = c(2022,1))
forecastY = ts(scale(y0)[253:258,],frequency = 12, start = c(2022,1))
ts.plot(aktualX,forecastX,aktualY,forecastY,col=c(3,3,1,1),type="l",
        main="Peramalan Nino 34 (Hitam) dan Curah Hujan (Hijau)")