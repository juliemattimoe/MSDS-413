
library('forecast')
library('readr')

airpassengers <- read_csv("~/Desktop/MSDS 413/monthlyairpassenger.csv")

head(airpassengers)

airpassengers.ts<-ts(airpassengers$Monthly, frequency=12, start=c(1960,1))
plot(airpassengers.ts)

train.ts<-window(airpassengers.ts, end=c(1975,12))
test.ts<-window(airpassengers.ts,start=c(1976,1))

fit.ETS<-ets(train.ts, model="ZZZ")
fit.Arima<-auto.arima(train.ts)
fit.nnet<-nnetar(train.ts)

fit.ETS

fit.Arima

fit.nnet

fcast.ETS<-forecast(fit.ETS,h=12)
fcast.Arima<-forecast(fit.Arima,h=12)
fcast.nnet<-forecast(fit.nnet,h=12)

plot(fcast.ETS) 
lines(test.ts, col="red")  
legend("topleft",lty=1,col=c("red","blue"),c("actual values","forecast"))

plot(fcast.Arima) 
lines(test.ts, col="red")  
legend("topleft",lty=1,col=c("red","blue"),c("actual values","forecast"))

plot(fcast.nnet) 
lines(test.ts, col="red")  
legend("topleft",lty=1,col=c("red","blue"),c("actual values","forecast"))

checkresiduals(fcast.ETS)

checkresiduals(fcast.Arima)

checkresiduals(fcast.nnet)

acc.ETS<-accuracy(fcast.ETS, test.ts)
acc.ETS

acc.Arima<-accuracy(fcast.Arima, test.ts)
acc.Arima

acc.nnet<-accuracy(fcast.nnet, test.ts)
acc.nnet
