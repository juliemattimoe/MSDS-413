
if (!require("fma")) install.packages("fma")
if (!require("forecast)")) install.packages("forecast")
if (!require("ZIM)")) install.packages("ZIM")
library(fma)
library(forecast)
library(ZIM)

if (!require("rdatamarket)")) install.packages("rdatamarket")
if (!require("ggplot2)")) install.packages("ggplot2")
if (!require("grid)")) install.packages("grid")
library(rdatamarket)
library(ggplot2)
library(grid)

library(fpp2)

plot(wmurders)

adf.test(wmurders)

plot(diff(wmurders))

fita=ts(wmurders, frequency = 12)
dec = stl(fita, s.window="periodic")
deseasonal <- seasadj(dec)
plot(dec)

acf(fita)

pacf(fita)

fitb = diff(deseasonal, differences = 1)
plot(fitb)

Acf(fitb)

Pacf(fitb)

fit2 = arima(fitb, order=c(1,1,1))
fit3 = arima(fitb, order = c(1,0,1))
fit2

fit3

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

lag1<-bshift(wmurders, k = 1)
fitlag <- arima(lag1,order=c(1,1,1))
fitlag

checkresiduals(fitlag)

fcast<-forecast(fitlag, h= 3)
fcast

plot(fcast)

fit.auto <- auto.arima(deseasonal)
fit.auto

autoplot(usgdp)
autoplot(BoxCox(usgdp, BoxCox.lambda(usgdp)))
lambda_usgdp <- BoxCox.lambda(usgdp)

usgdp_autoarima <- auto.arima(usgdp, 
                              lambda = lambda_usgdp)
autoplot(usgdp, series = "Data") +
  autolayer(usgdp_autoarima$fitted, series = "Fitted")

usgdp_autoarima

ndiffs(BoxCox(usgdp, lambda_usgdp))
ggtsdisplay(diff(BoxCox(usgdp, lambda_usgdp)))
usgdp_arima.1.1.0 <- Arima(
  usgdp, lambda = lambda_usgdp, order = c(1, 1, 0)
)
usgdp_arima.1.1.0
autoplot(usgdp, series = "Data") +
  autolayer(usgdp_arima.1.1.0$fitted, series = "Fitted")

usgdp_arima.1.1.0.drift <- Arima(
  usgdp, lambda = lambda_usgdp, order = c(1, 1, 0),
  include.drift = TRUE
)
usgdp_arima.1.1.0.drift
autoplot(usgdp, series = "Data") +
  autolayer(usgdp_arima.1.1.0.drift$fitted, series = "Fitted")

accuracy(usgdp_autoarima)
accuracy(usgdp_arima.1.1.0)
accuracy(usgdp_arima.1.1.0.drift)

checkresiduals(usgdp_autoarima)
checkresiduals(usgdp_arima.1.1.0.drift)

fc_usgdp_autoarima <- forecast(
  usgdp_autoarima
)
autoplot(fc_usgdp_autoarima)

fc_usgdp_ets <- forecast(
  ets(usgdp)
)
autoplot(fc_usgdp_ets)

plot(austourists)

acf(austourists)

pacf(austourists)

ggtsdisplay(diff(austourists, lag = 4))

ggtsdisplay(diff(diff(austourists, lag = 4)))

fc_austourists_autoarima <- forecast(
  auto.arima(austourists)
)
fc_austourists_autoarima$model

fc_austourists_arima.1.1.0.1.1.0.4 <- forecast(
  Arima(austourists, 
        order = c(1, 1, 0), 
        seasonal = c(1, 1, 0))
)
autoplot(fc_austourists_autoarima)


autoplot(fc_austourists_arima.1.1.0.1.1.0.4)


accuracy(fc_austourists_autoarima)
accuracy(fc_austourists_arima.1.1.0.1.1.0.4)

checkresiduals(fc_austourists_autoarima)

fit<-bshift(austourists, k = 1)
plot.ts(fit)

auto.arima(fit)

unemployment_ts <- ts(rdatamarket::dmseries("http://bit.ly/2V0RXnr")[,1], start=c(1980,1), frequency = 12)

plot.ts(unemployment_ts)

adf.test(unemployment_ts
         , alternative = "stationary"
         , k = 12
)

ggAcf(unemployment_ts)

ggPacf(unemployment_ts)

diff1 = diff(unemployment_ts)

plot.ts(diff1)

ggAcf(diff1)

ggPacf(diff1)

fit.arima <- Arima(unemployment_ts, order=c(1,1,0), seasonal=c(1,1,1))
summary(fit.arima)  

tsdisplay(residuals(fit.arima))

ggAcf(fit.arima$residuals)

ggPacf(fit.arima$residuals)

checkresiduals(fit.arima)

arima.forecast <- forecast(fit.arima, h = 48)
plot(arima.forecast)

fit.ets <- ets(unemployment_ts, model="AAN", alpha=NULL, beta=NULL,
    gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
    additive.only=FALSE, restrict=TRUE,
    allow.multiplicative.trend=FALSE)

checkresiduals(fit.ets)

ets.forecast <- forecast(fit.ets, h = 48)
plot(ets.forecast)

str(advert)
head(advert)
autoplot(advert, facets = TRUE)

advert_tslm <- tslm(sales ~ advert, data = advert)

checkresiduals(advert_tslm)

advert_dreg.0.0.0 <- Arima(
  advert[, "sales"], xreg = advert[, "advert"],
  order = c(0, 0, 0)
)
checkresiduals(advert_dreg.0.0.0)
advert_tslm$residuals - advert_dreg.0.0.0$residuals

advert_dreg.auto <- auto.arima(
  advert[, "sales"], xreg = advert[, "advert"]
)
advert_dreg.0.0.0

advert_dreg.auto

checkresiduals(advert_dreg.auto)

fc_advert_dreg.auto <- forecast(
  advert_dreg.auto, h = 6,
  xreg = rep(10, 6)
  )
autoplot(fc_advert_dreg.auto)

autoplot(huron)
t <- time(huron)
t.knot <- 1920

t.pw <- ts(pmax(0, t - t.knot), start = t[1])
huron_xreg <- cbind(t = t, t.pw = t.pw)
huron_dreg.auto <- auto.arima(
  huron, xreg = huron_xreg
)
huron_dreg.auto

autoplot(huron) +
  autolayer(huron_dreg.auto$fitted)

h <- 30
t.new <- t[length(t)] + seq(h)
t.pw.new <- t.pw[length(t.pw)] + seq(h)
newdata <- cbind(t = t.new, t.pw = t.pw.new)
fc_huron_dreg.auto <- forecast(
  huron_dreg.auto, xreg = newdata, h = 30
)
autoplot(fc_huron_dreg.auto)

checkresiduals(fc_huron_dreg.auto)

autoplot(motel, facets = TRUE)
avg.cost_night.room <- motel[, "Takings"] / motel[, "Roomnights"]
autoplot(avg.cost_night.room)

CPI_autoarima <- auto.arima(
  motel[, "CPI"], lambda = 0
)
autoplot(motel[, "CPI"]) +
  autolayer(CPI_autoarima$fitted)

autoplot(avg.cost_night.room)
autoplot(CPI_autoarima$fitted)

avg.cost_night.room_tslm <- tslm(
  avg.cost_night.room ~ CPI_autoarima$fitted,
  lambda = 0
)
checkresiduals(avg.cost_night.room_tslm)

avg.cost_night.room_dreg.auto <- auto.arima(
  avg.cost_night.room, xreg = CPI_autoarima$fitted,
  lambda = 0, stepwise = FALSE, approximation = FALSE
)
checkresiduals(avg.cost_night.room_dreg.auto)

fc_CPI_autoarima <- forecast(
  CPI_autoarima, h = 12
)
fc_avg.cost_night.room_dreg.auto <- forecast(
  avg.cost_night.room_dreg.auto,
  xreg = fc_CPI_autoarima$mean,
  h = 12
)
autoplot(fc_avg.cost_night.room_dreg.auto)

str(gasoline)
head(gasoline)
autoplot(gasoline)

t <- time(gasoline)
t.knot1 <- 2007.5
t.knot2 <- 2013
t.pw1 <- ts(pmax(0, t - t.knot1), start = t[1],
            frequency = 365.25/7)
t.pw2 <- ts(pmax(0, t - t.knot2), start = t[1],
            frequency = 365.25/7)

AICc <- Inf
K_min.Aicc <- 0

for(num in c(1:26)){
  gasoline_tslm <- tslm(
    gasoline ~ trend + t.pw1 + t.pw2 + fourier(
      gasoline, K = num
    )
  )
  AICc_value <- CV(gasoline_tslm)["AICc"]
  
  if(AICc > AICc_value){
    AICc <- AICc_value
  }else{
    K_min.Aicc <- num
    break 
  }
}
K_min.Aicc

gasoline_tslm

autoplot(gasoline) +
  geom_line(color = "gray") +
  autolayer(gasoline_tslm$fitted.values)

gasoline_autoarima <- Arima(
  gasoline, xreg = cbind(t=t, t.pw1=t.pw1,  t.pw2=t.pw2, Fourier = fourier(gasoline, K = 11)),
  order = c(4, 0, 2), seasonal = c(1, 0, 0)
  )
gasoline_autoarima

checkresiduals(gasoline_autoarima)

gasoline.from2000 <- window(gasoline, start = 2000)
t.from2000 <- window(t, start = 2000)
t.pw1.from2000 <- window(t.pw1, start = 2000)
t.pw2.from2000 <- window(t.pw2, start = 2000)

AICc <- Inf
K_min.Aicc <- 0
for(num in c(1:26)){
  gasoline.from2000_tslm <- tslm(
    gasoline.from2000 ~ trend + t.pw1.from2000 + t.pw2.from2000 + fourier(
      gasoline.from2000, K = num
    )
  )
  AICc_value <- CV(gasoline.from2000_tslm)["AICc"]
  
  if(AICc > AICc_value){
    AICc <- AICc_value
  }else{
    K_min.Aicc <- num
    break 
  }
}
K_min.Aicc

gasoline.from2000_tslm

xreg.from2000 <- cbind(
  t = t.from2000, 
  t.pw1 = t.pw1.from2000, 
  t.pw2 = t.pw2.from2000,
  Fourier = fourier(
    gasoline.from2000, K = 11
    )
  )

gasoline.from2000_autoarima <- auto.arima(
  gasoline.from2000,
  xreg = xreg.from2000
)

gasoline.from2000_autoarima

checkresiduals(gasoline.from2000_autoarima)

gasoline.from2000_arima.6.0.1 <- Arima(
  gasoline.from2000,
  xreg = xreg.from2000,
  order = c(6, 0, 1)
)

checkresiduals(gasoline.from2000_arima.6.0.1)

h = 52
t.new <- t.from2000[length(t.from2000)] + seq(h)/365.25
t.pw1.new <- t.pw1.from2000[length(t.pw1.from2000)] + seq(h)/365.25
t.pw2.new <- t.pw2.from2000[length(t.pw2.from2000)] + seq(h)/365.25
xreg.new <- cbind(
  t = t.new, 
  t.pw1 = t.pw1.new, 
  t.pw2 = t.pw2.new,
  Fourier = fourier(
    gasoline.from2000, K = 11, h = h
    )
  )
fc_gasoline.from2000_arima.6.0.1 <- forecast(
  gasoline.from2000_arima.6.0.1,
  xreg = xreg.new,
  h = h
)
autoplot(fc_gasoline.from2000_arima.6.0.1)
