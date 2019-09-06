
library(fpp2)

autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales, lag.max = 400)

autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths, lag.max = 60)

autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq, lag.max = 200)

autoplot(sunspotarea)
# ggseasonplot(sunspotarea) 
# ggsubseriesplot(sunspotarea)
gglagplot(sunspotarea)
ggAcf(sunspotarea, lag.max = 50)

autoplot(gasoline)
ggseasonplot(gasoline)
# ggsubseriesplot(gasoline)
gglagplot(gasoline)
ggAcf(gasoline, lag.max = 1000)

autoplot(visnights[,"QLDMetro"]) 

train1= window(visnights[,"QLDMetro"], end = c(2015,4))
train2= window(visnights[,"QLDMetro"], end = c(2014,4))
train3= window(visnights[,"QLDMetro"], end = c(2013,4))

fc1 = snaive(train1)
fc2 = snaive(train2)
fc3 = snaive(train3)

accuracy(fc1,window(visnights[,"QLDMetro"], start = 2016, end = c(2016,4)))
accuracy(fc2,window(visnights[,"QLDMetro"], start = 2015, end = c(2015,4)))
accuracy(fc3,window(visnights[,"QLDMetro"], start = 2014, end = c(2014,4)))

par(mfrow=c(2,2))
plot(dowjones, main="Fig 12: Dow Jones Index")
plot(log(dowjones), main = "Fig 13: Log DJI")
plot(sqrt(dowjones), main = "Fig 14: SQRT")

dowjones_drift <- rwf(dowjones , h=10, drift=TRUE)
dowjones_drift_log <- rwf(log(dowjones), h = 10, drift = TRUE)
dowjones_drift_sqrt <- rwf(sqrt(dowjones), h =10, drift = TRUE)
par(mfrow=c(2,2))

plot(dowjones_drift,plot.conf=FALSE,main="Drift Method Dow Jones", ylab="Index",xlab="Year")
legend("topleft",lty=1, col=c(4),legend=c("SQRT"))
plot(dowjones_drift_log,plot.conf=FALSE,main="Log Method Dow Jones", ylab="Index",xlab="Year")
legend("topleft",lty=1, col=c(4),legend=c("Log"))
plot(dowjones_drift_sqrt,plot.conf=FALSE,main="SQRT Method Dow Jones", ylab="Index",xlab="Year")
legend("topleft",lty=1, col=c(4),legend=c("Drift"))

dj_first_last <- window(dowjones, start=1, end=66-.1)
dj_first_last_mean <- meanf(dj_first_last,h=12)
dj_first_last_1 <- rwf(dj_first_last,h=12)
dj_first_last_2 <- rwf(dj_first_last,h=12, drift = TRUE)
plot(dj_first_last_mean, plot.conf=FALSE, main="Dow Jones Index", xlim=c(1,78))
lines(dj_first_last_1$mean,col=2)
lines(dj_first_last_2$mean,col=3)
lines(dowjones)
legend("topleft", lty=1, col=c(4,2,3), legend=c("Mean ","Naive","Drifit"))

par(mfrow=c(1,1))
dowjones_drift <- rwf(dowjones , h=24, drift=TRUE)
dowjones_drift_mean <-meanf(dowjones, h=42)
dowjones_drift_naive <-naive(dowjones, h=42)

plot(dowjones_drift,plot.conf=FALSE,main="Drift Method Dow Jones", ylab="Index",xlab="Year")

lines(dowjones_drift_mean$mean, col=2)
lines(dowjones_drift_naive$mean, col=3)
legend("topleft",lty=1, col=c(4,2,3),legend=c("Mean Method","Naive Method","Drift"))

head(ibmclose)
summary(ibmclose)
par(mfrow=c(2,2))
plot(ibmclose)
qqnorm(ibmclose)
qqline(ibmclose)
plot(log(ibmclose))
plot(sqrt(ibmclose))

ibm_close_train <- window(ibmclose ,end=300)
ibm_close_test <- window(ibmclose ,start=301)

par(mfrow=c(1,1))
ibm_close_avg <- meanf(ibm_close_train,h=54)$mean
ibm_close_naive <- naive(ibm_close_train ,h=54)$mean
ibm_close_drift <- rwf(ibm_close_train ,drift=TRUE,h=54)$mean

plot(ibm_close_train,main="IBM Close Prices",xlab="Day",ylab="Price")

lines(ibm_close_naive,col=2)
lines(ibm_close_avg,col=4)
lines(ibm_close_drift,col=3)
lines(ibm_close_test,col=8)

legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean Method","Naive Method","Drift Method"))

plot(ibm_close_train,main="IBM Close Prices", ylab="Price",xlab="Day", xlim=c(250,369), ylim=c(300,425))
lines(ibm_close_naive,col=2)
lines(ibm_close_avg,col=4)
lines(ibm_close_drift,col=3)
lines(ibm_close_test,col=8)

legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean Method","Naive Method","Drift Method"))

checkresiduals(ibm_close_drift)

head(hsales)
summary(hsales)
par(mfrow=c(2,2))
plot(hsales)
qqnorm(hsales)
qqline(hsales)
plot(log(hsales))
acf(hsales)

monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),23)
monthdays <- monthdays[-275]
monthdays[38 + (4*12)*(0:4)] <- 29
plot(hsales/monthdays, ylab="Sales", xlab="Years")

training <- window(hsales, start=1973, end=1994-1/12)
test <- window(hsales, start=1994)

plot(training, ylab="Sales", xlab="Year", xlim =c(1973, 1995))
fit1 <- meanf(training, h=23)
fit2 <- rwf(training, h=23)
fit3 <- snaive(training, h=23)
fit4 <- rwf(training, h=23, drift=TRUE)
lines(fit1$mean, col=2)
lines(fit2$mean, col=3)
lines(fit3$mean, col=4)
lines(fit4$mean, col=5)
accuracy(fit1, test)
accuracy(fit2, test)
accuracy(fit3, test)
accuracy(fit4, test)
lines(test)

checkresiduals(fit3)

gasoline_until_2004 <- window(gasoline, end = 2005)
autoplot(gasoline_until_2004, xlab = "Year") +
  ggtitle("US finished motor gasoline product supplied") +
  xlab("Year") + ylab("million barrels per day") 
for(num in c(1, 2, 3, 5, 10, 20)){
  var_name <- paste("tslm_ft",
                    as.character(num),
                    "_gasoline_until_2004",
                    sep = "")

  assign(var_name,
         tslm(gasoline_until_2004 ~ trend + fourier(
           gasoline_until_2004,
           K = num
         ))
  )
  print(
    autoplot(gasoline_until_2004) +
      autolayer(get(var_name)$fitted.values,
                series = as.character(num)) +
      ggtitle(var_name) +
      ylab("gasoline") +
      guides(colour = guide_legend(title = "Number of Fourier Transform pairs")) +
      theme(legend.position="bottom")
  )
}

autoplot(gasoline_until_2004) +
  autolayer(tslm_ft1_gasoline_until_2004$fitted.values, series = "1") +
  autolayer(tslm_ft5_gasoline_until_2004$fitted.values, series = "2") +
  autolayer(tslm_ft10_gasoline_until_2004$fitted.values, series = "3") +
  autolayer(tslm_ft10_gasoline_until_2004$fitted.values, series = "5") +
  autolayer(tslm_ft20_gasoline_until_2004$fitted.values, series = "10") +
  autolayer(tslm_ft20_gasoline_until_2004$fitted.values, series = "20") +
  guides(colour = guide_legend(title = "Fourier Transform pairs")) +
  scale_color_discrete(breaks = c(1, 2, 3, 5, 10, 20)) +
  theme(legend.position="bottom")

for(i in c(1, 2, 3, 5, 10, 20)){
  tslm_ft_gasoline_until_2004.name <- paste(
    "tslm_ft", as.character(i), "_gasoline_until_2004",
    sep = ""
  )
  writeLines(
    paste(
      "\n", tslm_ft_gasoline_until_2004.name, "\n"
    )
  )
  print(CV(get(tslm_ft_gasoline_until_2004.name)))
}

min_AICc <- Inf
min_K_by_AICc <- 0
min_CV <- Inf
min_K_by_CV <- 0
AICc_K <- 0
CV_K <- 0

for(num in 1:26){
  AICc_K <- CV(
    tslm(
      gasoline_until_2004 ~ trend + fourier(gasoline_until_2004, K = num)
    )
  )[["AICc"]]
  CV_K <- CV(
    tslm(
      gasoline_until_2004 ~ trend + fourier(gasoline_until_2004, K = num)
    )
  )[["CV"]]

  if(num != 1){
    if(AICc_K >= min_AICc & CV_K >= min_CV){
      writeLines(
        paste("The number of Fourier Transform pairs to minimize AICc:",
              as.character(min_K_by_AICc)
        )
      )
      writeLines(
        paste("The number of Fourier Transform pairs to minimize CV:",
              as.character(min_K_by_CV)
        )
      )
      break
    }
  }
  
  if(AICc_K < min_AICc){
    min_AICc <- AICc_K
    min_K_by_AICc <- num
  }
  if(CV_K < min_CV){
    min_CV <- CV_K
    min_K_by_CV <- num
  }
}

tslm_ft7_gasoline_until_2004 <- tslm(
  gasoline_until_2004 ~ trend + fourier(
    gasoline_until_2004, 
    K = 7
  )
)

checkresiduals(tslm_ft7_gasoline_until_2004)

fc_gasoline_2005 <- forecast(
  tslm_ft7_gasoline_until_2004,
  newdata=data.frame(fourier(
    gasoline_until_2004, K = 7, h = 52)
  )
)

autoplot(fc_gasoline_2005) +
  autolayer(window(
    gasoline,
    start = 2004,
    end = 2006
  )
  ) +
  scale_x_continuous(limits = c(2004, 2006)) +
  theme(legend.position="bottom") 

stl_brick_fixed_st <- stl(bricksq, 
                          s.window = "periodic",
                          robust = TRUE)
autoplot(stl_brick_fixed_st) +
  ggtitle("STL with fixed seasonality")

stl_brick_changing_st <- stl(bricksq,
                             s.window = 5,
                             robust = TRUE)
autoplot(stl_brick_changing_st) +
  ggtitle("STL with changing seasonality")

autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(stl_brick_fixed_st),
            series = "Trend-cycle") +
  autolayer(seasadj(stl_brick_fixed_st),
            series = "Seasonally Adjusted Data") +
  ggtitle("brick production in Australia",
          subtitle = "fixed seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted")) +
  theme(legend.position="bottom")

autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(stl_brick_changing_st),
            series = "Trend-cycle") +
  autolayer(seasadj(stl_brick_changing_st),
            series = "Seasonally Adjusted Data") +
  ggtitle("brick production in Australia",
          subtitle = "changing seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted")) +
  theme(legend.position="bottom")

stl_brick_fixed_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast",
          subtitle = "fixed seasonality") +
  theme(legend.position="bottom")
stl_brick_changing_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast",
          subtitle = "changing seasonality") +
  theme(legend.position="bottom")

stlf_brick <- stlf(bricksq)
autoplot(stlf_brick)

checkresiduals(stlf_brick)

stlf_brick_robust <- stlf(bricksq, robust = TRUE)
autoplot(stlf_brick_robust) +
  theme(legend.position="bottom") 

checkresiduals(stlf_brick_robust)

trainset_brick <- subset(bricksq, end = length(bricksq) - 8)
testset_brick <- subset(bricksq, start = length(bricksq) - 7)
snaive_brick <- snaive(trainset_brick)
stlf_brick_part <- stlf(trainset_brick, robust = TRUE)

autoplot(bricksq, series = "Previous data") +
  geom_line(size = 1) +
  autolayer(stlf_brick_part, PI = FALSE, size = 1,
            series = "stlf") +
  autolayer(snaive_brick, PI = FALSE, size = 1,
            series = "snaive") +
  scale_color_manual(values = c("gray50", "blue", "red"),
                     breaks = c("Original data", "stlf", "snaive")) +
  scale_x_continuous(limits = c(1990, 1994.5)) +
  scale_y_continuous(limits = c(350, 550)) +
  guides(colour = guide_legend(title = "Data")) +
  ggtitle("stlf and snaive forecasts") +
  annotate(
    "rect",
    xmin=1992.75,xmax=1994.5,ymin=-Inf,ymax=Inf,
    fill="lightgreen",alpha = 0.3
  ) +
  theme(legend.position="bottom")

str(visitors)
head(visitors)
autoplot(visitors)
ggseasonplot(visitors)

visitors_train <- subset(visitors, 
                         end = length(visitors) - 24)
visitors_test <- subset(visitors,
                        start = length(visitors) - 23)
hw_mul_visitors_train <- hw(visitors_train,
                            h = 24,
                            seasonal = "multiplicative")

autoplot(hw_mul_visitors_train)

fc_ets_visitors_train <- forecast(ets(visitors_train), h = 24)
autoplot(fc_ets_visitors_train)

fc_ets_add_BoxCox_visitors_train <- forecast(
  ets(visitors_train, 
      lambda = BoxCox.lambda(visitors_train),
      additive.only = TRUE),
  h = 24
)
autoplot(fc_ets_add_BoxCox_visitors_train)

fc_snaive_visitors_train <- snaive(visitors_train, h = 24)
autoplot(fc_snaive_visitors_train)

fc_BoxCox_stl_ets_visitors_train <- visitors_train %>%
  stlm(
    lambda = BoxCox.lambda(visitors_train),
    s.window = 13,
    robust = TRUE,
    method = "ets"
  ) %>%
  forecast(h = 24)
autoplot(fc_BoxCox_stl_ets_visitors_train)

accuracy(hw_mul_visitors_train, visitors_test)
accuracy(fc_ets_visitors_train, visitors_test)
accuracy(fc_ets_add_BoxCox_visitors_train, visitors_test)
accuracy(fc_snaive_visitors_train, visitors_test)
accuracy(fc_BoxCox_stl_ets_visitors_train, visitors_test)

checkresiduals(fc_snaive_visitors_train)

fets_add_BoxCox <- function(y, h) {
  forecast(ets(
    y,
    lambda = BoxCox.lambda(y),
    additive.only = TRUE
  ),
  h = h)
}
fstlm <- function(y, h) {
  forecast(stlm(
    y, 
    lambda = BoxCox.lambda(y),
    s.window = frequency(y) + 1,
    robust = TRUE,
    method = "ets"
  ),
  h = h)
}
fets <- function(y, h) {
  forecast(ets(y),
           h = h)
  }
sqrt(mean(tsCV(visitors, snaive, h = 1)^2, na.rm = TRUE))
sqrt(mean(tsCV(visitors, fets_add_BoxCox, h = 1)^2,
          na.rm = TRUE))
sqrt(mean(tsCV(visitors, fstlm, h = 1)^2,
          na.rm = TRUE))
sqrt(mean(tsCV(visitors, fets, h = 1)^2, na.rm = TRUE))
sqrt(mean(tsCV(visitors, hw, h = 1, 
               seasonal = "multiplicative")^2,
          na.rm = TRUE))
