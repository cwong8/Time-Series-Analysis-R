setwd("C:/Users/Christopher/Desktop/STA 137/Final")
source("C:/Users/Christopher/Desktop/STA 137/Midterm/trndseas.R")
source("C:/Users/Christopher/Desktop/STA 137/Homework 4/aicc.R")
library(xlsx)
petroleum <- read.xlsx("petrolium.xlsx", sheetIndex = 1)

# 
# .	Explain the data, why it is a time series, why it is important to analyze it.
### Look at midterm

# .	Use graphical techniques to understand the nature of variation in the data.
plot.ts(petroleum$Petrolium.Consumption, 
        ylab = "Petroleum consumed (in trillion BTU)",
        main = "US residential petroleum consumption (Jan 1984 - Dec 2015)")

# .	Determine if the series is stationary or not. You may need to transform, estimate the trend and the seasonal effects in order to carry out the analysis. 
library(MASS)
a <- boxcox(petroleum$Petrolium.Consumption~1, lambda = seq(-2, 2, 0.001))
a$x[which.max(a$y)]

# 1/sqrt transformation?
# > a$y[which(a$x == -0.50)]
# [1] -563.9076
# > a$y[which(a$x == -1)]
# [1] -564.8943

transformPlots <- function(data){
  par(mfrow = c(2, 2))
  plot.ts(data^-2, main = expression(""*lambda*" = -2"))
  plot.ts(data^-1, main = expression(""*lambda*" = -1"))
  plot.ts(data^1, main = expression(""*lambda*" = 1"))
  plot.ts(data^2, main = expression(""*lambda*" = 2"))
  par(mfrow = c(1, 1))
  plot.ts(1/sqrt(data), main = expression(""*lambda*" = -0.5"))
}
transformPlots(petroleum$Petrolium.Consumption)


t.petroleum <- petroleum
t.petroleum$Petrolium.Consumption <- 1/sqrt(petroleum$Petrolium.Consumption)
mod <- trndseas(t.petroleum$Petrolium.Consumption, degtrnd = 3, seas = 12)
plot.ts(t.petroleum$Petrolium.Consumption,
        main = "1/sqrt US residential petroleum consumption (Jan 1984 - Dec 2015)")
points(mod$fit, type = 'l', col = 'red')
legend("topright", legend = c("Observed data", "Fitted smooth"), col = c("black", "red"), lty = 1)
## Really any degree 2, 3, 4 works fine. we go with 3

plot.ts(mod$trend, main = "Trend of 1/sqrt petroleum consumed", ylab = "1/sqrt Petroleum consumed (in trillion BTU)")
plot.ts(mod$season, main = "Seasonality of 1/sqrt petroleum consumed", ylab = "1/sqrt Petroleum consumed (in trillion BTU)")
rough <- t.petroleum$Petrolium.Consumption - mod$fit
plot.ts(rough, main = "Rough of 1/sqrt petroleum consumed", ylab = "1/sqrt Petroleum consumed (in trillion BTU)")

par(mfrow = c(1, 2))
qqnorm(rough)
qqline(rough)
hist(rough)
par(mfrow = c(1, 1))

Box.test(rough, lag = 10, type = "Ljung-Box")

acf(rough)
pacf(rough)
## Consistent with ARIMA

par(mfrow = c(2, 2))
Petroleum <- rough
petroleum.pgrm = spec.pgram(Petroleum)
petroleum.pgrm3 = spec.pgram(Petroleum, spans = 3)
petroleum.pgrm7 = spec.pgram(Petroleum, spans = 7)
petroleum.pgrm11 = spec.pgram(Petroleum, spans = 11)
par(mfrow = c(1, 1))







library(forecast)
mod1 <- auto.arima(rough, max.p = 8, max.q = 8)
res <- mod1$res
acf(res)
pacf(res)
Box.test(res, lag = 10, type = "Ljung-Box")
qqnorm(res); qqline(res)
# Yes, it is white noise

# Part c
library(astsa)
coef.ar <- mod1$coef[1:2]
coef.ma <- mod1$coef[3]
sigma2 <- mod1$sigma2
mod1spec <- arma.spec(ar = coef.ar, ma = coef.ma, var.noise = sigma2, log = "no",
                      ylim = c(min(petroleum.pgrm7$spec), max(petroleum.pgrm7$spec)))
lines(petroleum.pgrm$freq, petroleum.pgrm7$spec, type='l')

ARIMA.Fitter <- function(data, p = 5){
  Result <- sapply(0:p, function(i){
    sapply(0:p, function(j){
      fit <- arima(data, order = c(i, 0, j))
      return(list(fit, aicc(fit)))
    })
  })
  return(Result)
}
models <- ARIMA.Fitter(rough)



### Using first 372 observations to predict the last 12
n <- 372
h <- 12
mod0 <- trndseas(t.petroleum$Petrolium.Consumption[1:n], degtrnd = 3, seas = 12)
m.fit <- mod0$trend
s.fit <- rep(mod0$season, length.out = n)
smooth.fit <- mod0$fit
deg <- 3
coef = mod0$coef[1:(deg+1)]
time = (n+(1:h))/n
predmat = matrix(rep(time,deg)^rep(1:deg,each=h),nrow=h,byrow=F)
predmat = cbind(rep(1,h),predmat)

rough0 <- t.petroleum$Petrolium.Consumption[1:n] - mod0$fit
fit0 <- arima(rough0, order = c(2, 0, 1))

m.fc = predmat %*% coef
s.fc = rep(mod0$season,length.out=n+h)
s.fc = s.fc[-(1:n)]
fcast = predict(fit0,n.ahead=h)
x.fc = fcast$pred
y.fc = m.fc + s.fc + x.fc

plot.ts(t.petroleum$Petrolium.Consumption[1:n],xlim=c(0,n+h),
        ylab = "1/sqrt Petroleum consumed (in trillion BTU)",
        main = "1/sqrt US residential petroleum consumption (Jan 1984 - Dec 2015)")
points(x=n+1:h, y=y.fc, col='purple',type='b',pch=19)
points(x=n+1:h, y=t.petroleum$Petrolium.Consumption[373:384], col = "red", type = "l")
legend("topright", legend = c("Observed data", "Fitted smooth"),
       col = c("red", "purple"), lty = 1, pch = c(NA, 19))
t.result <- cbind(y.fc, t.petroleum$Petrolium.Consumption[373:384])
colnames(t.result) <- c("Forecast", "Actual")
t.result

# This is the inverse of 1/sqrt(x)!!!!!!!!!
a1 <- 1/(y.fc)^2
plot.ts(petroleum$Petrolium.Consumption[1:n],xlim=c(0,n+h),
        ylab = "Petroleum consumed (in trillion BTU)",
        main = "US residential petroleum consumption (Jan 1984 - Dec 2015)")
points(x=n+1:h, y=a1, col='purple',type='b',pch=19)
points(x=n+1:h, y=petroleum$Petrolium.Consumption[373:384], col = "red", type = "l")
legend("topright", legend = c("Observed data", "Fitted smooth"),
       col = c("red", "purple"), lty = 1, pch = c(NA, 19))
result <- cbind(a1, petroleum$Petrolium.Consumption[373:384])
colnames(result) <- c("Forecast", "Actual")
result


# .	Obtain the appropriate ACF, PACF plots and periodogram (and its smoothed version), and use these to make a preliminary identification of a time series model.
# .	Fit an ARIMA model obtained via preliminary identification, and examine the residuals and their properties.
# .	Select the final model using a model selection criterion such as AICC. [If you fit ARIMA(p,d,q), it may be enough to consider the 81 models with p=0,.,8 and q=0,.,8, where p is the AR order and q is the MA order. The R function auto.arima ('forecast package') may be used.]
# .	Plot the spectral density of the final model as well as the smoothed periodogram on the same graph.
# .	Perform the necessary diagnostics on the residuals of the final model including the ACF, PACF plots as well as the smoothed periodogram.
# .	Write down the final model, the estimated parameters and the standard errors.
# .	Refit the final model (i.e., use AR and MA orders of the final model, but not the parameter estimates) using all the data except for the year 2015 and use this model to forecast petroleum consumption for the 12 months in 2015. Plot the available observed and the forecasted values against time (12 months of 2015) on the same graph. [If you need to extrapolate the trend, often a linear extrapolation is reasonable.] 
# .	Summarize your findings from the analysis and explain your conclusion.  If you feel the analysis done by you can be improved, please provide a brief explanation.


