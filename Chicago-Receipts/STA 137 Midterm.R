setwd("C:/Users/Christopher/Desktop/STA 137/Midterm")
source("trndseas.R")
source("C:/Users/Christopher/Desktop/STA 137/Homework 4/aicc.R")
library(MASS)

chicago <- read.delim("C:/Users/Christopher/Desktop/STA 137/Midterm/chicago.txt", header=FALSE)
names(chicago) <- c("Average receipts per theater", "Date")

plot(1:nrow(chicago), chicago$`Average receipts per theater`, type = "l",
     main = "Avg receipts per theater for Chicago (Jan 3, 2003 to Apr 18, 2003")

a <- boxcox(chicago$`Average receipts per theater`~1)
a$x[which.max(a$y)]

transformPlots <- function(data){
  par(mfrow = c(2, 2))
  plot.ts(data^-2, main = expression(""*lambda*" = -2"))
  plot.ts(data^-1, main = expression(""*lambda*" = -1"))
  plot.ts(data^1, main = expression(""*lambda*" = 1"))
  plot.ts(data^2, main = expression(""*lambda*" = 2"))
  par(mfrow = c(1, 1))
  plot.ts(log(data), main = expression(""*lambda*" = 0"))
}

#### Log-transformed data from here on out
chicago$`Average receipts per theater` = log(chicago$`Average receipts per theater`)

mod <- trndseas(chicago$`Average receipts per theater`, degtrnd = 4)
plot.ts(chicago$`Average receipts per theater`)
points(mod$fit, type = 'l', col = 'red')

mod <- trndseas(chicago$`Average receipts per theater`, degtrnd = 4, seas = 7)
plot.ts(chicago$`Average receipts per theater`)
points(mod$fit, type = 'l', col = 'red')
legend("topright", legend = c("Observed data", "Fitted smooth"), col = c("black", "red"), lty = 1)

plot.ts(mod$trend, main = "Trend of average receipts per theater", ylab = "Average receipts per theater")
plot.ts(mod$season, main = "Seasonality of average receipts per theater", ylab = "Average receipts per theater")

rough <- chicago$`Average receipts per theater` - mod$fit
plot.ts(rough, main = "Rough of Average receipts per theater", ylab = "Average receipts per theater")
qqnorm(rough)
qqline(rough)
hist(rough)
acf(rough)
pacf(rough)
# It's an AR(1) model
Box.test(chicago$`Average receipts per theater`, lag = 10, type = "Ljung-Box")
# Reject assumption of independence

AR.Fitter <- function(data, p = 5){
  Result <- sapply(0:p, function(i){
    fit <- arima(data, order = c(i, 0, 0))
    return(list(fit, aicc(fit)))
  })
  return(Result)
}
models <- AR.Fitter(rough)

fit <- arima(rough, order = c(1, 0, 0))
acf(fit$residuals, main = "AR(1) fit residuals")
pacf(fit$residuals, main = "AR(1) fit residuals")


### Using first 99 observations to predict the last 7
n <- 99
h <- 7
mod0 <- trndseas(chicago$`Average receipts per theater`[1:n], degtrnd = 4, seas = 7)
m.fit <- mod0$trend
s.fit <- rep(mod0$season, length.out = n)
smooth.fit <- mod0$fit
deg <- 4
coef = mod0$coef[1:(deg+1)]
time = (n+(1:h))/n; time
predmat = matrix(rep(time,deg)^rep(1:deg,each=h),nrow=h,byrow=F)
predmat = cbind(rep(1,h),predmat); predmat

rough0 <- chicago$`Average receipts per theater`[1:n] - mod0$fit
fit0 <- arima(rough0, order = c(1, 0, 0))

m.fc = predmat %*% coef
s.fc = rep(mod0$season,length.out=n+h)
s.fc = s.fc[-(1:n)]
fcast = predict(fit0,n.ahead=h)
x.fc = fcast$pred
y.fc = m.fc + s.fc + x.fc
y.fc

plot.ts(chicago$`Average receipts per theater`[1:n],xlim=c(0,n+h), ylim = c(4, 9), ylab = "Average receipts per theater", main = "Avg receipts per theater for Chicago (Jan 3, 2003 to Apr 18, 2003")
points(x=n+1:h, y=y.fc, col='purple',type='b',pch=19)
points(x=n+1:h, y=chicago$`Average receipts per theater`[100:106], col = "red", type = "l")



### Using all observations to predict the last 7
m.fit <- mod$trend
s.fit <- rep(mod$season, length.out = n)
smooth.fit <- mod$fit
deg <- 4
coef = mod$coef[1:(deg+1)]
time = (n+(1:h))/n; time
predmat = matrix(rep(time,deg)^rep(1:deg,each=h),nrow=h,byrow=F)
predmat = cbind(rep(1,h),predmat); predmat

m.fc = predmat %*% coef
s.fc = rep(mod$season,length.out=n+h)
s.fc = s.fc[-(1:n)]
fcast = predict(fit,n.ahead=h)
x.fc = fcast$pred
y.fc = m.fc + s.fc + x.fc
y.fc

plot.ts(chicago$`Average receipts per theater`,xlim=c(0,n+h), ylim = c(4, 9), ylab = "Average receipts per theater", main = "Avg receipts per theater for Chicago (Jan 3, 2003 to Apr 18, 2003")
points(x=n+1:h, y=y.fc, col='purple',type='b',pch=19)


