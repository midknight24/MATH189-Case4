# Math 189 Data Analysis and Inference 
# Calibrating a Snow Gauge
# Longde Wang. Brianna Inghilterra, Kevin Griem, Dingran Lu, Jiawei Wang, Jiawei Wang
# Kevin Greim
library(ggplot2)
data <- read.table("C:/Users/Kevin/Documents/Math 189/Case Study 4/data.txt", header = TRUE)
lgain <- log(data$gain)
fit <- lm(formula = density~lgain, data = data)
summary(fit)
newdata1 <- data.frame(lgain = log(38.6))
newdata2 <- data.frame(lgain = log(426.7))
predict(fit, newdata1, interval = "predict")
predict(fit, newdata2, interval = "predict")
pred.int <- predict(fit, interval="prediction")
pred.lower <- pred.int[,2]
pred.upper <- pred.int[,3]
plot(lgain, data$density, xlab = "log(gain)", ylab = "Density", main = "Figure 2.1: Prediction intervals of density given log(gain)")
abline(fit)
lines(x = lgain, y = pred.lower, col = "red")
lines(x = lgain, y = pred.upper, col = "red")

#Brianna: Fitting
#gauge:
fit=lm(formula=density~loggain, data=newgauge)
plot(gauge)
abline(fit, col="red")
plot(fit$residuals)
abline(0, 0, col="red")
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")
Newgauge:Log(Gain)
loggain = log(gauge$gain)
newdata=data.frame(loggain=loggain,density=gauge$density)
newfit=lm(formula=density~loggain, data=newgauge)
plot(newgauge)
abline(newfit, col="red")
plot(newfit$residuals)
abline(0, 0, col="red")
hist(newfit$residuals)
qqnorm(newfit$residuals)
qqline(newfit$residuals, col="red")

#Longde Wang
dat = read.table('C:/Users/Longde Wang/Desktop/School/UCSD/Junior/2018 Spring/MATH289C/Homework/Homework 4/gauge.txt', header = TRUE)
density_tr = subset(dat, dat$density != 0.508)$density
gain_tr = subset(dat, dat$density != 0.508)$gain
dat_tr = data.frame(density_tr, gain_tr)
density_te = subset(dat, dat$density == 0.508)$density
gain_te = subset(dat, dat$density == 0.508)$gain
dat_te = data.frame(density_tr = density_te, gain_tr = gain_te)
# Ordinary least squares
fit = lm(formula = density_tr ~ gain_tr, data = dat_tr)
summary(fit)
prd = predict(fit, newdata = dat_te, interval = 'confidence')
prd
mse = mean((prd[, 1] - density_te)^2)
mse
fit_l = lm(formula = density_tr ~ log(gain_tr), data = dat_tr)
summary(fit_l)
prd_l = predict(fit_l, newdata = dat_te, interval = 'confidence')
prd_l
mse_l = mean((prd_l[, 1] - density_te)^2)
mse_l
plot(gain_tr, density_tr)
abline(fit, col = 'red')
plot(log(gain_tr), density_tr)
abline(fit_l, col = 'blue')
# Weighted least squares
w = 1 / gain_tr
fit_w = lm(formula = density_tr ~ gain_tr, data = dat, weights = w)
summary(fit_w)
prd_w = predict(fit_w, dat_te, interval = 'confidence')
prd_w
mse_w = mean((prd_w[, 1] - density_te)^2)
mse_w
plot(gain_tr, density_tr)
abline(fit, col="red")
abline(fit_w, col="blue")
# Local Regression
library(ggplot2)
fit_lo = loess(formula = density_tr ~ gain_tr, data = dat_tr)
summary(fit_lo)
prd_lo = predict(fit_lo, dat_te, interval = 'confidence')
prd_lo
mse_lo = mean((prd_lo - density_te)^2)
mse_lo
ss_density = sum(scale(density_tr, scale = FALSE)^2)
ss_res = sum(resid(fit_lo)^2)
pseudo_r_sq_lo = 1 - ss_res / ss_density
pseudo_r_sq_lo
ggplot(dat_tr, aes(gain_tr, density_tr)) + geom_point() + geom_smooth(method = lm, se =
                                                                        TRUE)
ggplot(dat_tr, aes(gain_tr, density_tr)) + geom_point() + geom_smooth(se = TRUE)
# Quadratic model
fit_q = lm(formula = density_tr ~ poly(gain_tr, 2), data = dat_tr)
summary(fit_q)
prd_q = predict(fit_q, dat_te, interval = 'confidence')
prd_q
mse_q = mean((prd_q[, 1] - density_te)^2)
mse_q
fit_q_log = lm(formula = density_tr ~ poly(log(gain_tr), 2), data = dat)
summary(fit_q_log)
prd_q_log = predict(fit_q_log, dat_te, interval = 'confidence')
prd_q_log
mse_q_log = mean((prd_q_log[, 1] - density_te)^2)
mse_q_log
# Cubic model
fit_c = lm(formula = density_tr ~ poly(gain_tr, 3), data = dat_tr)
summary(fit_c)
prd_c = predict(fit_c, dat_te, interval = 'confidence')
prd_c
mse_c = mean((prd_c[, 1] - density_te)^2)
mse_c
fit_c_log = lm(formula = density_tr ~ poly(log(gain_tr), 3), data = dat_tr)
summary(fit_c_log)
prd_c_log = predict(fit_c_log, dat_te, interval = 'confidence')
prd_c_log
mse_c_log = mean((prd_c_log[, 1] - density_te)^2)
mse_c_log
# Panel data model
#entity = as.factor(c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10), rep(6, 10), rep(7, 10), rep(8, 10), rep(9,10)))
#time = as.factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 9))
#panel_dat = data.frame(entity, time, density, gain)
#library(foreign)
#library(plm)
#fit_p = plm(formula = density ~ gain, data = panel_dat, model = 'random', index = c('entity', 'time'))
#summary(fit_p)

# Dingran Lu
data <- read.csv("~/Documents/R/math189/HW4/gauge-1wb1wa6.txt", sep="")
gain <- data$gain
density <- data$density
gaincross <- c(gain[1:20],gain[31:90])
densitycross <- c(density[1:20],density[31:90])
loggain = log(gaincross)
plot(loggain, densitycross, xlab = 'log of gain', ylab = 'density', main = 'scatter plot excluding data at density equales to 0.508')
data.cross <- data.frame(density = densitycross, log.gain = loggain)
crossfit <- lm(formula = density~log.gain, data = data.cross)
plot(loggain, densitycross, xlab = 'log of gain', ylab = 'density', main = 'scatter plot with fitted line excluding data at density equales to 0.508')
abline(crossfit, col="red")
plot(crossfit$residuals, xlab = 'observation index', ylab = 'residuals', main = 'residual plot exluding data at density equales to 0.508')
abline(0, 0, col="red")
hist(crossfit$residuals, xlab = 'residuals', ylab = 'counts', main = 'histogram of residuals excluding data at density equales to 0.508')
qqnorm(crossfit$residuals)
qqline(crossfit$residuals, col="red", main = 'Q-Q plot against normal distribution')
data.predict <- data.frame(log.gain = log(38.6))
predict(crossfit, newdata = data.predict, interval = 'prediction')

#Sam
#3.b
#CrossValidation
data=read.table("gauge.txt",header=TRUE)
data=subset(data,data$density!=0.001)
log_gain=log(data$gain)
fit=lm(formula = density~log_gain,data=data)
summary(fit)
attach(data)
plot(log_gain,data$density)
abline(fit,col="red")
res=resid(fit)
hist(res)
plot(res)
abline(h=0,col="red")
newdata=data.frame(log_gain=log(38.6))
predict(fit,newdata,interval="confidence")