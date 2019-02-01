library(car)
library(faraway)
library(leaps)

#Data cleaning
a <- read.csv("E:/MS-ISE/2nd Semester/ISE 6450 Applied Linear Regression/Final Project/adult.csv")
str(a)
a <- a[1:3000,]
a

a[a=='?'] <- NA
anew <- na.omit(a)
str(anew)

#Full model fit
fit1 <- glm(income ~ age + workclass  + educational.num + marital.status +
occupation + relationship + race + gender + capital.gain +
  capital.loss + hours.per.week + native.country, data= anew, family=binomial(link="logit"))

summary(fit1)

#(To check if there is any 2nd order effect)

#Residuals age (no effect)
plot(anew$age,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fit1) ~ anew$age)
y  <- predict(rl,se=TRUE)

xvals <- anew$age
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))


#Residuals interaction gender and age (no effect)
plot(anew$age,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$gender], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# now we get a smoothed mean over age but now 
# we fit a separate mean function for males and for females
rl_m <- loess(residuals(fit1)[anew$gender == "Male"] ~ 
                anew$age[anew$gender == "Male"])
rl_f <- loess(residuals(fit1)[anew$gender == "Female"] ~ 
                anew$age[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$age
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

# plot confidence intervals and mean residual for males
polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

# plot confidence intervals and mean residual for females
polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))


#Residuals capital-gain (no effect)
plot(anew$capital.gain,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital gain")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# again, get mean of residuals versus age to see if 
# there are any trends we missed
rl <- loess(residuals(fit1) ~ anew$capital.gain)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.gain
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))


#Residual interaction gender and education (no effect)
plot(anew$educational.num,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$gender], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# now we get a smoothed mean over age but now 
# we fit a separate mean function for males and for females
rl_m <- loess(residuals(fit1)[anew$gender == "Male"] ~ 
                anew$educational.num[anew$gender == "Male"])
rl_f <- loess(residuals(fit1)[anew$gender == "Female"] ~ 
                anew$educational.num[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$educational.num
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

# plot confidence intervals and mean residual for males
polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

# plot confidence intervals and mean residual for females
polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))


#Residuals capital-gain (no effect)
plot(anew$capital.gain,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital gain")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# again, get mean of residuals versus age to see if 
# there are any trends we missed
rl <- loess(residuals(fit1) ~ anew$capital.gain)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.gain
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))

#Residuals capital-loss (no effect)
plot(anew$capital.loss,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital loss")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)


rl <- loess(residuals(fit1) ~ anew$capital.loss)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.loss
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))


#Residual hours per week (no effect)
plot(anew$hours.per.week,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fit1) ~ anew$hours.per.week)
y  <- predict(rl,se=TRUE)

xvals <- anew$hours.per.week
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))


#Residuals educational num (no effect)
plot(anew$educational.num,residuals(fit1), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fit1) ~ anew$educational.num)
y  <- predict(rl,se=TRUE)

xvals <- anew$educational.num
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))


#Model selection
#null model
fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))

#full model
fit1

#AIC
forwselaic <- step(fit0, scope=list(lower=fit0, upper=fit1), direction="forward",
                k = 2,trace=FALSE)
forwselaic

#BIC
forwselbic <- step(fit0, scope=list(lower=fit0, upper=fit1), direction="forward",
                k = log(nrow(anew)))
forwselbic


#new fitted model from BIC
fitnew <- glm(income ~ relationship + educational.num + capital.gain + 
               capital.loss + hours.per.week + age, family = binomial(link = "logit"), 
             data = anew)
summary(fitnew)


#Are there influential points?

halfnorm(rstudent(fitnew))
#1609 2737

fitnewr <- glm(income ~ relationship + educational.num + capital.gain + 
                 capital.loss + hours.per.week + age, family = binomial(link = "logit"), 
               data = anew,subset = c(-1609,-2737))

compareCoefs(fitnew,fitnewr)
#no influential points




#Residuals age (new model)
plot(anew$age,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fitnew) ~ anew$age)
y  <- predict(rl,se=TRUE)

xvals <- anew$age
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))


#Residuals interaction gender and age (no effect)
plot(anew$age,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$gender], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# now we get a smoothed mean over age but now 
# we fit a separate mean function for males and for females
rl_m <- loess(residuals(fitnew)[anew$gender == "Male"] ~ 
                anew$age[anew$gender == "Male"])
rl_f <- loess(residuals(fitnew)[anew$gender == "Female"] ~ 
                anew$age[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$age
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

# plot confidence intervals and mean residual for males
polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

# plot confidence intervals and mean residual for females
polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))


fitnewinteraction <- glm(income ~ relationship + educational.num + capital.gain + 
                           capital.loss + hours.per.week + age + age:gender, family = binomial(link = "logit"), 
                         data = anew)
summary(fitnewinteraction)



#Residuals capital-gain (no effect)
plot(anew$capital.gain,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital gain")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# again, get mean of residuals versus age to see if 
# there are any trends we missed
rl <- loess(residuals(fitnew) ~ anew$capital.gain)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.gain
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))


#Residual interaction gender and education (no effect)
plot(anew$educational.num,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$gender], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# now we get a smoothed mean over age but now 
# we fit a separate mean function for males and for females
rl_m <- loess(residuals(fitnew)[anew$gender == "Male"] ~ 
                anew$educational.num[anew$gender == "Male"])
rl_f <- loess(residuals(fitnew)[anew$gender == "Female"] ~ 
                anew$educational.num[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$educational.num
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

# plot confidence intervals and mean residual for males
polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

# plot confidence intervals and mean residual for females
polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))


fitnewgendereducation <- glm(income ~ relationship + educational.num + capital.gain + 
                               capital.loss + hours.per.week + age + gender:educational.num, family = binomial(link = "logit"), 
                             data = anew)

summary(fitnewgendereducation)

#Residuals capital-gain (no effect)
plot(anew$capital.gain,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital gain")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

# again, get mean of residuals versus age to see if 
# there are any trends we missed
rl <- loess(residuals(fitnew) ~ anew$capital.gain)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.gain
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))

#Residuals capital-loss (no effect)
plot(anew$capital.loss,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "capital loss")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)


rl <- loess(residuals(fitnew) ~ anew$capital.loss)
y  <- predict(rl,se=TRUE)

xvals <- anew$capital.loss
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))

fit0 <- glm(income ~ 1,data=anew,family=binomial(link="logit"))


#Residual hours per week (no effect)
plot(anew$hours.per.week,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fitnew) ~ anew$hours.per.week)
y  <- predict(rl,se=TRUE)

xvals <- anew$hours.per.week
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))



#Residuals educational num (no effect)
plot(anew$educational.num,residuals(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$income], cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Deviance Residuals", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("<50k", ">=50k"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)

rl <- loess(residuals(fitnew) ~ anew$educational.num)
y  <- predict(rl,se=TRUE)

xvals <- anew$educational.num
orderx <- order(xvals)

# also plot confidence intervals of this mean
polygon(c(rev(xvals[orderx]), xvals[orderx]), 
        c(rev((y$fit+2*y$se.fit)[orderx]), (y$fit-2*y$se.fit)[orderx]), 
        col = scales::alpha('grey50', 0.5), border = NA)

# plot the mean itself
lines(x = xvals[orderx], y = y$fit[orderx], lwd = 2, col = scales::alpha("black", 0.75))







#plotting some stuff

best <- regsubsets(income~relationship + educational.num + capital.gain + 
                     capital.loss + hours.per.week + age, 
                   data = anew,nbest=1,nvmax=6,really.big = TRUE)
best
plot(best,scale="adjr2")
plot(best,scale="Cp")
plot(best,scale="bic")

predict(fitnew,type="response")
par(mfrow=c(1,1))


#Predicted log odds plot of gender 

plot(anew$age,predict(fitnew), col = scales::alpha(c("blue","red"), 0.35)[anew$gender], 
     cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Predicted log-odds", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)


rl_m <- loess(predict(fitnew)[anew$gender == "Male"] ~ 
                anew$age[anew$gender == "Male"])
rl_f <- loess(predict(fitnew)[anew$gender == "Female"] ~ 
                anew$age[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$age
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))

p <- predict(fitnew)[-which(predict(fitnew)>20)]
a <- anew$age[-which(predict(fitnew)>20)]

plot(a,p, col = scales::alpha(c("blue","red"), 0.35)[anew$gender], 
     cex = 2, pch = 20, cex.axis = 1.5, cex.lab = 1.5,
     ylab = "Predicted log-odds", xlab = "Age")
abline(h=0,lty=2,col="grey", lwd = 2)

legend("bottomright", legend = c("Female", "Male"), 
       col = scales::alpha(c('blue', 'red'), 0.75),
       pch = 20, cex = 2)


rl_m <- loess(predict(fitnew)[anew$gender == "Male"] ~ 
                anew$age[anew$gender == "Male"])
rl_f <- loess(predict(fitnew)[anew$gender == "Female"] ~ 
                anew$age[anew$gender == "Female"])
y_m  <- predict(rl_m,se=TRUE)
y_f  <- predict(rl_f,se=TRUE)

xvals <- anew$age
xvals_m <- xvals[anew$gender == "Male"]
xvals_f <- xvals[anew$gender == "Female"]
orderx <- order(xvals)

orderx_m <- order(xvals[anew$gender == "Male"])
orderx_f <- order(xvals[anew$gender == "Female"])

polygon(c(rev(xvals_m[orderx_m]), xvals_m[orderx_m]), 
        c(rev((y_m$fit+2*y_m$se.fit)[orderx_m]), (y_m$fit-2*y_m$se.fit)[orderx_m]), 
        col = scales::alpha('red', 0.5), border = NA)

lines(x = xvals_m[orderx_m], y = y_m$fit[orderx_m], lwd = 2, col = scales::alpha("red", 0.75))

polygon(c(rev(xvals_f[orderx_f]), xvals_f[orderx_f]), 
        c(rev((y_f$fit+2*y_f$se.fit)[orderx_f]), (y_f$fit-2*y_f$se.fit)[orderx_f]), 
        col = scales::alpha('blue', 0.5), border = NA)

lines(x = xvals_f[orderx_f], y = y_f$fit[orderx_f], lwd = 2, col = scales::alpha("blue", 0.75))



#confusion table
yhat <- ifelse(predict(fitnew, type = "response") > 0.5, 
               ">50k", 
               "<=50k")
actual <- anew$income
yhat <- paste("Predicted:", yhat)
actual <- paste("Actual:", actual)
tabl <- table(Predicted = yhat, Actual = actual)
tabl

sum(diag(tabl))/sum(tabl)
