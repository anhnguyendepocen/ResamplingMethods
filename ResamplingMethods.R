# Resampling Methods
# Fall 2014
# StatLab@UVa Library
# Clay Ford

# Writing a function in R:
# Clay: add something here


library(boot) 
library(bootstrap) # has data from book, An Introduction to the Bootstrap (Efron & Tibshirani, 1993)

# use data that comes from bootstrap package
# A small randomized experiment done with 7 mice (treatment group).
# Measurement unit is days of survival following surgery.
mouse.t
mean(mouse.t)
# assess normality
qqnorm(mouse.t)
qqline(mouse.t)

# take a bootstrap sample:
sample(mouse.t,replace=TRUE) 

# take the mean of the bootstrap sample:
mean(sample(mouse.t,replace=TRUE))

# take 200 bootstrap samples and evaluate the bootstrap replication (ie,
# calculate the mean for each bootstrap sample)
rep <- replicate(n = 200,expr = mean(sample(mouse.t,replace=TRUE)))

# rep contains the bootstrap replications
rep

# estimate SE by calculating sample standard deviation of the B=200
# replications:
sd(rep) # Bootstrap estimate of the standard error
sd(mouse.t)/sqrt(7) # standard error using formula

# How to do with boot() function (from the boot package)
# first define a function;
# In this case it needs two arguments:
# 1. data
# 2. indices which define the bootstrap sample
mean.fun <- function(data,ind)mean(data[ind])

# example of how function works
indices <- sample(length(mouse.t),replace=T) # generate indices
indices
mouse.t
mouse.t[indices]
mean(mouse.t[indices])
# what did you get?

# plug data and indices into function; returns mean of values selected by
# indices:
mean.fun(data=mouse.t,ind=indices) 

# now use in the boot function with R=200 replications:
boot(data=mouse.t, statistic=mean.fun, R=200)

# save results
bout <- boot(data=mouse.t, statistic=mean.fun, R=200)

# examine boot object:
str(bout) # does not actually have standard error
bout$t # bootstrap replicates

# standard error calculated when "printing" object
bout

# to extract the SE
sd(bout$t)

# should always examine graphical plot of the bootstrap replicates
# Do they look sensible? Check for discreteness.
plot(bout)

# see the frequency or index array for the bootstrap resamples:
boot.array(bout)
boot.array(bout, indices=TRUE)


# EXAMPLE: Standard Error of correlation coefficient

# Law school data. A random sample of size n=15 from the universe of 82 USA law
# schools (as of 1993). Two measurements: LSAT (average score on a national law
# test) and GPA (average undergraduate grade-point average).

law
plot(law)
cor(law)
cor(law$LSAT,law$GPA)

# correlation coefficient function
cor.fun <- function(dat,ind){
  cor(dat[ind,1],dat[ind,2])
}

# example of how function works
indices <- sample(nrow(law),replace=T) # generate indices
indices
law
law[indices,]
# plug data and indices into function; returns correlation of values selected by
# indices:
cor.fun(dat=law,ind=indices) 

bout2 <- boot(data=law, statistic=cor.fun, R=999)
bout2 
plot(bout2)
# skewed left; inferences based on the normal curve (such as traditional CIs's)
# are suspect when the bootstrap histogram is non-normal.

# to extract the SE
sd(bout2$t)

# Bootstrap estimates of SE ranging from 25 to 12800
B <- c(25, 50, 100, 200, 400, 800, 1600, 3200, 6400, 12800)
SEb <- numeric(10)
for(i in seq_along(B)){
  SEb[i] <- sd(boot(data=law, statistic=cor.fun, R=B[i])$t)
}
data.frame(B,SEb=round(SEb,3)) 


# compare bootstrap estimate to COMPLETE population of law schools
law82 

# 999 samples of size 15 drawn from the population
cors <- replicate(999,cor(law82[sample(82,15, replace=TRUE),-1])[1,2])

# Compare bootstrap replication histogram with sampling distribution from entire
# population
par(mfrow=c(1,2))
hist(bout2$t, breaks=20, xlim=c(0,1),main="Bootstrap") 
hist(cors, breaks=20, xlim=c(0,1), main="Random samples")
par(mfrow=c(1,1))

# Note the similarities between the distribution of bootstrap replications and
# the distribution of random samples from the population.

# Parametric bootstrapping
# Instead of sampling with replacement from the data, we draw B samples of size
# n from the parametric estimate of the population

# Assume law data came from bivariate normal population.

# Example of bivariate normal distribution
library(MASS) # has function mvrnorm()
library(mvtnorm) # has function dmvnorm()
# bivariate standard normal pdf
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
f <- function(x,y) dmvnorm(cbind(x,y))
z <- outer(x, y, f)
persp(x, y, z, expand=0.5,col="green",main = "Bivariate Standard Normal PDF", phi=50)


# summary stats for law
mean(law$LSAT)
mean(law$GPA)
cov(law)
range(law$LSAT)
range(law$GPA)

# assumed shape of distribution
x <- seq(500,700,length.out = 100)
y <- seq(200,400,length.out = 100)
f <- function(x,y) dmvnorm(cbind(x,y),
                           mean=c(mean(law$LSAT),mean(law$GPA)),
                           sigma=cov(law))
z <- outer(x, y, f)
persp(x, y, z, expand=0.5,col="green",main = "Assumed Bivariate Normal Distribution of Law Data",
      phi=50)


# draw 15 from the parametric estimate of the population
mvrnorm(n=15, 
        mu=c(mean(law$LSAT), mean(law$GPA)), 
        Sigma=cov(law))

# compute the correlation
cor(mvrnorm(n=15, 
            mu=c(mean(law$LSAT), mean(law$GPA)), 
            Sigma=cov(law)))[1,2]

# now draw 15 and compute correlation 999 times
pcors <- numeric(999)
for(i in 1:999){
  pcors[i] <- cor(mvrnorm(n=15, 
                          mu=c(mean(law$LSAT), mean(law$GPA)), 
                          Sigma=cov(law)))[1,2]
}

# parametric bootstrap estimate of standard error
sd(pcors) 

# parametric bootstrap using boot() function
# i.e., same as above, but more formal (and complicated)
# 1) define maximum likelihood estimates (mle)
law.mle <- list(c(mean(law$LSAT), mean(law$GPA)), cov(law))

# 2) write function to generate data
law.sim <- function(law, mle){
  n <- nrow(law) # 15
  mvrnorm(n, mu=mle[[1]], Sigma=mle[[2]]) # calls mle in boot() below
}

# 3) write function to calculate correlation
law.fun <- function(data, i=1:nrow(data)){
  d <- data[i,]
  cor(d[,1],d[,2])
}

# 4) run parametric bootstrap with boot() function;
# NOTE: requires sim = "parametric" and ran.gen and mle arguments;
pbout <- boot(law, law.fun, R=999, sim = "parametric",
              ran.gen=law.sim, mle=law.mle)

pbout
plot(pbout)


# Bootstrap estimate of Bias ----------------------------------------------

# Eight subjects wore medical patches designed to infuse a certain
# naturally-occuring hormone into the blood stream. Each subject had his blood
# levels of the hormone measured after wearing three different patches: a
# placebo patch, an "old" patch manufactured at an older plant, and a "new"
# patch manufactured at a newly opened plant.

data(patch)
head(patch)
# z = oldpatch - placebo
# y = newpatch - oldpatch

# The purpose of the study was to show bioequivalence, defined as:
# ratio mean(y)/mean(z) <= 0.20
# want the new facility to match the old facility within 20%

# ratio estimate
mean(patch$y)/mean(patch$z)

# what's the SE of this estimate? Is it biased?

# write a function for boot()
ratio.fun <- function(dat, ind){
  tmp <- dat[ind,]
  mean(tmp$y)/mean(tmp$z)
}

# do the bootstrap
bout <- boot(patch, ratio.fun, R=400)
bout

# bias
mean(bout$t) - bout$t0

# ratio of estimated bias to standard error
(mean(bout$t) - bout$t0)/sd(bout$t)

# As a rule of thumb, a bias less than 0.25 standard errors can be ignored.


# The Jackknife -----------------------------------------------------------

# use the jackknife() function in the bootstrap package

# simple example
mouse.t

# find jackknife values for the sample median
jackknife(mouse.t, median)

# example with complex data structure (ratio)
# first write a function; have to use "x" for indices
ratio.fun <- function(x, dat){
  mean(dat[x,6])/mean(dat[x,5])
}

# find jackknife values for the sample ratio
jackknife(1:nrow(patch), ratio.fun, patch)
# Compare to bootstrap values
bout


# The jackknife-after-bootstrap -------------------------------------------


# A bootstrap diagnostic for estimating the variability 
# from a set of bootstrap estimates.

# Calculate the jackknife influence values from a bootstrap output object and
# plot the corresponding jackknife-after-bootstrap plot.
# Recall: bout = bootstrap object for the patch data
jack.after.boot(bout)

# The x-axis are the jackknife influence values
# The y-axis are the centered jackknife quantiles
# horizontal dashed lines are the quantiles of the centered bootstrap distribution

# vertical asterisks are the quantiles of the centered bootstrap distribution
# when the indicated observation is removed

# How to calculate centered jackknife quantiles and jackknife influence values

alpha <- c(0.05, 0.1, 0.16, 0.5, 0.84, 0.9, 0.95)

# (1) get the bootstrap replicates
br <- bout$t
# (2) get number of rows in data set
n <- nrow(patch)
# (3) get the frequency array for the bootstrap resamples
f <- boot.array(bout)
# (4) create a placeholder matrix for jackknife quantiles
percentiles <- matrix(data = NA, length(alpha), n)
# (5) create a placeholder for jackknife influence values
J <- numeric(n)

# (6) calculate Jackknife quantiles with jth value removed
for (j in 1:n) {
  values <- br[f[, j] == 0] # bootstrap replicates calculated without observation j
  J[j] <- mean(values) # mean of bootstrap replicates with obs j missing
  
  # centered jackknife quantiles w/obs j missing
  percentiles[, j] <- quantile(values,  sort(alpha, decreasing = T)) - J[j] 
}

# (7) calculate jackknife influence values
J <- (n - 1) * (mean(J) - J) 
# (8) standardize jackknife influence values
J <- J/sqrt(var(J)) 

J
percentiles

# Bootstrap confidence intervals ------------------------------------------

# recall correlation coefficient of law school data
cor(law)
corr(law) # function in boot package

# we used bootstrap to estimate standard error
bout2
plot(bout2)

# use the boot.ci() function to calculate CI (defaults to 95% CI)
boot.ci(bout2, type="perc")
boot.ci(bout2, type="bca")
# Note the warning: "Some BCa intervals may be unstable"
# means intervals are using the upper or lower 10 order statistics

ciout <- boot.ci(bout2, type="bca")
ciout$bca # 2nd and 3rd are indices of the order statistics 
ciout$bca[2]<=10
ciout$bca[3]>= (999-9)

# to get rid of warning, can increase R or narrow the interval
boot.ci(bout2, type="bca", conf=0.90)

# can do more than one interval type
boot.ci(bout2)
boot.ci(bout2, type="all") # same thing
boot.ci(bout2, type=c("perc","bca"))

# In case you're interested, how "basic" bootstrap calculated:
# Basic bootstrap method
boot.ci(bout2, type="basic")
2*bout2$t0 - sort(bout2$t)[(999+1)*(1-0.025)]
2*bout2$t0 - sort(bout2$t)[(999+1)*0.025]

# In case you're interested, how "normal" bootstrap calculated:
# Normal bootstrap method
boot.ci(bout2, type="norm")
bias <- mean(bout2$t)-bout2$t0 # calculate bias
(bout2$t0 - bias) + c(sd(bout2$t)*-1.96, sd(bout2$t)*1.96)

# Thirteen accident victims have had the strength of their teeth measured, It is
# desired to predict teeth strength from measurements not requiring destructive
# testing. Four such variables have been obtained for each subject, (D1,D2) are
# difficult to obtain, (E1,E2) are easy to obtain.

data(tooth)
head(tooth)
# Question: how do D and E variables compare as predictors of strength?

modD <- lm(strength ~ D1 + D2, data=tooth) # difficult to obtain
summary(modD)
modE <- lm(strength ~ E1 + E2, data=tooth) # easy to obtain
summary(modE)

# calculate residual squared error; smaller is better
RSED <- sum(modD$residuals^2) # RSE(D)
RSEE <- sum(modE$residuals^2) # RSE(E)

# comparison statistic: 1/n * [RSE(D) - RSE(E)]
(RSEE - RSED)*(1/nrow(tooth)) 

# positive value -> E not as good as D;
# D appears to be better, but need to
# understand its statistical variability;
# use a confidence interval to answer this question

# compare predictions from both models
plot(modD$fitted.values, modE$fitted.values,xlim=c(34,36.5),ylim=c(34,36.5),
     xlab="D fit", ylab="E fit")
abline(0,1)

# write a function for boot()
RSEdiff <- function(dat,ind){
  modD <- lm(strength ~ D1 + D2, data=dat[ind,]) 
  modE <- lm(strength ~ E1 + E2, data=dat[ind,]) 
  RSED <- sum(modD$residuals^2) 
  RSEE <- sum(modE$residuals^2) 
  # statistic of interest:
  (RSEE - RSED)*(1/nrow(dat))
}
outb <- boot(tooth, RSEdiff, R=2000)
outb # note the standard error
# point estimate less than one standard error above 0,
# but estimate is biased downward:
mean(outb$t) - outb$t0
sum(outb$t<outb$t0)/2000 # about 60% less than point estimate
plot(outb)

boot.ci(outb, conf=c(0.90,0.95), type=c("perc","bca"))
jack.after.boot(outb)


# Permutation Tests and Bootstrap Hypothesis Testing ----------------------


# Again, let's use the mouse data.
# Having observed these two sets of values from treatment and control,
# we wish to test the null hypothesis they were drawn from populations
# with identical probability distributions.

# Say mouse.t was drawn from distribution F,
# and mouse.c was drawn from distribution G.
# We want to test F = G, (ie, same probability distributions)

# A traditional test: the T-Test
# assumes F and G are normally distributed with possibly different means:
t.test(mouse.t, mouse.c,alternative="greater",var.equal=TRUE)

# Now, let's do a permutation test.
# It makes no assumptions about distribution of F and G.

# Process: 
# 1. combine n + m observations from both groups
# 2. sample n observations WITHOUT replacement and place in one group
# 3. place remaining m observations in another group
# 4. compute difference between group means.
# 5. repeat steps 1-4 many times
# 6. if original difference falls outside middle 95% of 
#    dist'n of differences, then reject null.

all.mouse  <- c(mouse.t, mouse.c) 
n <- length(mouse.t)
m <- length(mouse.c)
diff <- numeric(1000)
for(i in 1:1000){
  s <- sample(n + m, n, replace = FALSE)
  diff[i] <- mean(all.mouse[s]) - mean(all.mouse[-s])
}

# compute achieved significance level (p-value)
sum(diff >= (mean(mouse.t) - mean(mouse.c)))/1000
# this also works
mean(diff >= (mean(mouse.t) - mean(mouse.c)))

# plot distribution of differences
hist(diff, breaks=30, main="Permutation Test")
abline(v=(mean(mouse.t) - mean(mouse.c)), lty=2)

# can use the perm package for this, which has permTS() function
library(perm)
permTS(mouse.t, mouse.c, alternative ="greater", method="exact.mc",
       control=permControl(nmc=1000))
# complete enumeration of all possibilities
permTS(mouse.t, mouse.c, alternative ="greater", method="exact.ce")


# Bootstrap Hypothesis Testing 

# let's analyze the same problem using Bootstrap Hypothesis Testing.
# Recap: we have samples z and y from possibly different probability 
# distributions F and G. We want to test null hypothesis F = G 
# (ie, the two populations are identical)

# Under the null, we assume both groups were drawn from a common population.
# To test with a bootstrap, we combine both groups into one sample and 
# use it an estimate of the common population that gave rise to both groups.

# Process:
# 1. combine n + m observations from both groups (the commone population estimate)
# 2. sample n + m observations WITH REPLACEMENT
# 3. put the first n members in one group and the remaining m members in another group
# 4. compute difference between group means.
# 5. repeat steps 1-4 many times
# 6. if original difference falls outside middle 95% of 
#    dist'n of differences, then reject null.

mouse.t 
mouse.c 
mean(mouse.t) - mean(mouse.c)

# combine data for bootstrap calculations
all.mouse  <- c(mouse.t, mouse.c) 
m <- length(mouse.t) # get size of first sample
s <- seq_len(n) # seq_len(n) returns 1,2,...,n

# function for boot() that calculates difference of means;
# we're testing if the two populations are identical, so I name it same.pop
same.pop <- function(dat, ind){
  tmp <- dat[ind]
  mean(tmp[s]) - mean(tmp[-s])
}

# do the bootstrap
hbout <- boot(all.mouse, same.pop, R=1000)
# calculate p-value (or achieved significance level)
# number of times bootstrap replicate equaled or exceeded original estimate
mean(hbout$t >= hbout$t0)

plot(hbout)


# more accurate testing can be obtained by bootstrapping the t-statistic.

# classic two-sample t test assumng equal variances
t.test(mouse.t, mouse.c, var.equal = TRUE)
# get just the t-statistic
t.test(mouse.t, mouse.c, var.equal = TRUE)$statistic

# define a function for boot
t.stat <- function(dat,ind){
  tmp <- dat[ind]
  t.test(tmp[s], tmp[-s], var.equal = TRUE)$statistic
}

# do the bootstrap
htbout <- boot(all.mouse, t.stat, R=1000)
# calculate p-value (or achieved significance level)
# number of times bootstrap replicate equaled or exceeded original estimate
mean(htbout$t >= htbout$t0)

plot(htbout)


# Bootstraping Regression Models ------------------------------------------


# cars

plot(cars, main = "Stopping Distance versus Speed")
lines(lowess(cars))

library(car)
scatterplot(cars$speed, cars$dist)


mod1 <- lm(dist~speed,data=cars)
coef(mod1)

# case resampling
mod.fun <- function(dat, ind){
  mod1 <- lm(dist~speed,data=dat[ind,])
  coef(mod1)
}

# test function
s <- sample(nrow(cars), replace=TRUE)
mod.fun(cars,s)

# use boot() for case resampling
rbout <- boot(cars, mod.fun, R=1000)
rbout
summary(mod1)

# residual resampling
mod.fun2 <- function(dat, ind){
  dat$y <- fitted(mod) + resid(mod1)[ind]
  mod2 <- lm(y~speed,data=dat)
  coef(mod2)
}

# test function
s <- sample(nrow(cars), replace=TRUE)
mod.fun2(cars,s)

# use boot() for residual resampling
rbout2 <- boot(cars, mod.fun2, R=1000)
rbout2
summary(mod1)

# the car package has the Boot() function that makes
# basic bootstrapping of regression models very easy.
# Note the capital "B": boot() vs. Boot()
Boot(mod1, R=1000, method="case")
Boot(mod1, R=1000, method="residual")

# cell data: Data on cell survival under different radiation doses.
library(MASS) # for robust regression function rlm()

head(cell)
par(mfrow=c(1,2))
plot(cell, main="with outlier") # note the outlier
identify(x=cell[,1],y=cell[,2])
m1 <- lm(log.surv ~ -1 + dose + I(dose^2), data=cell)
m2 <- rlm(log.surv ~ -1 + dose + I(dose^2), data=cell)
# add fitted lines
lines(cell[,1], fitted(m1))
lines(cell[,1], fitted(m2), lty=2)
# remove 13th obs and refit
m3 <- lm(log.surv ~ -1 + dose + I(dose^2), data=cell[-13,])
m4 <- rlm(log.surv ~ -1 + dose + I(dose^2), data=cell[-13,])
# replot and add fitted lines
plot(cell[-13,], main="with outlier removed") # outlier removed
lines(cell[-13,1], fitted(m3))
lines(cell[-13,1], fitted(m4), lty=2)

# it doesn't make sense to bootstrap standard errors for lm objects;
# if errors are close to normal, standard theory suffices;
# if errors are not normal, use a different method;
# rlm() uses M-estimation, which is based on aymptotic theory,
# therefore makes sense to bootstrap the standard errors

rrbout <- Boot(m2, R=1000, method="case")
# To minimize warnings, increase maxit from 20 to something higher
# in the rlm() call; see help(rlm)

summary(rrbout)
# compare to standard errors returned by rlm()
summary(m2)

# bootstrap confidence intervals using confint() from car package
confint(rrbout, type="bca")
confint(rrbout, type="perc")

# bootstrap diagnostics
par(mfrow=c(2,1))
jack.after.boot(rrbout, index=1, main="dose")
jack.after.boot(rrbout, index=2, main="dose^2")
par(mfrow=c(1,1))


# Cross Validation --------------------------------------------------------


cars
# The speed of cars and the distances taken to stop. 
plot(cars)
nrow(cars)
mod <- lm(dist ~ speed, data=cars)
summary(mod)
summary(mod)$sigma # residual standard error
summary(mod)$sigma^2 # residual squared error

# residual squared error "by hand"
# sum((observed - predicted)^2)/n - p
sum((cars$dist - mod$fitted)^2)/(nrow(cars) - length(mod$coefficients))

# cross validation (5 fold)
# 50/5 = 10 (5 groups of 10)
# first randomly assign rows to one of 5 groups
g <- rep(1:5,each=10)
cars$g <- sample(g)


# 5-fold cross validation
mse.5 <- numeric(5)
for(i in 1:5){
  tmp.mod <- lm(dist ~ speed, data=cars, subset= g!=i) # build model from all but i-th group
  tmp.pred <- predict(tmp.mod, newdata=subset(cars,g==i)) # predict results using i-th group
  mse.5[i] <- sum((cars$dist[cars$g==i] - tmp.pred)^2)/sum(cars$g==i) # calculate error mean square
}
sum(mse.5)/5 # 5-fold CV test error

# LOOCV cross validation
mse.loo <- numeric(50)
for(i in 1:50){
  tmp.mod <- lm(dist ~ speed, data=cars[-i,],) # build model from all but i-th row
  tmp.pred <- predict(tmp.mod, newdata=cars[i,]) # predict results using i-th row
  mse.loo[i] <- (cars$dist[i] - tmp.pred)^2 # calculate error mean square
}
sum(mse.loo)/nrow(cars) # Leave-one-out CV test error


# faster way with cv.glm() function (in boot package)
mod2 <- glm(dist ~ speed, data=cars) # Note: need to use glm()
cv.glm(cars, mod2, K=5)$delta # 5-fold CV
cv.glm(cars, mod2)$delta # LOOCV

# first number is CV estimate; 2nd number is bias-corrected version;
# LOOCV is always the same but K-fold CV varies.


# Example from An Intro to the Boostrap (p. 238)

data(hormone)
# Amount in milligrams of anti-inflammatory hormone remaining in 27 devices, 
# after a certain number of hours of wear. The devices were sampled from 3
# different manufacturing lots, called A, B and C.
head(hormone)
plot(amount ~ hrs, data=hormone, pch=Lot)

# fit regression lines to data with different intercepts but common slope:
mod <- lm(amount ~ hrs + factor(Lot) - 1, data=hormone)
summary(mod)

# plot the fit with base R graphics
plot(amount ~ hrs, data=hormone, pch=19, col=as.integer(factor(Lot)))
abline(a=coef(mod)[2],b=coef(mod)[1],col=1) # a = intercept, b = slope
abline(a=coef(mod)[3],b=coef(mod)[1],col=2)
abline(a=coef(mod)[4],b=coef(mod)[1],col=3)
legend("topright", pch=19, c("A","B","C"), col=unique(as.integer(factor(hormone$Lot))), lty=1)

# plot the fit with ggplot
library(ggplot2)
df <- data.frame(amount=predict(mod), hrs=hormone$hrs, Lot=hormone$Lot)
ggplot(hormone, aes(x=hrs,y=amount,color=Lot)) + geom_point() +
  geom_line(data=df)
  
# Two Questions:
# 1) How well will model predict amount of hormone remaining for new device?
# 2) Does this model predict better (or worse) than single regression line?

# answering first question:
# could look at residual SD (or residual variance):
summary(mod)$sigma
summary(mod)$sigma^2
# but this is too optimistic; using same data to assess model fit that we used
# to fit the model.

# cross-validation can help us obtain a more realistic estimate of prediction
# error.

# leave-one-out cross validation (LOOCV)
# fit a model leaving first observation out:
mod1 <- lm(amount ~ hrs + factor(Lot) - 1, data=hormone[-1,])
# compute predicted value for observation left out
predict(mod1,newdata=hormone[1,])
hormone[1,"amount"] # observed value

# now do for each observation and compute average cross-validation sum of
# squares
n <- nrow(hormone)
pv <- numeric(n)
for(i in 1:n){
  tmp <- lm(amount ~ hrs + factor(Lot) - 1, data=hormone[-i,])
  pv[i] <- predict(tmp,newdata=hormone[i,])
}
# calculate CV
sum((hormone$amount - pv)^2)/n

# compare to unbiased estimate of residual variance
summary(mod)$sigma^2

# again, easier to use cv.gml()
# default is leave-one-out CV
gmod <- glm(amount ~ hrs + factor(Lot) - 1, data=hormone)
cv.glm(hormone, gmod)$delta

# plot of residuals and cross-validated residuals;
# notice the CV residuals are equal to or larger than the usual residuals
plot(hormone$hrs,mod$residuals, ylim=c(3.5,-3.5))
abline(h=0)
points(hormone$hrs,(hormone$amount - pv),pch="*")


# residual squared error by Lot
pv # LOOCV predicted values
# calculate squared deviations: (observed - predicted)^2
cvsq <- (hormone$amount - pv)^2
tapply(cvsq, hormone$Lot, mean)
# amounts for devices in lot B are more difficult to predict

# Answering second question:
# Does this model predict better (or worse) than single regression line?
# again CV can help with this:
gmod2 <- glm(amount ~ hrs, data=hormone) # does not account for Lot
cv.glm(gmod2, data=hormone)$delta
cv.glm(gmod, data=hormone)$delta
# yields a quantitative measure for the price we pay not accounting for Lot.


# There are other simple estimates of prediction error.
# residual squared error:
RSE <- sum((hormone$amount - predict(mod))^2)

# average residual squared error
RSE/n

# unbiased residual squared error
# RSE/(n - p)
RSE/(n - 4)

# unbiased residual squared error: using R output
summary(mod)$sigma^2

# adjusted residual squared error; p=4 in the example (4 predictors)
# RSE/(n - 2p)
RSE/(n - 2*4)

# C_p statistic
# RSE/n + 2*p*[RSE/(n - p)]/n
RSE/n + 2*4*summary(mod)$sigma^2/n

# BIC (Bayesian Information Criterion)
# RSE/n + log(n)*p*[RSE/(n - p)]/n
RSE/n + log(n)*4*summary(mod)$sigma^2/n

# Why bother with CV when simple alternatives are available?
# Number of parameters "p" not always known in more complicated modeling;
# C_p and BIC require this, but CV does not.
# An example of more complicated modeling follows.


# CV with regression trees (recursive partitioning)

# regression trees are fairly easy to interpret and have a 
# nice graphical representation.

# Example of classification tree:
library(tree)
head(iris)
ir.tr <- tree(Species ~ ., iris)
plot(ir.tr); text(ir.tr) # produces decision tree

# Let's use CV to build a regression tree for 
# The Breast Cancer Data Set 
# from the UCI Machine Learning Repository
# http://archive.ics.uci.edu/ml/datasets/Breast+Cancer
breast <- read.csv("breast-cancer.data", header=F, na.strings = "?")
names(breast) <- c("class", "age", "menopause", "tumor.size","inv.nodes","node.caps",
                   "deg.malig","breast","breast.quad","irradiat")
str(breast)
summary(breast)

# a traditional appraoch:
# logistic regression
lmod <- glm(class ~ ., data=breast, family=binomial)
summary(lmod)
low.predict <- predict(lmod, type="response") # returns predicted probability
low.predict <- factor(ifelse(low.predict>0.5, 1, 0)) # convert to binary
t1  <- table(low.predict, breast$class[complete.cases(breast)]) # confusion matrix
t1
(t1[1,1] + t1[2,2])/sum(t1)
1 - (t1[1,1] + t1[2,2])/sum(t1) # misclassification rate 

# another approach:
# build classification tree to predict recurrence-events
# Now build a large tree;
# predict using all variables; 
tree.breast <- tree(class ~ ., data=breast)
plot(tree.breast); text(tree.breast, pretty=0)
summary(tree.breast)

# now use 10-fold cross-validation to determine optimal size of tree;
set.seed(4321)
cv.breast <- cv.tree(tree.breast, FUN=prune.misclass, K=10)
names(cv.breast)
# size = number of terminal nodes
# dev = number of CV errors 
# k = cost-complexity parameter (tuning parameter); 
#     controls trade-off between complexity and fit
cv.breast

# plot error rate as a function of tree size
plot(cv.breast)
plot(cv.breast$size,cv.breast$dev,type="b")
# identify size associated with lowest number of CV errors (dev)

size <- cv.breast$size[which.min(cv.breast$dev)] # tree size with fewest misclassifications


# Note CV results can vary
# do 25 cross-validations:
cv.results <- numeric(25)
for(i in 1:25){
  cv.tmp <- cv.tree(tree.breast, FUN=prune.misclass, K=10)
  cv.results[i] <- cv.tmp$size[which.min(cv.tmp$dev)]
}
table(cv.results)


# prune tree using CV results (best = size)
prune.breast <- prune.misclass(tree.breast, best=size)
plot(prune.breast)
text(prune.breast, pretty=0)
summary(prune.breast)
prune.breast


# bagging and random forests
library(randomForest)
# Bagging (ie, Bootstrap aggregation)
# construct B regression trees using B bootstrapped training sets,
# and average the resulting predictions;
# B=500 by default; use ntree argument to change;
# Note: bagging is random forest with m=p
set.seed(1984)
bag.breast <- randomForest(class ~ . , data=breast, na.action=na.omit,
                             mtry=9, importance=TRUE)
bag.breast

# random forest (m = p/3 by default)
set.seed(5150)
rf.breast <- randomForest(class ~ . , data=breast, na.action=na.omit,
                            importance=TRUE)
rf.breast

# bagging and random forest sacrifice interpretation for prediction accuracy;
# Use varImpPlot() to get an idea of which predictors are most important
varImpPlot(bag.breast, type=1, main="Bagging Importance Measures")
varImpPlot(rf.breast, type=1, main="Random Forest Importance Measures")



#####################################################################

# BONUS MATERIAL
# Bootstrapping time series data

# ch 8, An Intro to the Bootstrap

# Look at the following data.
# A regular time series giving the luteinizing hormone in 
# blood samples at 10 mins intervals from a human female, 48 samples.
# data come with R:
str(lh) 
lh

# Fig 8.4
plot(lh, type="l", xlab="time period", ylab="hormone level")
abline(h=mean(lh),lty=2)

# It is clear these data are not a random sample from any distribution.
# These data are a time series.

# Fit a first-order autoregressive time series model to the data using ar().
# First order scheme: z_t = beta*z_t-1 + e_t
# Each z_t is a linear combination of the previous value and an error term.
# beta is unknown parameter between -1 and 1.
# errors are assumed to be a random sample from unknown distribution with mean 0.

# Use least-squares to estimate beta:
mod1 <- ar(x=lh, aic=FALSE, order.max=1, method="ols")
mod1
ar(x=lh-mean(lh), aic=FALSE, order.max=1, method="ols") # same thing

# How accurate is our estimate of 0.586? Let's bootstrap SE.
# we can use the bootstrap to answer this question.

# we will recursively boostrap the residuals:
ar(x=lh, aic=FALSE, order.max=1, method="ols")$resid
# Fig 8.5
hist(mod1$resid)

# initial value is z_1 = y1 - y-bar
# then we bootstrap recursively:
# z_2* = beta*z_1 + error_2*
# z_3* = beta*z_1 + error_3*
# z_4* = beta*z_1 + error_4*
# ....


# what we need for bootstrapping
mod1$ar[1] # beta
mod1$resid # errors (disturbances)

# walk through start of one bootstrap sample:
# 1) sample disturbances
distb <- sample(mod1$resid[-1], replace=TRUE)
beta <- mod1$ar[1]

# 2) bootstrap recursively:
# start here
z1 <- lh[1] - mean(lh)
# start of bootstrap time series
z2 <- beta*z1 + distb[1]
z3 <- beta*z2 + distb[2]
z4 <- beta*z3 + distb[3]
z1;z2;z3;z4

# now let's do a single bootstrap replication:
zstar <- numeric(length(mod1$resid[-1])) # time 1 has no residual in this model
zstar[1] <- lh[1] - mean(lh)
for(i in 2:length(mod1$resid[-1])){
  zstar[i] <- beta*zstar[i-1] + distb[i]  
}
# now estimate b-hat
ar(zstar, aic=FALSE, order.max=1, method="ols")$ar[1]

# now write a function for boot()
# dat = residuals (disturbances)
tsb <- function(dat, indices){
  zstar <- numeric(length(dat[-1]))
  distb <- dat[indices]
  zstar[1] <- lh[1] - mean(lh)
  for(i in 2:length(dat[-1])){
    zstar[i] <- beta*zstar[i-1] + distb[i]  
  } 
  ar(zstar, aic=FALSE, order.max=1, method="ols")$ar[1]
}

# test function
tsb(dat=mod1$resid[-1], sample(length(mod1$resid[-1]), replace=TRUE))

# now use boot to boostrap the SE:
boot(mod1$resid[-1],tsb, R=200) 
# t1* is meaningless here

# another appoach to bootstrapping time series:
# The moving blocks bootstrap
# instead of fitting model and sampling residuals, 
# we sample from all possible contigous blocks from the time series,
# and paste them together to form a bootstrap time series.
# You have to specify block size.

# let's redo the previous analysis using moving blocks bootstrap.

# First write a function for boot()
# it simply fits a model and extracts beta estimate
lh.fun <- function(dat) {
  ar.fit <- ar(dat, aic=FALSE, order.max=1, method="ols")
  ar.fit$ar[1]
}

# we then use the tsboot() function to do the bootstrap:
# We specify a block length of 3 and do 200 boostraps.
# sim = "fixed" means block resampling with fixed block lengths of 3
bout <- tsboot(lh, lh.fun, R = 200, l = 3, sim = "fixed")
bout



