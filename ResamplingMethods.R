# Resampling Methods
# Fall 2014
# StatLab@UVa Library
# Clay Ford

# install.packages("bootstrap")
# install.packages("car")

library(boot) # included with base R installation
library(bootstrap) # has data from book, An Introduction to the Bootstrap (Efron & Tibshirani, 1993)

# Functions in R ----------------------------------------------------------

# Quick demo on writing a function in R.
# R functions are like mini programs.
# Use the function() function; arguments are variables.

# EXAMPLE 1
# write function to calculate Body Mass Index (BMI)
# Formula: weight (lb) / [height (in)]^2 x 703
BMI <- function(weight, height) weight/height^2 * 703
BMI(weight=213,height=69)
BMI(c(200,198,145),c(65,70,64)) # 3 people

# EXAMPLE 2
# randomly sample n items with replacement from data vector and calculate mean

# an aside on R indices
x <- c(12,24,34,22,87)
x[2] # select 2nd number
x[c(2,1,2,4,3)] # select 2nd, 1st, 2nd, 4th and 3rd numbers, in that order


smean <- function(data, n){
  i <- sample(length(data),n, replace=TRUE) # indices to sample
  mean(data[i])
}

# generate fake data
fake <- rnorm(30,mean = 200,sd = 10)
# sample 10 items with replacement from fake and calculate mean 
smean(data=fake, n=10)
# sample 3 items with replacement from fake and calculate mean 
smean(data=fake, n=3)

# Bootstrapping SE --------------------------------------------------------

# use data that comes from bootstrap package
# A small randomized experiment done with 7 mice (treatment group).
# Measurement unit is days of survival following surgery.
mouse.t
mean(mouse.t)

# take a bootstrap sample (must be with replacement)
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
# what did you get?

sd(mouse.t)/sqrt(7) # standard error using formula

# How to do with boot() function (from the boot package)
# first define a function;
# To work with boot(), it needs two arguments:
# 1. data
# 2. indices which define the bootstrap sample
mean.fun <- function(data,ind) mean(data[ind])

# example of how function works
indices <- sample(length(mouse.t),replace=TRUE) # generate indices
indices
mouse.t
mouse.t[indices]
mean(mouse.t[indices])

# plug data and indices into function; returns mean of values selected by
# indices:
mean.fun(data=mouse.t,ind=indices) 
# what did you get?

# now use in the boot function with R=200 replications:
# Basic boot arguments: data, statistic, R
boot(data=mouse.t, statistic=mean.fun, R=200)
# do it again; different result

# save results
bout <- boot(data=mouse.t, statistic=mean.fun, R=200)

# bootstrap replicates
bout$t 

# standard error calculated when "printing" object
bout

# to extract the SE
sd(bout$t)

# examine graphical plot of the bootstrap replicates
plot(bout)

# see the frequency or index array for the bootstrap resamples:
boot.array(bout)
boot.array(bout, indices=TRUE)


# EXAMPLE 2: Standard Error of correlation 

# Law school data. A random sample of size n=15 from the universe of 82 USA law
# schools (as of 1993). Two measurements: LSAT (average score on a national law
# test) and GPA (average undergraduate grade-point average).

law
plot(law)
cor(law[,1],law[,2]) # correlation of column 1 with column 2

# function to calculate correlation of indicated rows
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

# EXAMPLE 3: Standard Error of ratio

# Eight subjects wore medical patches designed to infuse a certain
# naturally-occuring hormone into the blood stream. Each subject had his blood
# levels of the hormone measured after wearing three different patches: a
# placebo patch, an "old" patch manufactured at an older plant, and a "new"
# patch manufactured at a newly opened plant.

patch
# z = oldpatch - placebo
# y = newpatch - oldpatch

# The purpose of the study was to show bioequivalence, defined as:
# ratio mean(y)/mean(z) <= 0.20
# want the new facility to match the old facility within 20%

# ratio estimate
mean(patch$y)/mean(patch$z)

# what's the SE of this estimate?

# write a function for boot()
ratio.fun <- function(dat, ind){
  tmp <- dat[ind,] 
  mean(tmp$y)/mean(tmp$z)
}

# do the bootstrap
bout3 <- boot(patch, ratio.fun, R=999)
bout3
sd(bout3$t)
plot(bout3)

# Bootstrap estimate of Bias ----------------------------------------------

# recall mouse bootstrap
bout

# calculate bias
mean(bout$t) - bout$t0 

# As a rule of thumb, a bias less than 0.25 standard errors can be ignored.
abs(mean(bout$t) - bout$t0)/sd(bout$t) < 0.25

# recall law data
bout2
mean(bout2$t) - bout2$t0 # bias
abs(mean(bout2$t) - bout2$t0)/sd(bout2$t) < 0.25

# a function for the above calculations
# obj = a boot object
bias <- function(obj){
  list(bias=mean(obj$t) - obj$t0,
       ignore=abs(mean(obj$t) - obj$t0)/sd(obj$t) < 0.25)
}

bias(bout)
bias(bout2)

# patch data
bias(bout3)


# The Jackknife -----------------------------------------------------------

# use the jackknife() function in the bootstrap package

# simple example
mouse.t

# find jackknife values for the sample median
# use jackknife() function from the bootstrap package
jackknife(mouse.t, mean)
# compare to bootstrap SE and bias
bout

# example with complex data structure (ratio)
# recall the patch data
patch

# first write a function; have to use "x" for indices
ratio.fun.j <- function(x, dat){
  tmp <- dat[x,] 
  mean(tmp$y)/mean(tmp$z)
}

# find jackknife values for the sample ratio
# have to pass as data to jackknife the vector 1,2,..n.
jackknife(1:nrow(patch), ratio.fun.j, patch)
# Compare to bootstrap values
bout3


# The jackknife-after-bootstrap -------------------------------------------


# A bootstrap diagnostic for estimating the variability 
# from a set of bootstrap estimates.

# Calculate the jackknife influence values from a bootstrap output object and
# plot the corresponding jackknife-after-bootstrap plot.
# Recall: bout3 = bootstrap object for the patch data
jack.after.boot(bout3)
# can do this as well
plot(bout3, jack=TRUE) 

# The x-axis are the jackknife influence values
# The y-axis are the centered jackknife quantiles
# horizontal dashed lines are the quantiles of the centered bootstrap distribution

# vertical asterisks are the quantiles of the centered bootstrap distribution
# when the indicated observation is removed

# see bonus materials below for details on calculations

# Bootstrap confidence intervals ------------------------------------------

# mouse data
bout
plot(bout)
# histogram and qq plot look symmetric and normal,
# so normal and percentile intervals should be similar.
boot.ci(bout, type="norm")
boot.ci(bout, type="perc")
boot.ci(bout, type="bca")
# Note the warning: "Some BCa intervals may be unstable";
# means interval is using the upper or lower 10 order statistics.
# to get rid of warning, you can try increasing R or narrow the interval
boot.ci(bout2, type="bca", conf=0.90)

boot.ci(bout, type=c("perc","bca","norm"))

# law school data
bout2
plot(bout2) # note the skewed histogram
# histogram and qq plot are not symmetric and normal,
# so normal and percentile intervals should not be similar.
boot.ci(bout2, type=c("perc","bca","norm"))
# anything look strange about your "normal" interval?

# patch data
bout3
plot(bout3)
boot.ci(bout3, type=c("perc","bca","norm"))

# Bootstraping Regression Models ------------------------------------------

library(car) # for function Boot()
library(MASS) # for robust regression function rlm()

# cell data: Data on cell survival under different radiation doses;
# comes with bootstrap package
# dose = raditaion level
# log.surv = log proportion of surving cells
cell
plot(cell) # note the outlier

# fit a standard linear model with quadratic term and no intercept;
# no intercept because survival is log(1)=0 with no radiation dose
m1 <- lm(log.surv ~ -1 + poly(dose,2,raw = T), data=cell)
# robust regression with rlm()
# maxit: limit on the number of IWLS iterations.
m2 <- rlm(log.surv ~ -1 + poly(dose,2,raw = T), data=cell, maxit = 100)
# add fitted lines
lines(cell[,1], fitted(m1))
lines(cell[,1], fitted(m2), lty=2) 
legend("topright",legend = c("lm","rlm"),lty = c(1,2))

summary(m1) # quadratic term significant
summary(m2) # quadratic term not significant

# Let's bootstrap the standard errors of the rlm model.

# doses were fixed values chosen by investigator, therefore makes sense to
# bootstrap residuals

# the car package has the Boot() function that makes
# basic bootstrapping of regression models very easy.
# Note the capital "B": Boot() vs. boot()
# use method="case" for case resampling
Boot1 <- Boot(m2, R=999, method="residual")
summary(Boot1)

# compare BootSE to original SE
summary(m2)$coefficients


# 90% bootstrap confidence intervals using confint() 
confint(Boot1, type="bca", level=0.90)
confint(Boot1, type="perc", level=0.90)

# bootstrap diagnostics
par(mfrow=c(2,1))
jack.after.boot(Boot1, index=1, main="dose")
jack.after.boot(Boot1, index=2, main="dose^2")
par(mfrow=c(1,1))


# Cross Validation --------------------------------------------------------

# Let's use CV to estimate error rate for logistic regression 

# low birth weight data (from Applied Logistic Regression, 2nd ed.)
# birthwt data from MASS package
head(birthwt)

# Selected variables:
# low = birth weight < 2500 g (1 = yes, 0 = no)
# age = age of mother in years
# lwt = weight of mother in pounds
# race = race of mother (1 = white, 2 = black, 3 = other)
# smoke = smoking status during pregnancy
# ht = history of hypertension

# model probability of low birth weight using factors above.
bw.glm <- glm(low ~ age + lwt + factor(race) + smoke + ht, 
              data=birthwt, family=binomial)
summary(bw.glm)

# calculate error rate
# fitted(bw.glm) returns predicted probabilities
# if P > 0.5, then predict 1, otherwise 0
pred <- ifelse(fitted(bw.glm) > 0.5, 1, 0)
tab <- table(birthwt$low, pred)
tab
# error rate
1 - sum(diag(tab))/sum(tab)

# error rate estimated to be about 27%;
# estimated with same data used to build model,
# so probably too optimistic.

# now estimate error rate using 5-fold cross validation

# "manual" 5-fold cross validation
# first randomly assign rows to one of 5 groups
birthwt$g <- sample(1:5, 189, replace=TRUE)

error.rate <- numeric(5)
for(i in 1:5){
  tmp.glm <- glm(low ~ age + lwt + factor(race) + smoke + ht, 
                 data=birthwt, subset= g!=i, family=binomial)
  tmp.pred <- predict(tmp.glm, newdata=subset(birthwt, g==i))
  pred <- ifelse(tmp.pred > 0.5, 1, 0)
  tab <- table(birthwt$low[birthwt$g==i], pred)
  error.rate[i] <- 1 - sum(diag(tab))/sum(tab)
}
sum(error.rate)/5 # 5-fold CV test error

# cross validation using cv.glm
# need to create a "cost function" to calculate error rate
# r = observed response (0 or 1)
# p = predicted probability
# if abs(r - p) > 0.5, count as a missclassification
cost <- function(r, p = 0) mean(abs(r-p) > 0.5)

# 5-fold CV
cv.err <- cv.glm(birthwt, bw.glm, cost, K = 5) 
cv.err$delta
# first number is CV estimate; 2nd number is bias-corrected version;

# Leave-one-out CV
cv.err2 <- cv.glm(birthwt, bw.glm, cost) 
cv.err2$delta


#####################################################################
# BONUS MATERIAL!


# Permutation Tests

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

# function to compute difference between group means
cdiff <- function(tot,n){
  s <- sample(tot, n, replace = FALSE)
  mean(all.mouse[s]) - mean(all.mouse[-s])
} 

# compute difference in means for 1000 permutations
diff <- replicate(n = 1000, cdiff(tot=(n+m),n=n))

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


######

# How to calculate centered jackknife quantiles and jackknife influence values
# we'll use the patch data from the bootstrap package
alpha <- c(0.05, 0.1, 0.16, 0.5, 0.84, 0.9, 0.95)

# (1) get the bootstrap replicates
br <- bout3$t
# (2) get number of rows in data set
n <- nrow(patch)
# (3) get the frequency array for the bootstrap resamples
f <- boot.array(bout3)
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

#####

# In case you're interested, how "normal" bootstrap calculated:
# Normal bootstrap method
boot.ci(bout2, type="norm")
bias <- mean(bout2$t)-bout2$t0 # calculate bias
(bout2$t0 - bias) + c(sd(bout2$t)*-1.96, sd(bout2$t)*1.96)

#####

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

# How accurate is our estimate of 0.586?
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
