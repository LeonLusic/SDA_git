### Exercise 4.1 ###

library(moments)

birthw = scan('birthweight.txt')

source('functions_Ch3.txt')
source('functions_Ch5.txt')

symplot(birthw)
qqlogis(birthw)
qqt(birthw,df=10)
qqlaplace(birthw)
qqcauchy(birthw)
qqchisq(birthw,df=9)
qqexp(birthw)
qqlnorm(birthw)
qqt(birthw,df=20)
qqline(birthw)

# relevant QQ-plots
par(mfrow=c(1,3))

qqlogis(birthw)
qqcauchy(birthw)
qqexp(birthw)

par(mfrow=c(1,1))

sd(bootstrap(birthw, median, B=1000))

# Parametric bootstrap estimate (part (b))
B = 1000
Tstar = numeric(B)

for(i in 1:B) {
  xstar = rexp(B, rate=1/mean(birthw))
  Tstar[i] = median(xstar)
}

sd(Tstar)

# One-line execution of part (c)
sd(replicate(1000, median(rexp(1000, rate=1/mean(birthw)))))

### Exercise 4.2 ###

source('thromboglobulin.txt')

attach(thromboglobulin)

## Mean estimates
bs_PRRP_mean = bootstrap(PRRP,mean,B=1000)
bs_PRRP_mean_star = bs_PRRP_mean - mean(bs_PRRP_mean)

interval_mean = 2*mean(bs_PRRP_mean)-quantile(bs_PRRP_mean,c(0.975,0.025))

#
hist(bs_PRRP_mean,
     main="Histogram of 1000 bootstrap samples of means of PRRP",
     xlab="Bootstrap samples of means of PRRP")
abline(v=interval_mean[1],col="red")
abline(v=interval_mean[2],col="red")

## Median estimates
bs_PRRP_med = bootstrap(PRRP,median,B=1000)
bs_PRRP_med_star = bs_PRRP_med - median(bs_PRRP_med)

interval_median = 2*median(bs_PRRP_med)-quantile(bs_PRRP_med,c(0.975,0.025))

# Histogram of means
hist(bs_PRRP_med,
     main="Histogram of 1000 bootstrap samples of medians of PRRP",
     xlab="Bootstrap samples of medians of PRRP")
abline(v=interval_median[1],col="red")
abline(v=interval_median[2],col="red")

## Mean difference estimate
bs_SDRP_mean = bootstrap(SDRP,mean,B=1000)
bs_PRRP_mean = bootstrap(PRRP,mean,B=1000)

zstar = numeric(B)

for(i in 1:B) {
  bs_SDRP_mean = bootstrap(SDRP,mean,B=1000)
  bs_PRRP_mean = bootstrap(PRRP,mean,B=1000)
  zstar[i] = (mean(bs_SDRP_mean) - mean(bs_PRRP_mean)) - (mean(SDRP) - mean(PRRP))
}

ninefive = c(quantile(zstar,0.025), quantile(zstar,0.975))
ten = c(quantile(zstar,0.45), quantile(zstar,0.55))

# Histogram of mean difference and 95%-confidence interval
hist(zstar,
     main="Histogram of 1000 bootstrap samples of differences of means between SDRP and PRRP",
     xlab="Bootstrap samples of differences of means")
abline(v=ninefive[1],col="red")
abline(v=ninefive[2],col="red")

# Histogram of mean difference and 10%-confidence interval
hist(zstar,
     main="Histogram of 1000 bootstrap samples of differences of means between SDRP and PRRP",
     xlab="Bootstrap samples of differences of means")
abline(v=ten[1],col="red")
abline(v=ten[2],col="red")

### Exercise 4.3 ###

source('light.txt')

ks1 = ks.test(light[[1]],pnorm,mean(light[[1]]),sd(light[[1]]))
ks2 = ks.test(light[[2]],pnorm,mean(light[[2]]),sd(light[[2]]))

# Observed values of the KS-test statistic D_n
d1_observed = ks1$statistic
d2_observed = ks2$statistic

# Bootstrap estimates of D_n
B = 1000
dstar1 = numeric(B)
dstar2 = numeric(B)

for(i in 1:B) {
  xstar1 = rnorm(B,mean(light[[1]]),sd(light[[1]]))
  xstar2 = rnorm(B,mean(light[[2]]),sd(light[[2]]))
  dstar1[i] = ks.test(light[[1]],xstar1)$statistic
  dstar2[i] = ks.test(light[[2]],xstar2)$statistic
}

# p-values for 1879 and 1882 respectively
p1 = min(sum(dstar1>=d1_observed)/B)
p2 = min(sum(dstar2>=d2_observed)/B)

# Histograms of bootstrap estimates of D_n with the observed values of D_n
hist(dstar1,
     main="Histogram of bootstrap estimates of D_n for 1879 with the observed values of D_n",
     xlab="Bootstrap estimates of D_n")
abline(v=d1_observed,col="red")
hist(dstar2,
     main="Histogram of bootstrap estimates of D_n for 1882 with the observed values of D_n",
     xlab="Bootstrap estimates of D_n")
abline(v=d2_observed,col="red")

# KS-test p-values
p1_ks = ks1$p.value
p2_ks = ks2$p.value
         