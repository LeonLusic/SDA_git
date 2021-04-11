### Exercise 5.1 ###

grades = scan("statgrades.txt")
source("functions_Ch3.txt")
source("functions_Ch5.txt")

hist(grades)
summary(grades)

qqnorm(grades)

# a)
# H_0: m <= 6, H_1: m > 6, alpha=1%
n = length(grades)
m = 6

l = sum(grades>m)
k = sum(grades==m)

binom.test(l,n-k,alt="g",conf.level=0.99)

help(binom.test)

# b)
# H_0: m = 6.5, H_1: m != 6.5, alpha=5%
m = 6.5

l = sum(grades>m)
k = sum(grades==m)

binom.test(l,n-k,alt="t",conf.level=0.95)

# p = P(grade>=7), H_0: p >= 35%, H_1: p < 35%, alpha=10%
m = 7

l = sum(grades>m)
k = sum(grades==m)

binom.test(l,n-k,alt="l",conf.level=0.90)

hist(grades)

### Exercise 5.2 ###

clouds = read.csv("clouds.txt", sep=" ")
clouds$seeded.clouds

seeded = clouds$seeded.clouds

# a)
# graphical and numerical investigation

summary(seeded)
mad(seeded)
sd(seeded)

# symplot and boxplot
par(mfrow=c(1,2))
symplot(seeded,
        main="Symplot of precipitation values for 'seeded' group")
boxplot(seeded,
        main="Boxplot of precipitation values for 'seeded' group")

# histograms
hist(seeded,
     main="Histogram of precipitation values for 'seeded' group",
     xlab="Precipitation values - 'seeded'")
hist(seeded,
     breaks=10,
     main="Histogram of precipitation values for 'seeded' group",
     xlab="Precipitation values - 'seeded'")

hist(seeded[seeded<500],
     main="Histogram of precipitation values for 'seeded' group (value < 500)",
     xlab="Precipitation values - 'seeded'")
hist(seeded[seeded<500],
     breaks=7,
     main="Histogram of precipitation values for 'seeded' group (value < 500)",
     xlab="Precipitation values - 'seeded'")

# QQ plots
qqnorm(seeded,
       main="QQ plot against normal dist.")
qqexp(seeded,
      main="QQ plot against exponential dist.")

qqchisq(seeded,
        df=1,
        main="QQ plot against Chi squared dist. df =")
qqlnorm(seeded,
        main="QQ plot against lognormal dist.")

# b)
# sample standard deviation
sd_seeded = sd(seeded)

# c)
# bootstrap sample standard deviation
sd_boot = sd(bootstrap(seeded,sd,B=1000))

# d)
# MAD measure of accuracy
mad_seeded = mad(seeded)
mad_seeded

# bootstrap MAD
mad_boot = mad(bootstrap(seeded,mad,B=1000))

sd(seeded)/sd_boot

mad(seeded)/mad_boot

# f)
# location tests
# sign test preferable

# g)
# location test on location less than 119.0, alpha=5%
m = 119.0

l = sum(seeded>m)
k = sum(seeded==m)

binom.test(l,n,alt="g",conf.level=0.95)

# h)
# 99% confidence interval for the location based on all three tests

rbind(0:n,round(pbinom(0:n,size=n,p=0.5),4))
sort(seeded)[7]
# m_0 > 92.4 = seeded[7]
pbinom(7,n-1,0.5)

hist(seeded)

rbind(0:n,round(1-pbinom((0:n)-1,size=n,p=0.5),4))
sort(seeded)[21]
# m_0 < 489.1 = seeded[21]
1 - pbinom(20,n-1,0.5)

t.test(seeded,conf.level=0.99)
wilcox.test(seeded,conf.int=T,conf.level=0.99)

### Exercise 5.3 ###

light = scan("newcomb.txt")

hist(light[c(1:20)])

hist(light[c(-46:-1)])

plot(density(light[c(1:20)]))

plot(density(light[c(-46:-1)]))

hist(light)
plot(density(light))
boxplot(light)
light[light>0]
hist(light[light>0])
boxplot(light[light>0])
plot(density(light[light>0]))




