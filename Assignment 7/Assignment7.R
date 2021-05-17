### Exercise 7.1 ###

source("functions_Ch8.txt")

geese = read.csv("geese.txt", sep="\t")
attach(geese)

# a)

par(mfrow=c(1,2))

plot(observer1,photo,
     xlab='Observer 1 count',
     ylab='Photo count')
plot(observer2,photo,
     xlab='Observer 2 count',
     ylab='Photo count')
mtext('Number of observed vs. photographed geese',
      side=3, line=-3, outer=TRUE)

help(mtext)

# b)

observer1lm = lm(photo~observer1)
observer2lm = lm(photo~observer2)

summary(observer1lm)
summary(observer2lm)

help(lm)

# c)

par(mfrow=c(1,2))

plot(observer1lm$residuals, photo,
     xlab='Residuals of observer 1',
     ylab='Number of geese in photos')
abline(v=0)
plot(observer2lm$residuals, photo,
     xlab='Residuals of observer 2',
     ylab='Number of geese in photos')
abline(v=0)

# d)

qqnorm(observer1lm$residuals)
qqnorm(observer2lm$residuals)

ks.test(lm.norm.test(observer1,photo), pnorm)

help(lm.norm)

# e)

logphoto = log(photo)
logobs1 = log(observer1)
logobs2 = log(observer2)

par(mfrow=c(1,2))

plot(logobs1,logphoto,
     xlab='Log observer 1 count',
     ylab='Log photo count')
plot(logobs2,logphoto,
     xlab='Log observer 2 count',
     ylab='Log photo count')
mtext('Log of number of observed vs. log of photographed geese',
      side=3, line=-3, outer=TRUE)

loglm1 = lm(logphoto~logobs1)
loglm2 = lm(logphoto~logobs2)

summary(loglm1)
summary(loglm2)

par(mfrow=c(1,2))

plot(loglm1$residuals, logphoto,
     xlab='Residuals of log(observer1)',
     ylab='Log of number of geese in photos')
abline(v=0)
plot(loglm2$residuals, logphoto,
     xlab='Residuals of log(observer2)',
     ylab='Log of number of geese in photos')
abline(v=0)

qqnorm(loglm1$residuals, main='Normal QQ-plot - Observer 1 residuals')
qqnorm(loglm2$residuals, main='Normal QQ-plot - Observer 2 residuals')

# f)

# g)

### Exercise 7.2 ###

pollution = read.csv("airpollution.txt", sep=" ")
attach(pollution)

alpha = 0.05

# a)

pairs(cbind(oxidant, wind, temperature, humidity, insolation))

# b)

windlm = lm(oxidant~wind)
temperaturelm = lm(oxidant~temperature)
humiditylm = lm(oxidant~humidity)
insolationlm = lm(oxidant~insolation)

summary(windlm)
summary(temperaturelm)
summary(humiditylm)
summary(insolationlm)

summary(lm(oxidant~wind+temperature))
summary(lm(oxidant~wind+humidity))
summary(lm(oxidant~wind+insolation))

summary(lm(oxidant~wind+temperature+humidity))
summary(lm(oxidant~wind+temperature+insolation))

stepup_lm = lm(oxidant~wind+temperature)

# c)

lmfull = lm(oxidant~wind+temperature+humidity+insolation)

summary(lmfull)

# d)

summary(lm(oxidant~wind+temperature+humidity))

summary(lm(oxidant~wind+temperature+humidity-1))

summary(lm(oxidant~wind+temperature-1))

# e)

plot(wind, oxidant)
plot(temperature, oxidant)

# f)

# g)

# h)

# i)

# j)

### Exercise 7.3 ###

expenses = read.csv("expensescrime.txt", sep=" ")
attach(expenses)

# plots against 'expend'
par(mfrow=c(1,2))

plot(bad, expend)
plot(lawyers, expend)
plot(employ, expend)
plot(pop, expend)

par(mfrow=c(1,1))

plot(crime, expend)

# looking for collinearity
pairs(cbind(bad, crime, lawyers, employ, pop))

# step-up method
summary(lm(expend~bad))
summary(lm(expend~crime))
summary(lm(expend~lawyers))
summary(lm(expend~employ))  # highest R^2
summary(lm(expend~pop))

summary(lm(expend~employ+bad))
summary(lm(expend~employ+crime))
summary(lm(expend~employ+lawyers))  # highest R^2
summary(lm(expend~employ+pop))

summary(lm(expend~employ+lawyers+bad))  # highest R^2, 'bad' not significant
summary(lm(expend~employ+lawyers+crime))
summary(lm(expend~employ+lawyers+pop))

crime_stepup_lm = lm(expend~employ+lawyers)

# step-down method

summary(lm(expend~bad+crime+lawyers+employ+pop))

# p-val highest for 'crime'

summary(lm(expend~bad+lawyers+employ+pop))

# p-val highest for 'pop'

summary(lm(expend~bad+lawyers+employ))

# p-val highest for 'bad'

summary(lm(expend~lawyers+employ))

# p-val highest for '(Intercept)', but significant

crime_stepdown_lm = lm(expend~lawyers+employ) # same as step-up

crime_lm = lm(expend~employ+lawyers)

# a)

# b)

# c)

