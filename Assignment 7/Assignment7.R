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

# b)

observer1lm = lm(photo~observer1)
observer2lm = lm(photo~observer2)

summary(observer1lm)
summary(observer2lm)

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

par(mfrow=c(1,2))

qqnorm(observer1lm$residuals, main='Normal QQ-plot for observer 1')
qqnorm(observer2lm$residuals, main='Normal QQ-plot for observer 2')

ks.test(lm.norm.test(observer1,photo), pnorm)
ks.test(lm.norm.test(observer2,photo), pnorm)

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

oxi_stepup_lm = lm(oxidant~wind+temperature)

summary(oxi_stepup_lm)

# c)

lmfull = lm(oxidant~wind+temperature+humidity+insolation)

summary(lmfull)

# d)

summary(lm(oxidant~wind+temperature+humidity))

summary(lm(oxidant~wind+temperature))

oxi_stepdown_lm = lm(oxidant~wind+temperature)

summary(oxi_stepdown_lm)

# e)

plot(wind, oxidant)
plot(temperature, oxidant)

oxi_lm = lm(oxidant~wind+temperature)

# f)

plot(oxidant)
plot(oxidant-oxi_stepdown_lm$residuals, oxidant)
plot(oxi_stepup_lm$fitted.values)

plot(temperature, oxidant)

plot(lm(oxidant~day+wind+humidity+insolation)$residuals,
     lm(temperature~day+wind+humidity+insolation)$residuals,
     main='added varplot for Temperature',
     xlab='RXKXK',
     ylab='RYXK')

plot(lm(oxidant~temperature+wind+humidity+insolation)$residuals,
     lm(day~temperature+wind+humidity+insolation)$residuals,
     main='added varplot for Day',
     xlab='RXKXK',
     ylab='RYXK')

plot(lm(oxidant~day+temperature+humidity+insolation)$residuals,
     lm(wind~day+temperature+humidity+insolation)$residuals,
     main='added varplot for Wind',
     xlab='RXKXK',
     ylab='RYXK')

plot(lm(oxidant~day+wind+temperature+insolation)$residuals,
     lm(humidity~day+wind+temperature+insolation)$residuals,
     main='added varplot for Humidity',
     xlab='RXKXK',
     ylab='RYXK')

plot(lm(oxidant~day+wind+humidity+temperature)$residuals,
     lm(insolation~day+wind+humidity+temperature)$residuals,
     main='added varplot for Insolation',
     xlab='RXKXK',
     ylab='RYXK')

# g)

plot(oxidant, lm(oxidant~wind+temperature)$residuals)

u = c(rep(0,3),1,rep(0,length(oxidant)-4))
msolm = lm(oxidant~wind+temperature+u-1)
summary(msolm)

# h)

# leverage points
p = 2
n = 30

2*(p+1)/n

hii = round(hatvalues(oxi_lm), 3)

hii[hii>= 0.2]

# influence points
round(cooks.distance(oxi_lm), 2)

plot(cooks.distance(oxi_lm),
     main='Cook\'s distance',
     xlab='i-th observation',
     ylab='Cook\'s distance')

# collinearity
round(cor(pollution[,2:5]), 2)

round(varianceinflation(pollution[,2:5]), 2)

round(conditionindices(pollution[,2:5]), 2)

round(vardecomposition(pollution[,2:5]), 3)

# i)

par(mfrow=c(1,2))

plot(oxi_lm$residuals, oxidant,
     main='Plot of residuals against the response variable',
     xlab='Residuals of the linear model',
     ylab='Oxidant')
abline(v=0)

qqnorm(oxi_lm$residuals,
       main='Normal QQ-plot of the residuals')

# j)

summary(oxi_lm)
round(oxi_lm$coefficients, 2)

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

expend_stepup_lm = lm(expend~employ+lawyers)

# step-down method

summary(lm(expend~bad+crime+lawyers+employ+pop))

# p-val highest for 'crime'

summary(lm(expend~bad+lawyers+employ+pop))

# p-val highest for 'pop'

summary(lm(expend~bad+lawyers+employ))

# p-val highest for 'bad'

summary(lm(expend~lawyers+employ))

# p-val highest for '(Intercept)', but significant

expend_stepdown_lm = lm(expend~lawyers+employ) # same as step-up

expend_lm = lm(expend~employ+lawyers)

logexpend_lm = lm(log(expend)~log(employ)+log(lawyers))

# residuals versus 'expend'

plot(expend_lm$residuals, expend,
     xlab='Residuals of the linear model',
     ylab='Expend')
abline(v=0)

qqnorm(expend_lm$residuals)

# leverage points
p = 2
n = 51

2*(p+1)/n

hii = round(hatvalues(expend_lm), 3)

hii[hii>= 2*(p+1)/n]

# influence points
expend_cook = round(cooks.distance(expend_lm), 2)

expend_cook[expend_cook >= 1]

plot(cooks.distance(oxi_lm),
     main='Cook\'s distance',
     xlab='i-th observation',
     ylab='Cook\'s distance')

# collinearity
round(cor(expenses[,3:7]), 2)

round(varianceinflation(expenses[,3:7]), 2)

round(conditionindices(expenses[,3:7]), 2)

round(vardecomposition(expenses[,3:7]), 3)

