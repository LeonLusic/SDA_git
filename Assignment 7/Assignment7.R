### Exercise 7.1 ###

source("functions_Ch8.txt")

geese = read.csv("geese.txt", sep="\t")
attach(geese)

# a)

plot(observer1,photo)
plot(observer2,photo)

# b)

observer1lm = lm(observer1~photo, data=geese)
observer2lm = lm(observer2~photo, data=geese)

summary(lm(observer1~photo, data=geese))
summary(lm(observer2~photo-1, data=geese))

summary(observer1lm)

help(lm)

# c)

plot(observer1lm$residuals, photo)
plot(observer2lm$residuals, photo)

# d)

lm.norm.test(observer1,photo)

help(lm.norm)

qqnorm(observer1lm$residuals)
qqnorm(observer2lm$residuals)

# e)

# f)

# g)

### Exercise 7.2 ###

pollution = read.csv("airpollution.txt", sep=" ")
attach(pollution)

alpha = 0.05

# a)

pairs(cbind(wind, temperature, humidity, insolation))

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

summary(lm(oxidant~wind+temperature+humidity))

summary(lm(oxidant~wind+temperature+humidity-1))

summary(lm(oxidant~wind+temperature-1))

# d)

# e)

# f)

# g)

# h)

# i)

# j)

### Exercise 7.3 ###

expenses = read.csv("expensescrime.txt", sep=" ")

# a)

# b)

# c)

