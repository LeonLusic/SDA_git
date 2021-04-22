### Exercise 6.1 ###

data = read.csv("expensescrime.txt", sep=" ")
nausea = read.csv("nausea.txt", sep=" ")

source("functions_Ch7.txt")

# a)

pairs(cbind(data$expend, data$bad))
pairs(cbind(data$expend, data$crime))
pairs(cbind(data$expend, data$lawyers))
pairs(cbind(data$expend, data$employ))
pairs(cbind(data$expend, data$pop))

pairs(cbind(data$bad, data$crime))
pairs(cbind(data$bad, data$lawyers))
pairs(cbind(data$bad, data$employ))
pairs(cbind(data$bad, data$pop))

pairs(cbind(data$crime, data$lawyers))
pairs(cbind(data$crime, data$employ))
pairs(cbind(data$crime, data$pop))

pairs(cbind(data$lawyers, data$employ))
pairs(cbind(data$lawyers, data$pop))

pairs(cbind(data$employ, data$pop))

# b)

lawyers_rate = data$lawyers/data$pop

data$pop[8]
data$lawyers[8]

max(lawyers_rate)

lawyers_rate[8]
data$state[8]

lawyers_rate[8] = mean(lawyers_rate)

pairs(cbind(lawyers_rate, data$crime), labels=c('Lawyers rate', 'Crime'))

plot(lawyers_rate, data$crime)
plot(data$crime, lawyers_rate)

help(plot)

help(pairs)

# c)

cor.test(lawyers_rate, data$crime, method='k')
cor.test(lawyers_rate, data$crime, method='s')

help(cor.test)

# d)

B = 10000
t = cor.test(lawyers_rate, data$crime, method='k')
permutationval = numeric(B)
for(i in 1:B) {
  sample_lawyers = sample(lawyers_rate)
  sample_crime = sample(data$crime)
  boot_sample = sample(cbind(lawyers_rate, data$crime))
  pl = cor.test(boot_sample[1:51],
                boot_sample[52:102],
                method='k',
                alternative='g')$p.value
  pr = cor.test(boot_sample[1:51],
                boot_sample[52:102],
                method='k',
                alternative='l')$p.value
  p = 2*min(pl, pr)
  print(p)
  permutationval[i] = p
}

permutationval
hist(permutationval)

# e)

# f)

# Asymptotic Relative Efficiency function
aresimulation = function(B, n) {
  pvalkendalltest=numeric(B)
  for(i in 1:B)
  {
    v = rmvnorm(n, mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1), 2,2))
    pvalkendalltest[i] = cor.test(v[,1], v[,2], method='k')$p.value
  }
  powerkendall=sum(pvalkendalltest<0.05)/B
  rbind(c("Kendall"),c(powerkendall))
}

B = 10000

# test 1: n = 45
n = 45
aresimulation(B, n)

# test 2: n = 50
n = 50
aresimulation(B, n)

# test 3: n = 55
n = 55
aresimulation(B, n)


### Exercise 6.1 ###

alpha = 0.05

infected = matrix(c(24, 15, 1020, 1167),
                  nrow=2, ncol=2, 
                  dimnames=list(c('men', 'women'),
                                c('deaths', 'recoveries')))

# a)

# null hypothesis: the row and column variables are independent
# alternative hypothesis: the row and column variables are dependent

fisher.test(infected)

# b)

# null hypothesis: men are less often among the fatalities than women
# alternative hypothesis: men are more often among the fatalities than women

fisher.test(infected, alt='g')

# c)

pl = phyper(24, 1044, 1182, 39)
pr = 1 - phyper(24-1, 1044, 1182, 39)
2*min(c(pl, pr))


### Exercise 6.1 ###

# a)

# b)

# c)

# d)

# e)

# f)

# g)
