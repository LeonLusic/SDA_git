### Exercise 6.1 ###

data = read.csv("expensescrime.txt", sep=" ")
nausea = read.csv("nausea.txt", sep=" ")

library(mvtnorm)

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

pairs(cbind(lawyers_rate, data$crime), labels=c('Lawyers rate', 'Crime'))

lawyers_rate[8] = mean(lawyers_rate)

pairs(cbind(lawyers_rate, data$crime), labels=c('Lawyers rate', 'Crime'))

# c)

cor.test(lawyers_rate, data$crime, method='k')
cor.test(lawyers_rate, data$crime, method='s')

# d)

B = 10000
t = cor.test(lawyers_rate, data$crime, method='k')$p.value
permutationval = numeric(B)
for(i in 1:B) {
  sample_crime = sample(data$crime)
  pl = cor.test(lawyers_rate,
                sample_crime,
                method='k',
                alternative='g')$p.value
  pr = cor.test(lawyers_rate,
                sample_crime,
                method='k',
                alternative='l')$p.value
  p = 2*min(pl, pr)
  permutationval[i] = p
}

length(permutationval[permutationval<=t])/B

# f)

# Asymptotic Relative Efficiency function
aresimulation = function(B, n) {
  pvalkendalltest=numeric(B)
  pvalspearmantest=numeric(B)
  for(i in 1:B)
  {
    v = rmvnorm(n, mean=c(0,0), sigma=matrix(c(1,0.5,0.5,1), 2,2))
    pvalkendalltest[i] = cor.test(v[,1], v[,2], method='k')$p.value
    pvalspearmantest[i] = cor.test(v[,1], v[,2], method='s')$p.value
  }
  powerkendall=sum(pvalkendalltest<0.05)/B
  powerspearman=sum(pvalspearmantest<0.05)/B
  sim = rbind(c("Kendall", "Spearman"), c(powerkendall, powerspearman))
  sim[2,] = as.numeric(sim[2,])
  sim
}

B = 10000

# test 1: n = 45
n = 45
sim1 = aresimulation(B, n)

# test 2: n = 50
n = 50
sim2 = aresimulation(B, n)

# test 3: n = 55
n = 55
sim3 = aresimulation(B, n)


col1 = c(as.numeric(sim1[2,1])/as.numeric(sim1[2,2]),
         as.numeric(sim2[2,1])/as.numeric(sim1[2,2]),
         as.numeric(sim3[2,1])/as.numeric(sim1[2,2]))

col2 = c(as.numeric(sim1[2,1])/as.numeric(sim2[2,2]),
         as.numeric(sim2[2,1])/as.numeric(sim2[2,2]),
         as.numeric(sim3[2,1])/as.numeric(sim2[2,2]))

col3 = c(as.numeric(sim1[2,1])/as.numeric(sim3[2,2]),
         as.numeric(sim2[2,1])/as.numeric(sim3[2,2]),
         as.numeric(sim3[2,1])/as.numeric(sim3[2,2]))

round(matrix(c(col1, col2, col3), nrow=3, ncol=3),3)

### Exercise 6.2 ###

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


### Exercise 6.3 ###

data = data.frame(nausea$Incidence.of.Nausea,
                  nausea$Number.of.Patients-nausea$Incidence.of.Nausea,
                  row.names = rownames(nausea))
colnames(data) = c('Nausea', 'No nausea')

alpha = 0.05

# b)

chisq.test(data)

chisq.test(data)$expected

# c)

chisq.test(data, simulate.p.value=TRUE)

# d)

round(chisq.test(data)$residuals, 2)

round(chisq.test(data)$stdres, 2)

# e)

B=10000

t = maxcontributionscat(data)
boot = bootstrapcat(data, B, maxcontributionscat)

mean(boot>=t)

# g)

fisher.test(data[1:2,], alternative='greater')
