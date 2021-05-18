### Exercise 3.1 ###

# import the data
sample1 = scan('sample31.txt')

# import the functions h_opt and CV
source('functions_Ch4.txt')

# plot the Epanechnikov KDE for h=1,2,3
plot(density(sample1, kernel='epanechnikov', bw=1))
plot(density(sample1, kernel='epanechnikov', bw=2))
plot(density(sample1, kernel='epanechnikov', bw=3))

test = density(sample1, kernel='epanechnikov', bw=3)
test['x']
test[test$y]
?density

hist(y1)

sigma = sd(y1)

n = length(y1)

ddf = 3/(5*n*h1^5)

plot(density(sample1, kernel='epanechnikov', bw=h1))

### Exercise 3.2 ###

sample2 = scan('sample32.txt')

plot(density(sample2, bw = h_opt(sample2), kernel='gaussian'))

yrange = seq(min(sample2), max(sample2), length.out=512)

lines(exp(yrange), density(sample2, bw=h_opt(sample2), kernel='epanechnikov',
      from=min(yrange), to=max(yrange))$sample2/exp(yrange))

?density

### Exercise 3.3 ###

sample3 = scan('sample33.txt')

h3 = h_opt(sample3)

hist(sample3)

plot(density(sample3, kernel='epanechnikov', bw=h3))

lines(pgamma(sample3, shape=3, scale=0.4))

hcv = CV(h3, sample3, kernel='gaussian')

h_vals = seq(0, 3, 0.1)
cv_vals = seq(0, 3, 0.1)
for (index in length(h_vals)) {
  cv_vals[index] = CV(h_vals[index], sample3, kernel='epanechnikov')
}
plot(h_vals, cv_vals)
