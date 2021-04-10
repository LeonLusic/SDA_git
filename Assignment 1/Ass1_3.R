exp <- function(n, rate) {
x = rexp(n, rate)
title = paste("Histogram vs. Density\n", "n =", n, ", rate =", rate)
hist(x, freq = FALSE, col="white", border="black",main=title)
points = seq(0, max(x), length=length(x))
lines(points, dexp(points,rate), col="red")
}

par(mfrow=c(2,3))

exp(10, 3)
exp(100, 3)
exp(1000, 3)
exp(1000, 3)
exp(1000, 10)
exp(1000, 100)
