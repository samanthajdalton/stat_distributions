source("./graphs.R")

bernoulli(0.7)

binomial.static()
n <- 10 #number of trials
p <- 0.5 # probability of success
binomial.dynamic(n, p)

poisson.static()
k <- 20 #number of trials
lambda <- 10 # probability of success
poisson.dynamic(k, lambda)

uniform.static()
a = 2
b = 5
uniform.dynamic(a, b)

beta.static()
alpha <- 2
beta <-5
beta.dynamic(alpha, beta)

rate <- 0.5
exponential.dynamic(rate)
exponential.static()

mean <- -2
sd <- 0.5
normal.dynamic(mean, sd)
normal.static()

