


iter = 50000L

n = 3

mu0 = 5; sd0 = 10

sigma2 = numeric(iter) 
sigma1 = numeric(iter) 
sigma1.un = numeric(iter) 

S2 = numeric(iter)
S1 = numeric(iter)
#========================================
sigma2.mle = function(x)  { sum( (x-mean(x))^2 ) / length(x) }
sigma1.mle = function(x)  { sqrt(sum( (x-mean(x))^2 ) / length(x)) }
sigma1.unbiased = function(x)  { sd(x) / (sqrt(2/(n - 1)) * exp(lgamma(n/2) - lgamma((n - 1)/2))) }
#========================================

for ( i in seq_len(iter) ) { 
    x = rnorm(n, mean=mu0, sd=sd0)
    S2[i] = var(x)
    S1[i] = sd(x)

    sigma2[i] = sigma2.mle(x)
    sigma1[i] = sigma1.mle(x)
    sigma1.un[i] = sigma1.unbiased (x)
}


# In theory, S2 (sample variance) is unbiased.
# S1 (sample standard deviation) is biased.
# sigma2 (MLE) of sigma^2 is biased
# sigma1 (MLE) of sigma is baised

mean(S1) - sd0
mean(S2) - sd0^2   #### very small 

mean(sigma1) - sd0
mean(sigma2) - sd0^2

mean(sigma1.un) - sd0   ### very small



