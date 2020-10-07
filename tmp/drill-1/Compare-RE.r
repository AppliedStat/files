
## source("/home/cp/MyFiles/Advising/p2-Chen/R/RChen.R")
source("RChen.R")

iter =  10000L  # The larger, the better

n = 50L # Sample size

   HL1.normal = numeric(iter)
  mean.normal = numeric(iter)
median.normal = numeric(iter)

   HL1.laplace= numeric(iter)
  mean.laplace= numeric(iter)
median.laplace= numeric(iter)

   HL1.logistic=numeric(iter)
  mean.logistic=numeric(iter)
median.logistic=numeric(iter)


for ( i in seq_len(iter) ) { 
    x = rnorm(n)
    y = rdexp(n)   # Laplace (double exponential) distribution
    z = rlogis(n)  # Logistic distribution

     HL1.normal[i]    = HL(x)
     HL1.laplace[i]   = HL(y)
     HL1.logistic[i]  = HL(z)

    mean.normal[i]    = mean(x)
    mean.laplace[i]   = mean(y)
    mean.logistic[i]  = mean(z)

    median.normal[i]  = median(x)
    median.laplace[i] = median(y)
    median.logistic[i]= median(z)
}

var.HL1.normal    = var( HL1.normal   )
var.mean.normal   = var(mean.normal   )
var.median.normal = var(median.normal )

var.HL1.laplace   = var( HL1.laplace  )
var.mean.laplace  = var(mean.laplace  )
var.median.laplace= var(median.laplace)

var.HL1.logistic   =var( HL1.logistic  )
var.mean.logistic  =var(mean.logistic  )
var.median.logistic=var(median.logistic)


# NOTE: var.mean.norma is 1/n theoretically.
VARs = rbind(
  c(var.HL1.normal,   var.mean.normal,   var.median.normal), 
  c(var.HL1.laplace,  var.mean.laplace,  var.median.laplace), 
  c(var.HL1.logistic, var.mean.logistic, var.median.logistic) )
rownames(VARs) = c("Normal", "Laplace", "Logistic")
colnames(VARs) = c("HL1", "Mean", "Median" )
VARs



# RE based on the mean 
var.mean.normal /  c(var.HL1.normal,   var.mean.normal,   var.median.normal)

var.mean.laplace / c(var.HL1.laplace,  var.mean.laplace,  var.median.laplace )   ## median is the best

var.mean.logistic/ c(var.HL1.logistic, var.mean.logistic, var.median.logistic)   ## HL1 is the best

# RE based on the best estimator. Then RE <= 1.0 always.

var.mean.normal /   c(var.HL1.normal,   var.mean.normal,   var.median.normal)

var.median.laplace/ c(var.HL1.laplace,  var.mean.laplace,  var.median.laplace )   ## median is the best

var.HL1.logistic /  c(var.HL1.logistic, var.mean.logistic, var.median.logistic)   ## HL1 is the best



#####################
# REPEAT the above with a very large n and iter 
# n = 1000 and iter=1000000L
# RE of the median based on the mean under the normal distribution is given by 2/pi 
#    2/pi is ARE (asymtotic RE) (that is, when n is very large)


####################################################################
# Normal: mean is the best
# Laplace dist.: median is the best
# Logistic dist.: Hodges-Lehmann may be the best
# ........
# In other words,
# mean is the best under normal
# median is the best under Laplace
# HL may be the best under Logistic.


####################################################################
# QUESTION: with weighted mean or median, which distribution is the best to each of these? 



