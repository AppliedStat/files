

library(rQCC)


iter = 10000L

n = 50
MADs = numeric(iter)
MADs.unbiased = numeric(iter)

for ( i in seq_len(iter) ) { 
    x = rnorm(n)
    MADs[i] = mad(x)
    MADs.unbiased[i] = mad.unbiased(x)

}

mean(MADs)
mean(MADs.unbiased)

