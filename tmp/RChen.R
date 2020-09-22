#=======================================================================
#  Function   : Double Exponential Distribution (Laplace distribution)
#-----------------------------------------------------------------------
#
#  Programmer : Park, Chanseok
#  Date       : Mar. 15, 1999
#  
#  Usage      : ddexp(x, location=0, rate=1) 
#               pdexp(q, location=0, rate=1)
#               qdexp(p, location=0, rate=1)
#               rdexp(n, location=0, rate=1)
#=======================================================================
ddexp <-
function(x, location=0, scale=1, rate=1/scale) {
   k <- max(lx<-length(x),lloc<-length(location),lrate<-length(rate))
   if (lx   < k)        x <- rep(x, length=k)
   if (lloc < k) location <- rep(location, length=k)
   if (lrate< k)     rate <- rep(rate, length=k)
   y <- 0.5*rate*exp(-abs(x-location)*rate);
   if(!is.null(Names <- names(x))) names(y) <- rep(Names,length=length(y))
   y
}
##----------
pdexp <-
function(x, location=0, scale=1, rate=1/scale) {
   k <- max(lx<-length(x),lloc<-length(location),lrate<-length(rate))
   if (lx   < k)        x <- rep(x, length=k)
   if (lloc < k) location <- rep(location, length=k)
   if (lrate< k)     rate <- rep(rate, length=k)
   y <- 0.5*exp(-abs(x-location)*rate);
   y <- 0.5 - sign(x-location)*(y-0.5);
   if(!is.null(Names<-names(x))) names(y) <- rep(Names,length=length(y))
   y
}
##----------
qdexp <-
function(p, location=0, scale=1, rate=1/scale) {
   k <- max(lp<-length(p),lloc<-length(location),lrate<-length(rate))
   if (lp   < k)        p <- rep(p, length=k)
   if (lloc < k) location <- rep(location, length=k)
   if (lrate< k)     rate <- rep(rate, length=k)
   index <- sign(0.5-p);
   q <- index*log(1+index*(2*p-1)) / rate + location;
   if(!is.null(Names<-names(p))) names(q)<-rep(Names,length=length(q))
   q
}
##----------
rdexp <-
function(n, location=0, scale=1, rate=1/scale) {
   lloc<-length(location); lrate<-length(rate)
   if (lloc < n) location <- rep(location, length=n)
   if (lrate< n)     rate <- rep(rate, length=n)
   y <- 2*runif(n)-1
   index <- sign(y);
   location - index*log(1-index*(y))/rate;
}
#=======================================================================
HL <- 
function (x, estimator = c("HL1", "HL2", "HL3"), na.rm = FALSE) {
    estimator = match.arg(estimator)
    if (na.rm) 
        x <- x[!is.na(x)]
    xx = outer(x, x, "+")
    HL.estimation = switch(estimator, HL1 = 0.5 * median(xx[lower.tri(xx, 
        diag = FALSE)]), HL2 = 0.5 * median(xx[lower.tri(xx, 
        diag = TRUE)]), HL3 = 0.5 * median(xx))
    return(HL.estimation)
}


