airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")
library(rjags)
library(lattice)

## complex model
#2. 
# (a) Let mi = number of passenger miles 
# flown in year i and gamma = accident rate per
# passenger mile. The model for the data is yijmi; gamma-> Poisson(mi*gamma). We use the
# noninformative 􀀀(0; 0) prior distribution for  as we did for  previously.
# The posterior distribution for gamma is gamma given y; m -> 􀀀(ny; n m) = 􀀀(634; 275:56) where
# n m =
#   P26
# i=1mi:
#   > sum( airline$miles )
# [1] 275.564
# Note that the model is invariant under scaling of m in the sense that if the ms
# are divided by a factor K then  is multiplied by K. In this exercise we have
# used the ms in the units of 1011miles as they are given in the le airline.csv.

cat("model {
  for( i in 1:I ){
    mu[i] <- lambda * miles[i]
    fatal[i] ~ dpois( mu[i] )
    }
  lambda ~ dgamma(0.01,0.01)
  }", file="a2.jag" )

a2.ini <- list( list( lambda=10 ),list( lambda=20 ),list( lambda=30 ) )
a2.dat <- list( fatal=c(airline$fatal,NA),miles=c(airline$miles,20), I=27 )
a2.par <- c("mu","fatal[27]")

# Model compilation and burn-in
a2.mod <- jags.model( file = "a2.jag",data = a2.dat,inits = a2.ini,n.chains = 3,n.adapt = 1000 )

# Sampling from the posterior
a2.res <- coda.samples( a2.mod,var = a2.par,n.iter = 10000,thin = 10 )
summary( a2.res )
  
print( xyplot( a2.res[,1:2] ) )

 ################################ 
#A closer inspection of the number of fatal airline crashes can be dome by:
par(mfrow=c(1,2))
with(airline, plot( year, fatal, pch=16, type="b", ylim=c(0,32), bty="n" ) )
with(airline, plot( year, rate, pch=16, type="b", ylim=c(0,7), bty="n" ) )
