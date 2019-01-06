library(rjags)
library(lattice)
airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")

cat( "model
{
for( i in 1:I )
{
mu[i] <- (alpha + beta*(i-10)) * miles[i]
fatal[i] ~ dpois( mu[i] )
}
alpha ~ dnorm(0,0.000001)
beta ~ dnorm(0,0.000001)
}",file="a3.jag" )

a3.ini <- list( list( alpha=10, beta=-0.5 ),list( alpha=20, beta=-0.6 ),list( alpha=30, beta=-0.4 ) )

a3.dat <- list( fatal=c(airline$fatal,NA),miles=c(airline$miles,20), I=27 )

a3.par <- c("alpha","beta","fatal[27]")

# Model compilation and burn-in
a3.mod <- jags.model( file = "a3.jag",data = a3.dat,inits = a3.ini,n.chains = 3,n.adapt = 1000 )
# Sampling from the posterior
a3.res <- coda.samples( a3.mod,var = a3.par,n.iter = 10000,thin = 10 )
summary( a3.res )

print( xyplot( a3.res[,1:2] ) )