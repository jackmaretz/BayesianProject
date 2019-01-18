library(rjags)
library(lattice)
airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")


# model -------------------------------------------------------------------


cat("model {
  for( i in 1:I ){
    mu[i] <- lambda * miles[i]
    fatal[i] ~ dpois( mu[i] )
    }
  lambda ~ dgamma(0.01,0.01)
  }", file="a2.jag" )


# glm ---------------------------------------------------------------------


summary(glm(fatal~I(year-1985), data=airline))


######################


library(rjags)
library(lattice)
library(ggplot2)
library("ggmcmc")

airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")

cat( "model
     {
     for( i in 1:I )
     {
     mu[i] <- alpha + beta* miles[i]
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

theta <- rgamma(6000, 634, 26 )
y.2002 <- rpois(6000,theta)
plot( table(y.2002), type="h", lwd=5, lend=2, col=gray(0.2), bty="n", ylab="", xlim=c(5,50) )
tpr <- table( as.matrix( a3.res[,"fatal[27]"] ) )
points( as.numeric(names(tpr))+0.4, tpr, type="h", col="red", lwd=4 )


result3 = ggs(a3.res)
ggs_density(result3)
ggs_traceplot(result3)
ggs_running(result3)