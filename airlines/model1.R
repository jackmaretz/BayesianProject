
airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")
library(rjags)
library(lattice)
cat( "model {
  for( i in 1:I ) {
    fatal[i] ~ dpois(mu)
    }
    mu ~ dgamma(0.01,0.01)
  }", file="a1.jag" )

a1.par <- c("mu","fatal[27]")

a1.ini <- list(list( mu=22 ),
              list( mu=23 ),
              list( mu=24 ) )

a1.dat <- list( fatal = c(airline$fatal,NA), I=27 )

# Model compilation and burn-in
a1.mod <- jags.model( file = "a1.jag",
                    data = a1.dat,
                    inits = a1.ini,
                    n.chains = 3,
                    n.adapt = 1000 )
  
# Sampling from the posterior
a1.res <- coda.samples( a1.mod,
                      var = a1.par,
                      n.iter = 10000,
                      thin = 10 )
summary( a1.res )
print( xyplot( a1.res[,1:2] ) )

theta <- rgamma(6000, 634, 26 )
y.2002 <- rpois(6000,theta)
plot( table(y.2002), type="h", lwd=5, lend=2, col=gray(0.2), bty="n", ylab="", xlim=c(5,50) )
tpr <- table( as.matrix( a1.res[,"fatal[27]"] ) )
points( as.numeric(names(tpr))+0.4, tpr, type="h", col="red", lwd=4 )



###################

