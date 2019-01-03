cat( "model
 {
 for( i in 1:I )
 {
 mu[i] <- exp( alpha + beta*(i-10) ) * miles[i]
 fatal[i] ~ dpois( mu[i] )
 }
 alpha ~ dnorm(0,0.000001)
 beta ~ dnorm(0,0.000001)
 }", file="a4.jag" )
a4.ini <- list( list( alpha=1.0, beta=-0.05 ),list( alpha=1.5, beta=-0.06 ),list( alpha=0.5, beta=-0.04 ) )
a4.dat <- list( fatal=c(airline$fatal,NA),miles=c(airline$miles,20), I=27 )
a4.par <- c("alpha","beta","fatal[27]")
 # Model compilation and burn-in
a4.mod <- jags.model( file = "a4.jag",data = a4.dat,inits = a4.ini,n.chains = 3,n.adapt = 1000 )