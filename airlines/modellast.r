library(rjags)
 cat( "model
 {
 for( i in 1:I )
 {
 fatal[i] ~ dpois( mu[i] )
 log(mu[i]) <- alpha + beta * miles[i] + beta2 *rate[i] + beta3 *miles[i]*rate[i]
 }
 alpha ~ dnorm(0,0.000001)
 beta ~ dnorm(0,0.000001)
 beta2 ~ dnorm(0,0.000001)
 beta3 ~ dnorm(0,0.000001)

 }",
 file="a3.jag" )

 a3.ini= list(
    list(alpha = 0.01, beta = 0.01, beta2= 0.01, beta3=0.01),
    list(alpha = 0.2, beta = 0.2, beta2=0.2,beta3=0.2),
    list(alpha = 0.7, beta = 0.7, beta2 =0.7, beta3 = 0.7))
 a3.dat <- list( fatal=c(airline$fatal,NA), miles=c(airline$miles,19.775), rate=c(airline$miles,0.7080), I=27 )
 a3.par <- c("alpha","beta","beta2","beta3","fatal[27]")

 # Model compilation and burn-in
 a3.mod <- jags.model(file = "a3.jag",data = a3.dat,inits = a3.ini,n.chains = 3,n.adapt = 1000 )
 # Sampling from the posterior
 a3.res <- coda.samples( a3.mod,var = a3.par,n.iter = 10000,thin = 10 )
 summary( a3.res )
 #predictions
 a3.m <- as.matrix(a3.res)
log.enum.2002 <- a3.m[,"alpha"] + a3.m[,"beta"]*19.775
#print(exp(log.enum.2002))
summary( exp(log.enum.2002 ))
(e2002.qnt <- quantile( enum.2002, probs=c(50,2.5,97.5)/100))
plot( density(enum.2002), type="l", lwd=2, col= "red" )
abline( v=e2002.qnt)