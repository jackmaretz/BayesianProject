airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")

airline

plot(airline$year,airline$fatal)

airline.lm = lm(airline$year~ airline$fatal)

par(mfrow=c(2,3))
plot(airline.lm)
summary(airline.lm)
####################
library(rjags)
cat( "model {
  for( i in 1:I ) {
     fatal[i] ~ dpois(mu[i])
     }
     mu[i] ~ alpha+beta*(miles[i]-mean(miles[]))
     alpha ~ dnorm(0,0.01)
     beta ~ dnorm(0,0.01)
     }", file="ar.jag" )

ar.par <- c("alpha","beta")

ar.ini <- list( list( alpha=1.0, beta=0.05 ),
                list( alpha=1.5, beta=0.06 ))

ar.dat <- list( fatal=airline$fatal,
                miles=airline$miles, I=26 )

# Model compilation and burn-in
ar.mod <- jags.model( file = "ar.jag",
                      data = ar.dat,
                      inits = ar.ini,
                      n.chains =2,
                      n.adapt = 1000 )

# Sampling from the posterior
a1.res <- coda.samples( a1.mod,
                        var = a1.par,
                        n.iter = 10000,
                        thin = 10 )
summary( a1.res )
#########################
theta <- rgamma(6000, 634, 26 )
y.2002 <- rpois(6000,theta)
plot( main = 'Posterior predictive distribution of y in 2002',table(y.2002), type="h", lwd=5, lend=2, col=gray(0.2), bty="n", ylab="", xlim=c(5,60) )
tpr <- table( as.matrix( a1.res[,"fatal[27]"] ) )
points( as.numeric(names(tpr))+0.4, tpr, type="h", col="red", lwd=4 )
legend("topright", c('Directly simulated','posterior values from BUGS output'),col=c(gray(0.2),'red'),lwd = 5)

############################
cat("model {
  trend[1] <- beta0 + beta1 * miles[1]
  for( i in 2 : 26 ) {
    fatal[i] ~ dnorm( mu[i] , tau)
    mu[i] <- trend[i] + ar1 * ( fatal[i-1] - trend[i-1] )
    trend[i] <- beta0 + beta1 * miles[i]
  }
  ar1 ~ dunif(-1.1,1.1) # or dunif(-0.01,0.01)
  beta0 ~ dnorm( 0 , 1.0E-12 )
  beta1 ~ dnorm( 0 , 1.0E-12 )
  tau ~ dgamma( 0.001 , 0.001 )}" ,file= "ar1.jag")
ar1.par <- c("ar1","beta0", "beta1", "tau")
ar1.ini <- list( list( ar1=0.01, beta0=0.01, beta1 = 0.01,tau=0.01 ))
ar1.dat <- list( fatal=airline$fatal,
                miles=airline$miles)
ar1.mod <- jags.model( file = "ar1.jag",
                      data = ar1.dat,
                      inits = ar1.ini,
                      n.chains =1,
                      n.adapt = 1000 )
ar1.res <- coda.samples( ar1.mod,
                        var = ar1.par,
                        n.iter = 10000,
                        thin = 10 )
summary( ar1.res )
library(ggmcmc)
result2 = ggs(ar1.res)
ggs_density(result2)
ggs_traceplot(result2)
ggs_running(result2)
ggs_autocorrelation(result2)

# prediction --------------------------------------------------------------
miles.new = c(19.775,23.300,20.300)

cat("model {
  trend[1] <- beta0 + beta1 * miles[1]
    for( i in 2 : N ) {
      fatal[i] ~ dnorm( mu[i] , tau)
      mu[i] <- trend[i] + ar1 * ( fatal[i-1] - trend[i-1] )
      trend[i] <- beta0 + beta1 * miles[i]
    }
    ar1 ~ dunif(-1.1,1.1) 
    beta0 ~ dnorm( 0 , 1.0E-12 )
    beta1 ~ dnorm( 0 , 1.0E-12 )
    tau ~ dgamma( 0.001 , 0.001 )}" ,file= "ar1_pred.jag")

ar1_pred.par <- c("ar1","beta0", "beta1", "tau")
ar1_pred.ini <- list( list( ar1=0.01, beta0=0.01, beta1 = 0.01,tau=0.01 ))
ar1_pred.dat <- list( fatal=c(airline$fatal,NA,NA,NA),
                 miles=c(airline$miles,miles.new),
                 N= 26+3)

#mod.forecast = jags.model(ar1_pred.dat,parameters.to.save = ar1_pred.par,mode)

ar1_pred.mod <- jags.model( file = "ar1_pred.jag",
                       data = ar1_pred.dat,
                       inits = ar1_pred.ini,
                       n.chains =1,
                       n.adapt = 1000 )
ar1_pred.res <- coda.samples( model = ar1_pred.mod,
                         var = ar1_pred.par,
                         n.iter = 10000,
                         thin = 10 )


ar1.m <- as.matrix(ar1_pred.res)
enum.2002 <-fatal[i] ~ dnorm( (ar1.m[,"beta0"]+ ar1.m[,"beta1"]), ar1.m[,"tau"])   #exp(a2.m[,"alpha"] + a2.m[,"beta"]*17)*20
summary( enum.2002 )
# fun\ --------------------------------------------------------------------
head(ar1_pred.res[,2])
a1.res[,1:2]
# sumary ------------------------------------------------------------------

head(ar1_pred.res)
summary( ar1_pred.res )

posterior_means <- apply(ar1_pred.res, 1, mean)

attach.jags(ar1_pred.mod)


ar1_pred.res
# prediction
# beta0 ~ dnorm(0,10)
# beta1 ~ dnorm(0,10)
# tau ~

miles.new = c(19.775,23.300,20.300)
pred= c()
ar1 ~ dunif(-1.1,1.1) 
beta0 ~ dnorm( 0 , 1.0E-12 )
beta1 ~ dnorm( 0 , 1.0E-12 )
tau ~ dgamma( 0.001 , 0.001 )

for(j in 1:N){
  fatal[i]~dnorm
}