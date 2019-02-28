
air.val <- data.frame("year1975" = 27:29, "year" = c(2002,2003,2004), "fatal" = c(14,7,9),"miles"=c(19.775,23.300,20.300), "rate"=c(0.7080,0.3004,0.4433))
air.train= airline[1:20, ]
air.test= airline[21:26, ]


# rjags -------------------------------------------------------------------

library(jags)

beta0 <- 1  # intercept
beta1 <- 1  # slope
mu <- beta0*1 + beta1*air.train$miles  # linear predictor function
lambda <- exp(mu)  # CEF
dat <- air.train  
N=nrow(air.train)

cat("model{
    ## Likelihood
    for(i in 1:N){
    fatal[i] ~ dpois(lambda[i])
    log(lambda[i]) <- mu[i]
    mu[i] <- beta0+beta1*miles[i]
    }     
    ## Prior
    beta0 ~ dnorm(0.01,2)  #  Normal prior
    beta1 ~ dnorm(0.01,2)
    }" , file = "count.jag")

count.par = c("beta0","beta1")

count.dat <- list(miles=cbind(dat$miles),  # predictors
                  fatal=dat$fatal,  # DV
                  N=N)  # sample size
                  # mu.beta=c(0.1,2),  # priors centered on 0
                  # tau.beta=c(0.0001,2))  # diffuse priors
library(rjags)

jagsmodel <- jags.model(file = "count.jag",  # compile model
                        data=count.dat,n.chains = 2,n.adapt = 1000)
