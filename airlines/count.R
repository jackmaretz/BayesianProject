library(jags)
cat("model{
    ## Likelihood
    for(i in 1:N){
      y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- mu[i]
      mu[i] <- alpha+beta*miles[i]
      }     
    ## Priors 
    beta ~ dnorm(mu.beta,tau.beta)  # multivariate Normal prior
}" , file = "count.jag")

count.par = c("beta","alpha")

count.dat <- list(X=cbind(1,dat$x),  # predictors
                y=dat$y,  # DV
                N=N,  # sample size
                mu.beta=rep(0,2),  # priors centered on 0
                tau.beta=diag(.0001,2))  # diffuse priors