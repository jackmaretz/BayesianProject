#model.loc = ("lm.txt")
jagsscript = cat("
                 model {  
                 mu ~ dnorm(0, 0.01); 
                 beta ~ dnorm(0,0.01);
                 tau.obs ~ dgamma(0.001,0.001); 
                 sd.obs <- 1/sqrt(tau.obs); 
                 
                 for(i in 1:N) {
                 predY[i] <- mu + C[i]*beta; 
                 Y[i] ~ dnorm(predY[i], tau.obs);
                 }
                 }  
                 ", 
                 file = "model.jag")

jags.dat = list(Y = airline$fatal, N = N, C = airline$miles)
jags.params = c("sd.obs", "predY", "mu", "beta")
jags.ini <- list( list( mu=0.1, beta=0.01,tau.obs=0.01 ),
                list( mu=0.2, beta=0.02 ,tau.obs=0.02),
                list( mu=0.5, beta=0.03 ,tau.obs=0.03 ))
# mod_lm = jags.model(data= jags.data,  = jags.params, file = "model.jag", 
#               n.chains = 3, n.adapt = 5000, n.thin = 1, n.iter = 10000)
jags.mod <- jags.model( file = "model.jag",
                      data = jags.dat,
                      inits = jags.ini,
                      n.chains = 3,
                      n.adapt = 1000 )
jags.res <- coda.samples( jags.mod,
                        var = jags.params,
                        n.iter = 10000,
                        thin = 1 )
summary( jags.res )

N=20
jags.data = list(Y = c(air.test$fatal, NA, NA, NA, NA, NA, NA), N = (N + 6))
jags.params = c( "predY", "mu")
model.loc = ("ss_model.txt")
mod_ss_forecast = jags.model(file = "model.jag",
                             data = jags.dat,
                             inits = jags.ini,
                             n.chains = 3,
                             n.adapt = 5000)
jags.res <- coda.samples( jags.mod,
                          var = jags.params,
                          n.iter = 10000,
                          thin = 1 )
summary( jags.res )$statistics[,"Mean"]
resid = summary( jags.res )$statistics[,"Mean"][2:21]
yres = as.numeric(resid)
length(air.train$fatal)
length(yres)
plot(yres,air.train$fatal)
