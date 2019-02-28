model.loc = ("ss_model.txt")
jagsscript = cat("
                 model {  
                 # priors on parameters
                 mu ~ dnorm(0, 0.01); 
                 tau.pro ~ dgamma(0.001,0.001); 
                 sd.q <- 1/sqrt(tau.pro);
                 tau.obs ~ dgamma(0.001,0.001);
                 sd.r <- 1/sqrt(tau.obs); 
                 phi ~ dnorm(0,1);
                 
                 X[1] <- mu;
                 predY[1] <- X[1];
                 Y[1] ~ dnorm(X[1], tau.obs);
                 
                 for(i in 2:N) {
                 predX[i] <- phi*X[i-1]; 
                 X[i] ~ dnorm(predX[i],tau.pro); # Process variation
                 predY[i] <- X[i];
                 Y[i] ~ dnorm(X[i], tau.obs); # Observation variation
                 }
                 }  
                 ", 
                 file = model.loc)

jags.data = list(Y = Wind, N = N)
jags.params = c("sd.q", "sd.r", "predY", "mu")
mod_ss = jags(jags.data, parameters.to.save = jags.params, model.file = model.loc, 
              n.chains = 3, n.burnin = 5000, n.thin = 1, n.iter = 10000, 
              DIC = TRUE)