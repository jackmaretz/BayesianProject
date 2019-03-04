library("rjags")

mod_string= cat("model{

for(i in i:N){
  
  fatal[i] ~ dpois(lam[i])    
  log(lam[i]) = alpha + beta1*miles[i] +beta2*rate[i]   }
  alpha ~ dnorm(0.0,0.00001)
  beta1 ~dnorm(0.0,0.001)
  beta2~dnorm(0.0,0.001)
  beta3~dnorm(0.0,0.001)
 }",file = "cur.jag")
data_jags= list(N=20,fatal = air.train$fatal,miles = air.train$miles, rate = air.train$rate)


params = c( "alpha", "beta1", "beta3" ,  "beta3")

mod = jags.model(file = "cur.jag",
                 data = data_jags, 
                 n.chains=1,
                 n.adapt = 1000)

update(mod, 1e3)

mod_sim = coda.samples(model=mod,
						variable.names = params,
						n.iter=5e3)

mod_csim = as.mcmc(do.call(rbind,mod_sim))

str(air.train)
