
rats.data = list(n =5, r = 30,
                 x = c(8,15,22,20,36),
                 Y = matrix(c(151, 199, 246, 283, 320,
                              145, 199, 249, 293, 354,
                              147, 214, 263, 312, 328,
                              155, 200, 237, 272, 297,
                              135, 188, 230, 280, 323,
                              159, 210, 252, 298, 331,
                              141, 189, 231, 275, 305,
                              159, 201, 248, 297, 338,
                              177, 236, 285, 350, 376,
                              134, 182, 220, 260, 296,
                              160, 208, 261, 313, 352,
                              143, 188, 220, 273, 314,
                              154, 200, 244, 289, 325,
                              171, 221, 270, 326, 358,
                              163, 216, 242, 281, 312,
                              160, 207, 248, 288, 324,
                              142, 187, 234, 280, 316,
                              156, 203, 243, 283, 317,
                              157, 212, 259, 307, 336,
                              152, 203, 246, 286, 321,
                              154, 205, 253, 298, 334,
                              139, 190, 225, 267, 302,
                              146, 191, 229, 272, 302,
                              157, 211, 250, 285, 323,
                              132, 185, 237, 286, 331,
                              160, 207, 257, 303, 345,
                              169, 216, 261, 295, 333,
                              157, 205, 248, 289, 316,
                              137, 180, 219, 258, 291,
                              153, 200, 244, 286, 324),nrow = 30,ncol = 5, byrow = TRUE ))

weekDay = rats.data$x
matplot(t(rats.data$Y),type="b", pch=15:19, xlab = "week",ylab="weight (gr)")
matplot(rats.data$x,t(rats.data$Y),type ="p",add = T)
title(main = " Rats growing rate per week measurement")
grid()

matplot(t(rats.data$Y),type="l",xlab = "week",xlab="weight(gr)")

# MODEL definition --------------------------------------------------------
Data = list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,   
            Y = structure(
              .Data = c(151, 199, 246, 283, 320,
                        145, 199, 249, 293, 354,
                        147, 214, 263, 312, 328,
                        155, 200, 237, 272, 297,
                        135, 188, 230, 280, 323,
                        159, 210, 252, 298, 331,
                        141, 189, 231, 275, 305,
                        159, 201, 248, 297, 338,
                        177, 236, 285, 350, 376,
                        134, 182, 220, 260, 296,
                        160, 208, 261, 313, 352,
                        143, 188, 220, 273, 314,
                        154, 200, 244, 289, 325,
                        171, 221, 270, 326, 358,
                        163, 216, 242, 281, 312,
                        160, 207, 248, 288, 324,
                        142, 187, 234, 280, 316,
                        156, 203, 243, 283, 317,
                        157, 212, 259, 307, 336,
                        152, 203, 246, 286, 321,
                        154, 205, 253, 298, 334,
                        139, 190, 225, 267, 302,
                        146, 191, 229, 272, 302,
                        157, 211, 250, 285, 323,
                        132, 185, 237, 286, 331,
                        160, 207, 257, 303, 345,
                        169, 216, 261, 295, 333,
                        157, 205, 248, 289, 316,
                        137, 180, 219, 258, 291,
                        153, 200, 244, 286, 324),
              .Dim = c(30,5)))

model1="
model
{
  for( i in 1 : N ) {
    for( j in 1 : T ) {
      mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
      Y[i , j] ~ dnorm(mu[i , j],tau.c)

    }
    alpha[i] ~ dnorm(alpha.mu,alpha.tau);
    beta[i] ~ dnorm(beta.mu,beta.tau)
  }

  #priors
  
  #sigma <- 1 / sqrt(tau.c);

  alpha.mu ~ dnorm(0.0,1.0E-6);   
  beta.mu ~ dnorm(0.0,1.0E-6);
  tau.c ~ dgamma(0.001,0.001);
  alpha.tau ~ dgamma(0.001,0.001);
  beta.tau ~ dgamma(0.001,0.001);

  #alpha0 <- alpha.c - xbar * beta.c }

  #tranformations
  alpha.sigma = 1.0 / sqrt(alpha.tau);
  beta.sigma = 1.0 / sqrt(beta.tau)
  sigma.c = 1.0 / sqrt(tau.c)
  x.bar = mean(x[]);
  alpha0 = alpha.mu - beta.mu*x.bar
  }"
  
writeLines(model1,"model.text")


# MCMC  -------------------------------------------------------------------

library(rjags)
  
model1 = jags.model("model.text", data = Data, n.chains =2)

update(model1,5000)
model1.samples = coda.samples(model=model1,variable.names = c('alpha.mu',
                                                              'alpha.sigma',
                                                              'beta.mu',
                                                              'beta.sigma',
                                                              'sigma.c'),
                              n.iter = 10000)

summary(model1.samples)

# basic plot and diagnostics ----------------------------------------------

library(ggmcmc)

#traceplot(model1.samples)
res = ggs(model1.samples)
ggs_density(res)

ggs_traceplot(res)

ggs_running(res)

ggs_autocorrelation(res)

ggs_crosscorrelation(res)
 

# Predictions -------------------------------------------------------------
library(coda,nlme)
chain1 = model1.samples[[1]]
alphachain = chain1[,"alpha.mu"]
betachain = chain1[, "beta.mu"]
alphasigchain = chain1[, "alpha.sigma"]
betasigchain = chain1[,"beta.sigma"]
predict_rat = function(rat.x){
  alpha.i = rnorm(alphachain,alphachain,alphasigchain)
  beta.i = rnorm(betachain,betachain,betasigchain)
  Ypred = alpha.i + beta.i*rat.x
  inter = HPDinterval(as.mcmc(Ypred))
  res2 =c(inter[1],inter[2])
  res2
}

intervaldata = data.frame(0:40)
colnames(intervaldata) = c("x")
intervaldata[C("min","max")] = t(mapply(predict_rat, intervaldata$x))
intervaldata$min= t(mapply(predict_rat, intervaldata$x))[,1]
intervaldata$max= t(mapply(predict_rat, intervaldata$x))[,2]

fig = ggplot(data.frame(x =c(0:40)), aes(x=x))+ geom_ribbon(data= intervaldata, aes(ymin=min,ymax=max),alpha=0.3)+ stat_function(fun=line,args = list(summary(model1.samples)$statistics["alpha.mu",1], summary(model1.samples)$statistics["beta.mu",1]),colour="green",size =1)
fig
                                                                                                                                 