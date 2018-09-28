library(ggplot2)
library(ggmcmc)
library(coda)
library(nlme)



Data <- list(
  N = 30,
  T = 5,
  Y = structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134,
                  160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146,
                  157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210,
                  189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212,
                  203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249,
                  263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242,
                  248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248,
                  219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313,
                  273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285,
                  286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305,
                  338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321,
                  334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30,
                                                                              5)),
  x = c(8.0, 15.0, 22.0, 29.0, 36.0)
)

Data.org <- data.frame(
  xj=rep(c(8, 15, 22, 29, 36), each=30),
  i=rep(1:30, 5),
  Y=as.vector(Data$Y))

ggplot(Data.org, aes(xj, Y)) + geom_point(shape = 1) +
  facet_wrap(~i)

lmfit <- lmList(Y ~ xj | i, Data.org[, ])
fig = ggplot(data.frame(x = c(0,40)), aes(x=x))

for(fit in lmfit){
  coeff = fit$coefficients
  2
  line <- function(x, alpha, beta){alpha+beta*x}
  fig = fig + stat_function(fun=line, args = list(coeff[1], coeff[2]), colour="dodgerblue", size = 1)
}
fig = fig + ggtitle("Linear regression curves")
fig

summary(lmList(Y ~ xj | i, Data.org[Data.org$i %in% 1:5, ]))

means = c(
  mean_alpha = mean(sapply(lmfit, function(X) coef(X)[1])), # mean of intercept
  mean_beta = mean(sapply(lmfit, function(X) coef(X)[2])), # mean of slope
  sd_alpha = sd(sapply(lmfit, function(X) coef(X)[1])), # sd of intercept
  sd_beta = sd(sapply(lmfit, function(X) coef(X)[2])) # sd of slope
)
means

fig = fig + stat_function(fun=line, args = list(means["mean_alpha"],means["mean_beta"]), colour="red", size = 1.5)
fig


modelstring <- "
model {
# Model
for (i in 1:N) {
for (j in 1:T) {
mu[i, j] <- alpha[i] + beta[i] * (x[j]);
Y[i,j] ~ dnorm(mu[i,j], tau.c)
}
alpha[i] ~ dnorm(alpha.mu, alpha.tau);
beta[i] ~ dnorm(beta.mu, beta.tau);
}
# Priors
alpha.mu ~ dnorm(0, 1.0E-4);
beta.mu ~ dnorm(0, 1.0E-4);
tau.c ~ dgamma(1.0E-3, 1.0E-3);
alpha.tau ~ dgamma(1.0E-3, 1.0E-3);
beta.tau ~ dgamma(1.0E-3, 1.0E-3);
# Transformations
alpha.sigma <- 1.0/sqrt(alpha.tau);
beta.sigma <- 1.0/sqrt(beta.tau);
sigma.c <- 1.0/sqrt(tau.c);
x.bar <- mean(x[]);
alpha0 <- alpha.mu - beta.mu*x.bar;
}
"
writeLines(modelstring, "model.txt")

mod1 <- jags.model("model.txt", data=Data, n.chains=2)

#Burn In
update(mod1, 5000)
mod1.samples <- coda.samples(model=mod1,
                             variable.names=c('alpha.mu',
                                              'alpha.sigma',
                                              'beta.mu',
                                              'beta.sigma',
                                              'sigma.c'),
                             n.iter=10000)

library("ggmcmc")

result <- ggs(mod1.samples)
ggs_density(result)
ggs_traceplot(result)
ggs_running(result)
ggs_autocorrelation(result)
ggs_crosscorrelation(result)


# prediction --------------------------------------------------------------

chain1 = mod1.samples[[1]]
alphachain = chain1[,"alpha.mu"]
betachain = chain1[,"beta.mu"]
alphasigchain = chain1[,"alpha.sigma"]
betasigchain = chain1[,"beta.sigma"]
predict_rat <- function(rat.x){
  alpha.i = rnorm(alphachain,alphachain, alphasigchain)
  beta.i = rnorm(betachain,betachain, betasigchain)
  Ypred = alpha.i + beta.i*rat.x
  inter = HPDinterval(as.mcmc(Ypred))
  res = c(inter[1], inter[2])
  res
}
intervaldata = data.frame(0:40)
colnames(intervaldata)<-c("x")
intervaldata[c("min", "max")] = t(mapply(predict_rat, intervaldata$x))
fig = ggplot(data.frame(x = c(0,40)), aes(x=x))+
  geom_ribbon(data=intervaldata,aes(ymin=min,ymax=max),alpha=0.3) +
  stat_function(fun=line, args = list(summary(mod1.samples)$statistics["alpha.mu",1],
                                      summary(mod1.samples)$statistics["beta.mu",1]),
                colour="green", size = 1)

fig


# birats ------------------------------------------------------------------

bidata <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), N = 30, T = 5,
     Omega = structure(.Data = c(200, 0, 0, 0.2), .Dim = c(2, 2)),   
     mean = c(0,0),
     prec = structure(.Data = c(1.0E-6, 0, 0, 1.0E-6), .Dim = c(2, 2)),
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

bidata <- read.jagsdata("birats-data.R")

modelstring <- "
var
x[T],mu[N,T],Y[N,T],beta[N,2],mu.beta[2],Omega.beta[2,2],
Sigma2.beta[2,2],sigma.beta[2],tau.c,sigma,R[2,2],r,alpha0;
model {
for (i in 1:N) {
for (j in 1:T) {
Y[i,j] ~ dnorm(mu[i,j],tau.c); #
mu[i,j] <- beta[i,1] + beta[i,2] * x[j];
}
beta[i,] ~ dmnorm(mu.beta[],Omega.beta[,]); # bivariate Normal
}
tau.c ~ dgamma(1.0E-3,1.0E-3);
sigma <- 1.0/sqrt(tau.c);
# parameters considered MVN
Omega.beta[,] ~ dwish(R[,],2); # Wishart prior
Sigma2.beta[,] <- inverse(Omega.beta[,]);
sigma.beta[1]<-sqrt(Sigma2.beta[1,1]);
sigma.beta[2]<-sqrt(Sigma2.beta[2,2]);
r <- Sigma2.beta[1,2] / (sqrt(Sigma2.beta[1,1])
*sqrt(Sigma2.beta[2,2])); # correlation
mu.beta[1] ~ dnorm(0,.00001); # `flat' univariate Normal prior on mean
mu.beta[2] ~ dnorm(0,.00001); # `flat' univariate Normal prior on mean
}
"

writeLines(modelstring, "model3.txt")

init <- read.jagsdata("birats-inits.R")

mod3 <- jags.model("model3.txt",inits = init, data=bidata, n.chains=2)

