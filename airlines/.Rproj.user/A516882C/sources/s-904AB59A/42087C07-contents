# http://www.uta.fi/sis/mtt/mttts12/jags.html

install.packages("rjags","coda","Epi","lme4","pixmap","sp")
library(rjags)

airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")
sum( airline$fatal ) # the column fatal which contains the annual number of fatalities.



a.dat <- list( fatal = c(airline$fatal,NA), I=27 ) 
#Provide data to jags : the vector of fatal airline accidents expanded with a NA for prediction of the number in 2002

#Here is the BUGS code specifying the above model, using cat to put it in the file 'm1.jag':

cat( "model{
for( i in 1:I )
 {
  fatal[i] ~ dpois(lambda)
 }
lambda ~ dgamma(0.1,0.1)
}",
file="m1.jag" )

a.ini <- list( list( lambda=20 )) 
a.ini <- list( list( lambda=20 ), list( lambda=23 ), list( lambda=26 ) )

#for 3 chains and 2000 cycles of burn-in:
m <- jags.model( file = "m1.jag", data = a.dat,  n.chains = 3, inits = a.ini, n.adapt = 2000 )

res <- coda.samples( m, var = "lambda",  n.iter = 10000, thin = 10 )

class( res )  # [1] "mcmc.list"
summary( res )

par( mfrow=c(1,2) )
plot( res )


a1.par <- c("lambda","fatal[27]")    # For prediction
res1 <- coda.samples(m, var = a1.par, n.iter = 10000, thin=10)
#Each element of the list is a 1000 by 2 matrix.

summary(res1)   
plot(res1)

