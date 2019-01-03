
## From Timeseries object (ts)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library("ggmcmc")
theme_set(theme_classic())
theme_set(theme_bw())# Plot 
autoplot(airline$year) + 
  labs(title="AirFatal") + 
  theme(plot.title = element_text(hjust=0.5))

# Allow Default X Axis Labels
p = ggplot(airline, aes(x=year)) + 
  geom_line(aes(y=fatal,x =year)) + 
  geom_point(aes(y=fatal,x=year))+
  labs(title="Time Series Chart", 
       subtitle="Returns fatal from 'Airlines' Dataset", 
       caption="Source: American Airlines", 
       y="Fatal")

p  + theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + scale_x_continuous("year", labels = as.character(airline$year), breaks = airline$year)
p


########## density plots
print( xyplot( a1.res[,1:2] ) )

print(densityplot(a1.res[,1:2], aspect ="fill"))

library(plotMCMC)
plotAuto(mcmc=a1.res[,1:2], thin=1, log=FALSE, base=10, main=NULL, xlab="Lag",
         ylab="Autocorrelation", lty=1, lwd=1, col="black")

##############
theta <- rgamma(1000, 634, 26 )
y.2002 <- rpois(1000,theta)
summary(y.2002)

e2002.qnt <- quantile(y.2002,probs = c(50,2.5,97.5)/100)
plot(density(y.2002),type="l",lwd=3)
abline(v=e2002.qnt)


###################

result = ggs(a1.res)
ggs_density(result)
ggs_traceplot(result)
ggs_running(result)
ggs_autocorrelation(result)
ggs_pairs(result) ########mmmmmeh

result2 = ggs(a2.res)
ggs_density(result)
ggs_traceplot(result)
ggs_running(result)
ggs_autocorrelation(result2)


result3 = ggs(a3.res)
ggs_density(result)
ggs_traceplot(result)
ggs_running(result)
ggs_autocorrelation(result3)
