---
title: "Airlines Fatalities"
author: "Giacomo Maretto"
date: "28 febbraio 2019"
output: html_document
---


```{r, echo=FALSE,include=FALSE}
#install.packages("rjags","coda","Epi","lme4","pixmap","sp")
library(ggplot2)
library(ggfortify)
library(ggpubr)
library("ggmcmc")
library(rjags)
library(ggplot2)
library(knitr)
library(pander)
library(lattice)
airline <- read.table("http://www.uta.fi/sis/mtt/mttts12/jags/airline.txt",h=T,sep="")
set.seed(123)
```