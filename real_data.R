
library(tidyverse)
library(janitor)
library(mvtnorm) #multivariate normal and T 
library(BMA)
library(lars)
library(parallel)
library(mombf)


source('funciones.R')

data=read.csv('data/datanew.csv')
X=as.matrix(data[,-1])
y=data[,1]

set.seed(54)
A=read.csv('data/pricesnew.csv')%>%
  mutate(second_stage=simple_lasso(X,y))



A%>%
  group_by(second_stage,Price)%>%
  summarise(vars=paste(Regressor,collapse = ','))%>%
  write.csv('real_data_results_res.csv')



