library(tidyverse)
library(janitor)
library(mvtnorm) #multivariate normal and T 
library(BMA)
library(lars)
library(parallel)
library(mombf)


source('funciones.R')

data=read.csv('data/datanew.csv')
prices=read.csv('data/pricesnew.csv')%>%
  mutate(Price=Price/min(Price))%>%
  dplyr::select(Price)%>%
  pull()

sigma=data%>%
  dplyr::select(-y)%>%
  cov()

n=200
initial=lm(y~.,data=data)
signal2noise=var(initial$fitted.values)/var(data$y)
signal2noise=signif(signal2noise,2)
folder=paste0('noise',signal2noise)
dir.create(file.path( folder), showWarnings = FALSE)
total_budget=sum(prices)*n
semillas=as.list(1:200)


nc=detectCores()
cl <- makeCluster(nc)
clusterExport(cl=cl, varlist=c("get_X_y_oracle", 
                               "get_new_X_y", 
                               "get_mse", 
                               "simple_lasso",
                               "ada_lasso",
                               "bma.bic",
                               "bma.nl",
                               "ada_lasso_prop",
                               "lasso_prop",
                               "adalasso2"
                               ))

t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                  k=10,n=200,prices=prices,
                  total_budget=total_budget,sigma=sigma,
                  signal2noise=signal2noise)
#resultados=lapply(semillas, simulacion_experimento,
#                     k=10,n=200,prices=prices,
#                     total_budget=total_budget,sigma=sigma,
#                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_10.csv'))


### K=5
t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=5,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_5.csv'))



##### K=20


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=20,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_20.csv'))



##### K=40


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=40,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_40.csv'))



stopCluster(cl)



signal2noise=0.1
folder=paste0('noise',signal2noise)
dir.create(file.path( folder), showWarnings = FALSE)



nc=detectCores()
cl <- makeCluster(nc)
clusterExport(cl=cl, varlist=c("get_X_y_oracle", 
                               "get_new_X_y", 
                               "get_mse", 
                               "simple_lasso",
                               "ada_lasso",
                               "bma.bic",
                               "bma.nl",
                               "ada_lasso_prop",
                               "lasso_prop",
                               "adalasso2"
))

t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=10,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)

print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_10.csv'))


### K=5
t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=5,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_5.csv'))



##### K=20


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=20,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_20.csv'))



##### K=40


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=40,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_40.csv'))



stopCluster(cl)




signal2noise=1
folder=paste0('noise',signal2noise)
dir.create(file.path( folder), showWarnings = FALSE)



nc=detectCores()
cl <- makeCluster(nc)
clusterExport(cl=cl, varlist=c("get_X_y_oracle", 
                               "get_new_X_y", 
                               "get_mse", 
                               "simple_lasso",
                               "ada_lasso",
                               "bma.bic",
                               "bma.nl",
                               "ada_lasso_prop",
                               "lasso_prop",
                               "adalasso2"
))

t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=10,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)

print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_10.csv'))


### K=5
t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=5,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_5.csv'))



##### K=20


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=20,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_20.csv'))



##### K=40


t=Sys.time()
resultados=parLapply(cl,semillas, simulacion_experimento,
                     k=40,n=200,prices=prices,
                     total_budget=total_budget,sigma=sigma,
                     signal2noise=signal2noise)
print(Sys.time()-t)

resultados_org=do.call(rbind,resultados)

write.csv(resultados_org,paste0(folder,'/resultados_40.csv'))



stopCluster(cl)




