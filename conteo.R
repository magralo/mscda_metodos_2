### Binaria

library(tidyverse)
library(BMA)
library(pROC)


train=read.csv('../data/raw/datacountstudents.csv')%>%
  dplyr::select(-id)%>%
  rename(y=yC)%>%
  filter(!is.na(y))

y=train%>%
  dplyr::select(y)%>%
  as.matrix()
x=train%>%
  dplyr::select(-y)%>%
  as.matrix()

source('funciones.R')

t=Sys.time()
semillas=as.list(1:100)

las=lapply(semillas,lasso_c,x,y)

saveRDS(las, "las_lasso_con.rds")



spip=prob_inc(las)
betas=get_betas(las)
colnames(betas)=colnames(x)

ok=colnames(x)[spip>0.5]
data.frame(betas)%>%
  dplyr::select(ok)%>%
  mutate(seed=1:nrow(betas))%>%
  gather(beta,coef,-seed)%>%
  filter(coef!=0)%>%
  ggplot(aes(x=coef,fill=beta))+geom_histogram()+facet_wrap(~beta,scales = 'free')

x_red=data.frame(x)%>% ##quitamos x1 por el cambio de signo??
  dplyr::select(x14,x22,x24,x25,x26,x3,x32,x4)%>%
  as.matrix()

evals=lapply(semillas,eval_c,x_red,y) ##esta con round y dio mejor


train=data.frame(x_red)%>%
  mutate(y=y)

model=glm(y~.,family="poisson",data = train)
summary(model)

mets=get_met(evals)

data.frame(spec=mets$spec,global=mets$global)%>%
  mutate(seed=1:length(mets$spec))%>%
  gather(metrica,value,-seed)%>%
  filter(value<10)%>%
  ggplot(aes(x=metrica,y=value,fill=metrica))+
  geom_boxplot()+
  facet_wrap(~metrica,scales = 'free')

median(mets$global)

print(Sys.time()-t)


t=Sys.time()

las=lapply(semillas,bics_c,x,y)

saveRDS(las, "las_bic_con.rds")


spip=prob_inc(las)
betas=get_betas(las)
colnames(betas)=colnames(x)

ok=colnames(x)[spip>0.5]
data.frame(betas)%>%
  dplyr::select(ok)%>%
  mutate(seed=1:nrow(betas))%>%
  gather(beta,coef,-seed)%>%
  filter(coef!=0)%>%
  ggplot(aes(x=coef,fill=beta))+geom_histogram()+facet_wrap(~beta,scales = 'free')

x_red=data.frame(x)%>% ##quitamos x1 por el cambio de signo??
  dplyr::select(x22,x24,x25,x3)%>%
  as.matrix()

evals=lapply(semillas,eval_c,x_red,y)


train=data.frame(x_red)%>%
  mutate(y=y)

model=glm(y~.,family="poisson",data = train)
summary(model)

mets=get_met(evals)

data.frame(spec=mets$spec,global=mets$global)%>%
  mutate(seed=1:length(mets$spec))%>%
  gather(metrica,value,-seed)%>%
  filter(value<10)%>%
  ggplot(aes(x=metrica,y=value,fill=metrica))+
  geom_boxplot()+
  facet_wrap(~metrica,scales = 'free')


print(Sys.time()-t)

