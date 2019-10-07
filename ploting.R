library(tidyverse)
library(ggthemes)
k=10
noise=1
#file=paste0('costo operacion = 0/noise',noise,'/resultados_',k,'.csv')
file=paste0('noise',noise,'/resultados_',k,'.csv')
resultados_org=read.csv(file)


seed_filtro=resultados_org%>%
  filter(metodologia=='Non local',n>9000)%>%
  dplyr::select(seed)%>%
  pull()

#resultados_org=resultados_org%>%
#  filter(seed%in%seed_filtro)

no_oracle=resultados_org%>%
  mutate(price=18200/n)%>%
  dplyr::select(metodologia,mse,price,accuracy,seed)%>%
  filter(metodologia!='Oracle')

oracle=resultados_org%>%
  filter(metodologia=='Oracle')%>%
  dplyr::select(mse,seed)%>%
  rename(mse_oracle=mse)

all=no_oracle%>%
  inner_join(oracle,by='seed')%>%
  mutate(mse_ben=mse/mse_oracle)%>%
  dplyr::select(metodologia,mse_ben,price,accuracy)%>%
  gather(metric,value,-metodologia)


orden=all%>%
  filter(metric=='mse_ben')%>%
  group_by(metodologia)%>%
  summarise(mediana=median(value))%>%
  ungroup()%>%
  arrange(mediana)%>%
  dplyr::select(metodologia)%>%
  pull()

name=paste0('images/','sim_res_k',k,'n',noise,'.jpg')

all%>%
  mutate(metodologia=factor(metodologia,levels = orden),
         metric=factor(metric,levels = c('mse_ben','price','accuracy')),
         methodology=metodologia
  )%>%
  ggplot(aes(methodology,value,fill=methodology))+
  geom_boxplot()+
  facet_wrap(~metric,scales = 'free')+
  ggthemes::theme_few()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  ggsave(name,width = 10, height = 5)


name2=paste0('images/','total_sim_res_k',k,'n',noise,'.jpg')

oracle2=resultados_org%>%
  mutate(price=18200/n)%>%
  filter(metodologia=='Oracle')%>%
  dplyr::select(mse,price,seed)%>%
  rename(mse_oracle=mse,price_oracle=price)


all2=no_oracle%>%
  inner_join(oracle2,by='seed')%>%
  mutate(cost_ben_10000=((10000*sqrt(mse))+price)/((10000*sqrt(mse_oracle))+price_oracle),
         cost_ben_200=((200*sqrt(mse))+price)/((200*sqrt(mse_oracle))+price_oracle),
         cost_ben_100=((100*sqrt(mse))+price)/((100*sqrt(mse_oracle))+price_oracle),
         cost_ben_50=((50*sqrt(mse))+price)/((50*sqrt(mse_oracle))+price_oracle)
  )%>%
  dplyr::select(metodologia,cost_ben_50,cost_ben_100,cost_ben_200,cost_ben_10000)%>%
  gather(metric,value,-metodologia)

all2%>%
  mutate(metodologia=factor(metodologia,levels = orden),
         methodology=metodologia
  )%>%
  ggplot(aes(methodology,value,fill=methodology))+
  geom_boxplot()+
  facet_wrap(~metric,scales = 'free')+
  ggthemes::theme_few()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  ggsave(name2,width = 10, height = 5)