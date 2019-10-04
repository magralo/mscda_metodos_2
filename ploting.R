k=10
noise=0.43
#file=paste0('costo operacion = 0/noise',noise,'/resultados_',k,'.csv')
file=paste0('noise',noise,'/resultados_',k,'.csv')
resultados_org=read.csv(file)

orden=resultados_org%>%
  group_by(metodologia)%>%
  summarise(mediana=median(mse))%>%
  ungroup()%>%
  arrange(mediana)%>%
  dplyr::select(metodologia)%>%
  pull()



no_oracle=resultados_org%>%
  dplyr::select(metodologia,mse,seed)%>%
  filter(metodologia!='Oracle')

oracle=resultados_org%>%
  filter(metodologia=='Oracle')%>%
  dplyr::select(mse,seed)%>%
  rename(mse_oracle=mse)

all=no_oracle%>%
  inner_join(oracle,by='seed')


plotly::ggplotly(all%>%
  mutate(dif=mse-mse_oracle)%>%
  ggplot(aes(x=metodologia,dif,fill=metodologia))+
  geom_boxplot())
  

resultados_org%>%
  mutate(metodologia=factor(metodologia,levels = orden),
         precio=18200/n)%>%
  dplyr::select(metodologia,mse,precio,accuracy)%>%
  gather(metric,value,-metodologia)%>%
  ggplot(aes(x=metodologia,value,fill=metodologia))+
  geom_boxplot()+facet_wrap(~metric,scales = 'free')


