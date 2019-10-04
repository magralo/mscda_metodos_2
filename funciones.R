
get_X_y_oracle=function(k,sigma,n=200,signal2noise=0.43){
  library(mvtnorm) 
  X_sim=rmvnorm(n, rep(0,ncol(sigma)),sigma)
  p=ncol(X_sim)
  n=nrow(X_sim)
  var_imp=sample(p,k)
  beta=rep(0,p)
  beta[var_imp]=rnorm(k)
  alpha=rnorm(1)
  y=alpha+X_sim%*%beta
  signal_var=var(y)
  noise_var=signal_var/signal2noise
  y=y+rnorm(n,sd=sqrt(noise_var))
  return(list(y=y,X=X_sim,oracle=beta!=0,beta_oracle=beta,alpha_oracle=alpha,sd_oracle=sqrt(noise_var)))
}


get_new_X_y=function(alpha,beta,sigma,n,sd_var){
  library(mvtnorm) 
  X_sim=rmvnorm(n, rep(0,ncol(sigma)),sigma)
  y=alpha+X_sim%*%beta
  y=y+rnorm(n,sd=sd_var)
  return(list(y=y,X=X_sim))
}


get_mse=function(X,y,new_X,new_y,test_X,test_y,n,sel){
  if (sum(sel)>0){
    if(sum(sel)>1){
      new_X=head(new_X[,sel],n)
      all_X=rbind(X[,sel],new_X)
    }else{
      new_X=head(new_X[,sel],n)
      all_X=c(X[,sel],new_X)
    }
    
    new_y=head(new_y,n)
    all_y=c(y,new_y)
    model=lm(all_y~all_X )
    if(sum(sel)>1){
      y_final_pred=test_X[,sel]%*%model$coefficients[-1]+model$coefficients[1]
    }else{
      y_final_pred=test_X[,sel]*model$coefficients[-1]+model$coefficients[1]
    }
    mse=mean((test_y-y_final_pred)^2)
  }else{
    y_final_pred=mean(y)
    mse=mean((test_y-y_final_pred)^2)
  }
  return(mse)
}


##LASSO

simple_lasso=function(X,y){#retornamos cuales variables elejimos dado un X y un Y
  library(glmnet)
  cv <- cv.glmnet(X, y, alpha = 1)
  model <- glmnet(X, y, alpha = 1, lambda = cv$lambda.min)
  selected=as.numeric(model$beta)!=0
  return(selected)
}
##ADALASSO

ada_lasso=function(X,y){#retornamos cuales variables elejimos dado un X y un Y
  library(parcor)
  ada.object<-adalasso(X,y,k=10)
  selected=as.numeric(ada.object$coefficients.adalasso)!=0
  return(selected)
}


##BMA

bma.bic=function(X,y){#retornamos cuales variables elejimos dado un X y un Y
  library(BMA)
  bma.object<-bic.glm(X,y,maxCol = 55,glm.family=gaussian())
  selected=bma.object$probne0>50
  return(selected)
}

##BMA non local

bma.nl=function(X,y){#retornamos cuales variables elejimos dado un X y un Y
  #Define priors
  library(mombf)
  priorCoef <- momprior(tau=0.1)#idk how to choose this tau
  priorDelta <- modelbbprior(alpha.p=1,beta.p=1)
  priorVar <- igprior(alpha=.01,lambda=.01)
  NLObject<- modelSelection(y=y, x=X, center=FALSE, scale=FALSE, niter=500, priorCoef=priorCoef, priorDelta=priorDelta, priorVar=priorVar,method='Laplace')
  buenas=NLObject$postMode
  return(as.numeric(buenas)==1)
}
## ADALASSO2
source('adaLasso_wprice.R')
ada_lasso_prop=function(X,y,prices){#retornamos cuales variables elejimos dado un X y un Y
  ada.object<-adalasso2(X,y,prices=prices,k=10)
  selected=as.numeric(ada.object$coefficients.adalasso)!=0
  return(selected)
}


## LASSO2
source('adaLasso_wprice.R')
lasso_prop=function(X,y,prices,costo_error=100){#retornamos cuales variables elejimos dado un X y un Y
  cvfit = cv.glmnet(X, y,penalty.factor = prices,type.measure="mae")
  costo_modelo=c(0,prices)%*%as.matrix(coef(cvfit,s=cvfit$lambda)!=0)
  cv_realfo=cvfit$cvm*costo_error+costo_modelo
  lambda_optimo=cvfit$lambda[which.min(cv_realfo)]
  selected=as.numeric(coef(cvfit, s = lambda_optimo)[-1])!=0
  return(selected)
}


simulacion_experimento=function(seed,k,n,prices,total_budget,sigma,signal2noise){
  library(tidyverse)
  set.seed(seed)
  print(seed)
  simulacion=get_X_y_oracle(k,sigma = sigma,n=n,signal2noise = signal2noise)
  y=simulacion$y
  X=simulacion$X
  sel_lasso=simple_lasso(X,y)
  sel_ada_lasso=ada_lasso(X,y)
  sel_bic=bma.bic(X,y)
  sel_nl=bma.nl(X,y)
  sel_lasso_mod=lasso_prop(X,y,prices)
  sel_ada_mod=ada_lasso_prop(X,y,prices) #ada lasso modificado
  
  price_lasso=sum(prices[sel_lasso])
  n_lasso=floor(total_budget/price_lasso)
  
  price_ada_lasso=sum(prices[sel_ada_lasso])
  n_ada_lasso=floor(total_budget/price_ada_lasso)
  
  price_bic=sum(prices[sel_bic])
  n_bic=floor(total_budget/price_bic)
  
  price_nl=sum(prices[sel_nl])
  n_nl=floor(total_budget/price_nl)
  
  price_ada_mod=sum(prices[sel_ada_mod])
  n_ada_mod=floor(total_budget/price_ada_mod)
  
  price_lasso_mod=sum(prices[sel_lasso_mod])
  n_lasso_mod=floor(total_budget/price_lasso_mod)
  
  
  sel_naive=rep(TRUE,length(prices))
  price_naive=sum(prices[sel_naive])
  n_naive=floor(total_budget/price_naive)
  
  price_oracle=sum(prices[simulacion$oracle])
  n_oracle=floor(total_budget/price_oracle)
  
  nn=c(n_lasso,n_ada_lasso,n_bic,n_nl,n_ada_mod,n_lasso_mod,n_naive,n_oracle)
  
  new_data=get_new_X_y(simulacion$alpha_oracle,
                       simulacion$beta_oracle,
                       sigma = sigma,
                       n=max(nn[which(nn<=total_budget)]),
                       simulacion$sd_oracle)
  
  
  
  test_data=get_new_X_y(simulacion$alpha_oracle,
                        simulacion$beta_oracle,
                        sigma = sigma,
                        n=1000,
                        simulacion$sd_oracle)
  
  
  new_X=new_data$X
  new_y=new_data$y
  
  test_X=test_data$X
  test_y=test_data$y
  
  mse_lasso=get_mse(X,y,new_X,new_y,test_X,test_y,n_lasso,sel_lasso)
  
  mse_ada_lasso=get_mse(X,y,new_X,new_y,test_X,test_y,n_ada_lasso,sel_ada_lasso)
  
  mse_nl=get_mse(X,y,new_X,new_y,test_X,test_y,n_nl,sel_nl)
  
  mse_bic=get_mse(X,y,new_X,new_y,test_X,test_y,n_bic,sel_bic)
  
  mse_ada_mod=get_mse(X,y,new_X,new_y,test_X,test_y,n_ada_mod,sel_ada_mod)
  
  mse_lasso_mod=get_mse(X,y,new_X,new_y,test_X,test_y,n_lasso_mod,sel_lasso_mod)
  
  mse_naive=get_mse(X,y,new_X,new_y,test_X,test_y,n_naive,sel_naive)
  
  mse_oracle=get_mse(X,y,new_X,new_y,test_X,test_y,n_oracle,simulacion$oracle)
  
  acu_lasso=mean(sel_lasso==simulacion$oracle)
  acu_ada_lasso=mean(sel_ada_lasso==simulacion$oracle)
  acu_bic=mean(sel_bic==simulacion$oracle)
  acu_nl=mean(sel_nl==simulacion$oracle)
  acu_ada_mod=mean(sel_ada_mod==simulacion$oracle)
  acu_lasso_mod=mean(sel_lasso_mod==simulacion$oracle)
  acu_naive=mean(sel_naive==simulacion$oracle)
  acu_oracle=1
  
  resultado=data.frame(metodologia=c('Lasso','Ada Lasso','Local prior','Non local','Ada mod','Lasso mod','Naive','Oracle'),
                       mse=c(mse_lasso,mse_ada_lasso,mse_bic,mse_nl,mse_ada_mod,mse_lasso_mod,mse_naive,mse_oracle),
                       accuracy=c(acu_lasso,acu_ada_lasso,acu_bic,acu_nl,acu_ada_mod,acu_lasso_mod,acu_naive,acu_oracle),
                       n=nn
  )%>%
    mutate(seed=seed)
  return(resultado)  
}

