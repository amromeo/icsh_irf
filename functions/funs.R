mean.pred.intervals <- function(x, y, pred.x) {
  mydf<-data.frame(x,y)
  mydf<-na.omit(mydf)
  x<-mydf[,1]
  y<-mydf[,2]
  n <- length(y) # Find sample size
  lm.model <- mcreg(log(x),(y), method.reg = "PaBa",na.rm = TRUE) # Fit linear model

  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm.model@glob.coef[1]
  b1 <- lm.model@glob.coef[2]

  y.fitted <- (b1*log(x))+b0 # Extract the fitted values of y

  pred.y <- b1 * log(pred.x) + b0 # Predict y at the given value of x (argument pred.x)

  # Find SSE and MSE
  sse <- sum((y - y.fitted)^2, na.rm = TRUE)
  mse <- sse / (n - 2)

  t.val <- qt(0.975, n - 2) # Critical value of t

  mean.se.fit <- (1 / n + (pred.x - mean(x,na.rm = T))^2 / (sum((x - mean(x,na.rm = T))^2,na.rm = T))) # Standard error of the mean estimate
  pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x,na.rm = T))^2 / (sum((x - mean(x,na.rm = T))^2,na.rm = T))) # Standard error of the prediction

  # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
  mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
  mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)

  # Prediction Upper and Lower Confidence limits at 95% Confidence
  pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se.fit)
  pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se.fit)

  # Beta 1 Upper and Lower Confidence limits at 95% Confidence
  b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
  b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))

  # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
  upper <- data.frame(rbind(round(mean.conf.upper, 2), round(pred.conf.upper, 2), round(b1.conf.upper, 2)))
  lower <- data.frame(rbind(round(mean.conf.lower, 2), round(pred.conf.lower, 2), round(b1.conf.lower, 2)))
  fit <- data.frame(rbind(round(pred.y, 2), round(pred.y, 2), round(b1, 2)))

  # Collect all into data.frame and rename columns
  results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction', 'Coefficient'))
  colnames(results) <- c('Lower', 'Upper', 'Fit')

  return(results)
}

load_libs<-function(){
libloc<-"~/icsh_irf/library"
library(knitr)
opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
               echo=FALSE, warning=FALSE, message=FALSE)
library(purrr,lib.loc=libloc)
library(tidyr,lib.loc=libloc)
library(stringr,lib.loc=libloc)
library(ggplot2,lib.loc=libloc)
library(dplyr,lib.loc=libloc)
library(here,lib.loc=libloc)
library(janitor,lib.loc=libloc)
library(mcr,lib.loc=libloc)
library(readxl,lib.loc=libloc)
library(cowplot,lib.loc=libloc)
library(fcuk,lib.loc=libloc)
library(GGally,lib.loc=libloc)
library(reshape2,lib.loc=libloc)
library(gridGraphics,lib.loc=libloc)
}


firstgg<-function(x,y){
  df<-data_frame(x=x,y=y)

  mylims<-max(c(x,y))

  ggplot(mydf,aes(x,y))+
    geom_point()+
    geom_smooth()+
    geom_smooth(method = "lm",se=FALSE,color="green")+
    geom_abline(intercept = 1,slope = 1,linetype=2)+
    xlim(0,mylims)+
    ylim(0,mylims)
}


mydems<- function(x,y) {

  pbreg<-mcreg(x,y, method.reg = "PaBa",na.rm = TRUE)

  b0<-pbreg@glob.coef[1] %>% round(3)
  b1<-pbreg@glob.coef[2] %>% round(3)
  estim<- pbreg@para

  LCI.I<-estim[1,3] %>% round(3)
  UCI.I<-estim[1,4] %>% round(3)
  LCI.S<-estim[2,3] %>% round(3)
  UCI.S<-estim[2,4] %>% round(3)

  pcro<-cor(x,y,use = "complete.obs",method = "pearson") %>% round(3)

  list(model=pbreg,b0=b0,b1=b1,LCI.I=LCI.I,UCI.I=UCI.I,LCI.S=LCI.S,UCI.S=UCI.S,pcro=pcro)
}

biaser<-function(x,y){

  df<-data_frame(x,y)
  colnames(df)<-c("x","y")
  df<-df %>%
    rowwise() %>%
    mutate(avg = sum(x,y)/2,
           delta = x - y,
           delta_p = delta/avg)

  avg_delta<-mean(df$delta,na.rm = T)
  avg_delta_p<-mean(df$delta_p,na.rm = T)

  p1<-ggplot(df,aes(avg,delta))+
    geom_point()+
    geom_hline(yintercept = avg_delta)+
    geom_hline(yintercept = 0, linetype=2)+
    labs(x="Average IRF",
         y="DxH - XN")

  p2<-ggplot(df,aes(avg,delta_p))+
    geom_point()+
    geom_hline(yintercept = avg_delta_p)+
    geom_hline(yintercept = 0, linetype=2)+
    labs(x="Average IRF",
         y="Percent difference")
  list(p1,p2)

}
