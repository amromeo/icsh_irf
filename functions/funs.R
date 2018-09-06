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
}
