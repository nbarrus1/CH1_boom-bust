#### this script is for performing the regime shift analysis 

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)

theme_set(theme_bw())

###create the functions designed by Rodionov & Overland, Journal of Marine Science,
###2005

RSI <- function() {}



##load in the final set of time series for meta-analysis

load(here("output","final_set.Rdata"))


test.df <- final.set$ls[[1]] 

l = 5
degrees.freedom = l*2 - 2
p.crit = 0.05
t = qt(p.crit/2, df = degrees.freedom, lower.tail = FALSE)


y.seq = test.df |> 
  ungroup() |> 
  pull(y) 



y.matrix <- matrix(0, nrow = length(y.seq), ncol = l)
colnames(y.matrix) <- paste0("y",0:4)
rownames(y.matrix) <- paste0(test.df$x)

for (i in 1:l) {
  if(i == 1) {
    y.matrix[,i] <-y.seq 
  } else {
  y.matrix[,i] <- lead(y.seq,n = i-1)
  }
}

var.l <- as_tibble(y.matrix) |> 
  mutate(nrow = 1:nrow(y.matrix)) |> 
  pivot_longer(cols = 1:l, names_to = "lead",values_to = "value") |> 
  group_by(nrow) |> 
  summarise(sd = sd(value,na.rm = T)) |> 
  ungroup() |> 
  summarise(sd.l = mean(sd, na.rm = T)) |> 
  mutate(var.l = sd.l^2) |> 
  pull(var.l)

sd.l <- as_tibble(y.matrix) |> 
  mutate(nrow = 1:nrow(y.matrix)) |> 
  pivot_longer(cols = 1:l, names_to = "lead",values_to = "value") |> 
  group_by(nrow) |> 
  summarise(sd = sd(value,na.rm = T)) |> 
  ungroup() |> 
  summarise(sd.l = mean(sd, na.rm = T)) |> 
  mutate(var.l = sd.l^2) |> 
  pull(sd.l)

diff = t*sqrt(2*var.l/l)

x.cur = mean(y.seq[1:l])
crit.upp = x.cur + diff
crit.low = x.cur - diff
RSI = rep(0, times = l)
REGIMESHIFT = data.frame(x = test.df$x,
                        REGIME = rep(NA_character_, times = length(test.df$x)))

for (i in (l+1):length(y.seq)) {
  
#-------------------------------------------------
##first if  if else statement is for determining the
#new value is beyond the crit value range 
#-------------------------------------------------
  
##if yes and is above the upper tail
###calculate RSI
  
  if (y.seq[i] >= crit.upp) {  
    
    RSI[1] <- y.seq[i]-(x.cur+sd.l)
    
   for(j in 2:l) {  
      
      RSI[j] <- (y.seq[i+(j-1)]-(x.cur+sd.l)) + sum(RSI)
        
    }
  
    ##----------------
    ##second if else is used to evaluate the RSI
    ##----------------
    
    #if there were negatives recalculate the x.cur and
    #critical values
    
    if (any(RSI < 0, na.rm = T)) {
      
      REGIMESHIFT$REGIME[i] <- "no"
      
      x.cur = mean(y.seq[(i-l):i],na.rm =T)
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)
      print(i)
      
      #if there were no negative values recalculate the x.cur using the 
      #the new regime
      
    } else { 
      
      REGIMESHIFT$REGIME[i] <- "yes"
      
      x.cur = mean(y.seq[i:(i+l-1)],na.rm =T)
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)  
      
      h = i
      
      print(i)
      
      
    }
##if yes and is below the lower tail
###calculate RSI  
      
    
  } else if (y.seq[i] <= crit.low) {
    
    ##calculate the RSI
    
    RSI[1] <- (-y.seq[i]-l-sd.l)*-1
    
    for(j in 2:l) {
      
      RSI[j] <- (-y.seq[i+(j-1)]-l-sd.l)*-1 + sum (RSI)
      
    }
    ##----------------
    ##second if else is used to evaluate the RSI
    ##----------------
    
    #if there were negatives recalculate the x.cur and
    #critical values
    
    if (any(RSI < 0, na.rm = T)) {
      
      REGIMESHIFT$REGIME[i] <- "no"
      
      x.cur = mean(y.seq[(i-l):i],na.rm =T)
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)
      print(i)
      
    #if there were no negative values recalculate the x.cur using the 
    #the new regime
      
    } else { 
      
      REGIMESHIFT$REGIME[i] <- "yes"  
      
    x.cur = mean(y.seq[i:(i+l-1)],na.rm =T)
    crit.upp = x.cur + diff
    crit.low = x.cur - diff
    
    RSI = rep(0, times = l)  
    
    h = i
    
    print(i) 
    
      }
    
##if no and is below the lower tail
###calculate RSI    
    
    
    
  }else{                         #if the next value doesn not change in critical value
                                 #then lump with previous sequence and update cur mean and critical values
    
    REGIMESHIFT$REGIME[i] <- "no"
    
    if(is.na(REGIMESHIFT$REGIME[(i-1)]) | !any(REGIMESHIFT$REGIME == "yes", na.rm = T)) {
    
      x.cur = mean(y.seq[(i-l):i],na.rm =T)
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      print(i)
      
    } else {
      
      x.cur = mean(y.seq[if((i<=(h+l-1))){(h:(h+l-1))}else{((h+(i-h)):(h+(i-h)+l-1))}],na.rm =T)
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)  
      print(i) 
      
    }
    
  } 
}

