#### this script is for classifying the time series based off of the regimes deteted
####in step 4

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)

theme_set(theme_bw())


##load in the final set of time series for meta-analysis

load(here("output","final_set.Rdata"))

#------------------------------------------------------------------
####create the function for dynamic modelling of the break-points###
#------------------------------------------------------------------

regimeclassification <- final.set |> 
  filter(brks_opt_num > 0) |> 
  select(-brks_fit,-brks_fit_summ,-brks_fit_opt)


df.original <- regimeclassification$ls[[1]]
df.breaks <- regimeclassification$breaks.ls[[1]]

regime.means <- function(df.original, df.breaks) {
  
 if(nrow(df.breaks)==1) {
  
  df.original |> 
    mutate(regime.class = if_else(x <= df.breaks$x[1], true = "reg1", false = "reg2"),
           rate = log(lead(y)/y)) |> 
    group_by(regime.class) |> 
     mutate(regime.ave = mean(y, na.rm = T),
            regime.max = max(y, na.rm = T),
            regime.min = min(y, na.rm = T),
            regime.rate.max = max(rate, na.rm = T),
            regime.rate.min = min(rate, na.rm = T))
  
 } else if(nrow(df.breaks)==2) {
   
  df.original |> 
     mutate(regime.class = case_when(x <=df.breaks$x[1] ~ "reg1",
                                     x > df.breaks$x[1] & x <= df.breaks$x[2]~"reg2",
                                     x > df.breaks$x[2]~"reg3",
                                     .default = NA_character_),
            rate = log(lead(y)/y)) |> 
     group_by(regime.class) |> 
     mutate(regime.ave = mean(y, na.rm = T),
            regime.max = max(y, na.rm = T),
            regime.min = min(y, na.rm = T),
            regime.rate.max = max(rate, na.rm = T),
            regime.rate.min = min(rate, na.rm = T))
 }
  
}