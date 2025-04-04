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
  select(-brks_fit,-brks_fit_summ,-brks_fit_opt) |> 
  mutate(ls = if_else(brks_opt_num > 0,
                      true = map2(.x = ls, .y = breaks.ls, .f = regime.means),
                      false = ls),
         index4 = if_else(brks_opt_num > 0, true = 1, false = 0),
         class = map2_chr(.x = ls, .y = brks_opt_num, .f = classification_scheme))



table(regimeclassification$class)

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


df <- regimeclassification$ls[[3]]



classification_scheme <- function (df, n_regimes) {
  
  
  if(n_regimes > 0) {
  
  decline_mag = df |> 
    ungroup() |> 
    group_by(regime.class) |> 
    summarise(regime.max = mean(regime.max, na.rm =T),
              regime.min = mean(regime.min, na.rm = T)) |> 
    mutate(regime_min_lead = lead(regime.min)) |> 
    ungroup() |> 
    filter(regime.max == max(df$regime.max)) |> 
    mutate(perc_decline = (regime.max-regime_min_lead)/regime.max) |>
    summarise(perc_decline = max(perc_decline,na.rm = T)) |> 
    pull(perc_decline)
  
  n_length <- df |> 
    group_by(regime.class) |> 
    summarise(n = n()) |>
    slice(length(unique(df$regime.class))) |> 
    pull(n) 
  
  n_leads = n_length-1
  
n_sustained <- df |>
    filter(regime.class == unique(df$regime.class)[length(unique(df$regime.class))]) |> 
    ungroup() |> 
    select(y) |> 
    mutate(y = if_else(y < max(df$y)*0.1, true = 1, false = 0)) |> 
    mutate(across(
      everything(),
      .fns = list(!!!setNames(
        lapply(seq_len(n_leads), function(i) ~lead(., i)),
        paste0("lead_", seq_len(n_leads))
      )))) |> 
    mutate(nrow = 1:n()) |> 
    pivot_longer(cols = 1:(n_leads+1)) |> 
    group_by(nrow) |> 
    summarise(n_below = sum(value,na.rm = T)) |> 
  pull(n_below)
 
   
case_when(decline_mag == (-Inf)~"established",
          decline_mag>=0.9 & max(n_sustained)>3&length(unique(df$regime.class))==3 ~ "boom & bust",
          decline_mag>=0.9 & !(max(n_sustained)>3)&length(unique(df$regime.class))==3 ~ "boom & bust not sust.",
          decline_mag>=0.9 & max(n_sustained)>3&length(unique(df$regime.class))==2 ~ "boom unk rate & bust",
          decline_mag>=0.9 & !(max(n_sustained)>3)&length(unique(df$regime.class))==2~ "boom unk rate & bust not sust.",
          decline_mag<0.9~"overshoot")

  } else {
  
NA_character_
    
    
  }
}
