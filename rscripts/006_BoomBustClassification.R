#### this script is for classifying the time series based off of the regimes deteted
####in step 4

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)
library(flowchart)


theme_set(theme_bw())

##load in the final set of time series for meta-analysis

load(file = here("output","final_set_brks.Rdata"))
load(here("output","all_data.Rdata"))



###specify functions


regime.means <- function(df.original, df.breaks) {
  
  if(nrow(df.breaks)==1) {
    
    df.original |> 
      mutate(regime.class = if_else(x_pred <= df.breaks$x_pred[1], true = "reg1", false = "reg2"),
             rate = log(lead(pred)/pred)) |> 
      group_by(regime.class) |> 
      mutate(regime.ave = mean(pred, na.rm = T),
             regime.max = max(pred, na.rm = T),
             regime.min = min(pred, na.rm = T),
             regime.rate.max = max(rate, na.rm = T),
             regime.rate.min = min(rate, na.rm = T))
    
  } else if(nrow(df.breaks)==2) {
    
    df.original |> 
      mutate(regime.class = case_when(x_pred <=df.breaks$x_pred[1] ~ "reg1",
                                      x_pred > df.breaks$x_pred[1] & x_pred <= df.breaks$x_pred[2]~"reg2",
                                      x_pred > df.breaks$x_pred[2]~"reg3",
                                      .default = NA_character_),
             rate = log(lead(pred)/pred)) |> 
      group_by(regime.class) |> 
      mutate(regime.ave = mean(pred, na.rm = T),
             regime.max = max(pred, na.rm = T),
             regime.min = min(pred, na.rm = T),
             regime.rate.max = max(rate, na.rm = T),
             regime.rate.min = min(rate, na.rm = T))
  }
  
}



df <- regimeclassification$predictions[[55]]
n_regimes <- regimeclassification$brks_opt_num[[55]]

classification_scheme <- function (df, n_regimes) {
  
  
  if(n_regimes > 0) {
  
##calculate the maximum percent decline  
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
    
##calculate the maximum rate of increase between the lag/growth and high n phase    
     rate_increase <- df |> 
       ungroup() |> 
       mutate(lambda = lead(pred)/pred) |> 
       group_by(regime.class) |> 
       summarise(lambda.max = max(lambda, na.rm = T),
                 lambda.min = min(lambda, na.rm = T),
                 regime.max = mean(regime.max, na.rm =T),
                 regime.min = mean(regime.min, na.rm = T)) |> 
       ungroup() |> 
       filter(lead(regime.max) > regime.max) |> 
       pull(lambda.max)
     
     rate_increase <- if(is_empty(rate_increase)){NA_real_}else{max(rate_increase)}
     
##calculate the minimum rate of increase between the high n and low n phase      
     rate_decline <- df |> 
       ungroup() |> 
       mutate(lambda = lead(pred)/pred) |> 
       group_by(regime.class) |> 
       summarise(lambda.max = max(lambda, na.rm = T),
                 lambda.min = min(lambda, na.rm = T),
                 regime.max = mean(regime.max, na.rm =T),
                 regime.min = mean(regime.min, na.rm = T)) |> 
       ungroup() |> 
       filter(lag(regime.max) > regime.max) |> 
       pull(lambda.min)
     
     rate_decline <- if(is_empty(rate_decline)){NA_real_}else{min(rate_decline)}
##find the number of observation within the low n phase
    n_length <- df |> 
      group_by(regime.class) |> 
      summarise(n = n()) |>
      slice(length(unique(df$regime.class))) |> 
      pull(n) 
    
##find the number of leads needed for the sustained calculation    
    n_leads = n_length-1

##calculate the number of consecutive values across the sequence of the low n phase
##that are  >= 90% smaller than the maximum
   n_sustained <- max(df |>
      filter(regime.class == unique(df$regime.class)[length(unique(df$regime.class))]) |> 
      ungroup() |> 
      select(pred) |> 
      mutate(pred = if_else(pred <= max(df$pred)*0.1, true = 1, false = 0)) |> 
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
      pull(n_below))

   
##determine if the last value of the sequence in the low n phase is >= 90% smaller than
## the max value

decline_lastposition <- df|> 
  filter(regime.class == unique(df$regime.class)[length(unique(df$regime.class))]) |> 
  ungroup() |> 
  select(pred) |> 
  mutate(if.declined = if_else(pred <= max(df$pred)*0.1, true = 1, false = 0)) |> 
  slice(n_length) |> 
  pull(if.declined)


###perform the classification using our definition    
    case_when(decline_mag == (-Inf)~"\nestablished",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_increase >= 2&
                rate_decline <= 0.5&
                length(unique(df$regime.class))==3 ~ "boom &\nbust",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase >= 2&
                rate_decline <= 0.5&
                decline_lastposition > 0 &
                length(unique(df$regime.class))==3 ~ "boom &\nnot sust.",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_decline <= 0.5&
                length(unique(df$regime.class))>=2 ~ "unk rate &\nbust",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_decline <= 0.5&
                decline_lastposition > 0 &
                length(unique(df$regime.class))>=2~ "unk rate &\nnot sust.",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase >= 2&
                rate_decline <= 0.5&
                !(decline_lastposition > 0) &
                length(unique(df$regime.class))==3 ~ "\novershoot",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_decline <= 0.5&
                !(decline_lastposition) > 0 &
                length(unique(df$regime.class))>=2~ "\novershoot",
              decline_mag<0.9~"\novershoot")
    
  } else {
    
    NA_character_
    
    
  }
}



#------------------------------------------------------------------
####create the function for dynamic modelling of the break-points###
#------------------------------------------------------------------

regimeclassification <- final.set.preds.brks |> 
  select(-brks_fit,-brks_fit_summ,-brks_fit_opt) |> 
  mutate(predictions = if_else(brks_opt_num > 0,
                      true = map2(.x = predictions, .y = breaks.preds, .f = regime.means),
                      false = ls),
         index4 = if_else(brks_opt_num > 0, true = 1, false = 0),
         class = map2_chr(.x = predictions, .y = brks_opt_num, .f = classification_scheme))


save(regimeclassification, file = here("output","regimeclassification.Rdata"))


table(regimeclassification$brks_opt_num>0,
      is.na(regimeclassification$class))


###reading list

reading.list <- regimeclassification |> 
  filter(class == "boom &\nbust") |> 
  ungroup() |> 
  select(title, author,species.names,class)
