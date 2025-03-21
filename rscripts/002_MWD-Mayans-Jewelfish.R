###This script is dedicated to summarising our lab's data for African Jewelfish and 
###Mayan Cichlids in order to use them in the meta-analysis. These fish were chosen
###rather than all the non-natives because their populations have been described in the
###literature as having boom-busts

#remove everything in environment


rm(list = ls())

#libraries

library(tidyverse)
library(purrr)
library(here)
library(magrittr)
library(readxl)

theme_set(theme_bw())

#-----------------------------------------
#### Jewelfish ####
#-----------------------------------------



jewelfish <- read_csv(file = here("data","fish_count_1996_2024_p3.csv")) |>  #load in the data
  select(REGION,SITE,PLOT,SEASON,DATE,THROW,CUM,YEAR,PERIOD,MONTH,DAY,HEMLET) |> # only get the needed variables HEMLET data
  ##calculate densities for each period at each regional
  group_by(YEAR,REGION, PERIOD) |> 
  summarise(n = n(),
            count = sum(HEMLET, na.rm = T),
            MONTH = mean(MONTH, na.rm = T),
            DAY = mean(DAY, na.rm = T)) |> 
  mutate(MONTH = round(MONTH),
         DAY = round(DAY),
         density = count/n) |> 
  ungroup() |> 
  #Take the maximum of the periods like we did for all the other MON-YEAR time series
  group_by(YEAR,REGION) |> 
  summarise(density = max(density)) |> 
  #create the same variables that we had for the digitized data in order to combine
  mutate(measure = "Density",
         x_variable = "Mon-Year",
         scale = "(n/m^2)",
         plot = "Pintaretal_BiologicalInvasion_2023_jewelfish",
         group = case_when(REGION == "SRS" ~1,
                           REGION == "TSL" ~2,
                           REGION == "WCA"~3,
                           REGION == "PHD"~4)) |> 
  rename(x = YEAR,
         y = density,
         id = REGION) |> 
  filter(group != 4) |> 
  group_by(plot,group,measure, x_variable) |> 
  nest(.key = "ls")


#-----------------------------------------
#### Mayan Cichlids ####
#-----------------------------------------

mayans <- read_csv(file = here("data","FISHCOUNT_TRANS_ALL.csv")) |>    #load in the data
  select(REGION,SITE,PLOT,TRANSECT,CUM,YEAR,PERIOD,MONTH,DAY,CICURO) |> # only get the needed variables HEMLET data
  ##calculate densities for each period at each regional
  group_by(YEAR,REGION, PERIOD) |> 
  summarise(n = n(),
            count = sum(CICURO, na.rm = T),
            MONTH = mean(MONTH, na.rm = T),
            DAY = mean(DAY, na.rm = T)) |> 
  mutate(MONTH = round(MONTH),
         DAY = round(DAY),
         CPUE = count/n,
         REGION = if_else(REGION == "WCA3A" | REGION == "WCA3B", true = "WCA",false = REGION)) |> 
  ungroup() |> 
  group_by(YEAR,REGION) |> 
  summarise(CPUE = max(CPUE)) |> 
  #create the same variables that we had for the digitized data in order to combine
  mutate(measure = "CPUE",
         x_variable = "Mon-Year",
         scale = "(n/5mintransect)",
         plot = "Pintaretal_BiologicalInvasion_2023_mayans",
         group = case_when(REGION == "SRS" ~1,
                           REGION == "TSL" ~2,
                           REGION == "WCA"~3,
                           REGION == "PHD"~4,
                           REGION == "WCA2A"~5,
                           REGION == "LOX"~6)) |> 
  rename(x = YEAR,
         y = CPUE,
         id = REGION) |> 
  filter(group %in% c(1,2,3)) |> 
  group_by(plot,group,measure, x_variable) |> 
  nest(.key = "ls")


#Combine them and save


MDW <- jewelfish |> 
  bind_rows(mayans)

save(MDW,file = here("output","MDW_mayan-jewelfish.Rdata"))




