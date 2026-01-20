rm(list = ls())

####libraries####

library(here)
library(tidyverse)
library(patchwork)

###data###

load(here("output","all_data.Rdata"))
load(here("output","regimeclassification.Rdata"))




all_data <- all_data_summ

####

all_data_summ <- all_data|>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.10yrs >= 0.75, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group")) 




