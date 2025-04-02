###this script is dedicated to combining the geographic and trait information to the plots
###and obtaining timeseries length, completeness, and performing some first summaries describing
###the time series

#remove all 

rm(list = ls())



#libraries

library(tidyverse)
library(patchwork)
library(here)
library(flowchart)
library(sf)


theme_set(theme_bw())

##data
load(here("output","literatrure_timeseries.Rdata"))
load(here("output","MDW_mayan-jewelfish.Rdata"))

otherdata <- read_csv(here("data","BoomBust_Review - TimeSeries_Identification.csv"))

##global map for visualizations

worldmap <- st_read(here("data/shapefile/WorldMap_Continents/"))



####combine data###

all_data <- lit_data_tib |> 
  bind_rows(MDW) |> 
  left_join(otherdata, by = c("plot","group"))


#check for combining errors

all_data |> 
  filter(is.na(title)) |> 
  pull(plot,group)

unique(all_data |> 
  pull(species.names))

all_data |> 
  mutate(time.series.length = map_dbl(.x = ls, .f = nrow),
         time.series.period = map_dbl(.x = ls, .f = function(df) {
           floor(df |> pull(x) |> max())- floor(df |> pull(x) |> min())+1
         }),
         time.series.period.chr = map_chr(.x = ls, .f = function(df) {
           paste0("(",floor(df |> pull(x) |> min()),", ",
                  floor(df |> pull(x) |> max()), ")")
         })) |> 
  select(time.series.length, time.series.period,time.series.period.chr) |> 
  filter(time.series.length != time.series.period) 

####all_data summary

all_data_summ <- all_data |> 
  filter(species.names != "TOTAL") |> 
  filter(native.species != "Y") |> 
  mutate(time.series.length = map_dbl(.x = ls, .f = nrow),
         time.series.period = map_chr(.x = ls, .f = function(df) {
                                                        paste0("(",floor(df |> pull(x) |> min()),", ",
                                                                   floor(df |> pull(x) |> max()), ")")
                                                     }),
         years.surveyed = map_dbl(.x = ls, .f = function(df) {
                                                        df |> drop_na(y) |>
                                                                 mutate(place = 1) |>
                                                                 summarise(place = sum(place)) |> 
                                                                 pull(place)
                                                     }),
         completeness.full = years.surveyed/time.series.length,
         completeness.10yrs = if_else(time.series.length >10, true =  map_dbl(.x = ls, .f = function(df) {
           df |> 
             mutate(temp = if_else(is.na(y),true =0, false = 1),
                    comp10yrs = (temp +lag(temp)+lag(temp,2)+lag(temp,3)+
                                   lag(temp,4)+lag(temp,5)+lag(temp,6)+
                                   lag(temp,7)+lag(temp,8)+lag(temp,9))/
                      10) |> 
             select(-temp) |> 
             ungroup() |> 
             summarise(comp10yrs = max(comp10yrs, na.rm = T)) |> 
             pull(comp10yrs)
         }),false = completeness.full))


final.set <- all_data_summ |> 
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs) |> 
  filter((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7) |> 
  #filter(completeness.full >= 0.75) |> 
  filter(completeness.10yrs >= 0.75) |> 
  filter(measure != "Harvest") 

save(final.set,file = here("output","final_set.Rdata"))
#


#-------------------------------
###visualizations###
#-------------------------------



#### Flow Chart ####


all_data_summ <- all_data_summ |>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.full==1, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0))


index1.label <- "years surveyed < 10 years or \n < 8 years if longevity < 1"
index2.label <- "< 100% Complete"
index3.label <- "harvest"


sum(all_data_summ[["index1"]]==0)
label_exc <- paste(
  c(str_glue("{sum(all_data_summ$index1 == 0 | all_data_summ$index2 == 0 | all_data_summ$index3 == 0, na.rm = T)} excluded:"),
    str_glue("- {sum(all_data_summ$index1 == 0, na.rm = TRUE)}: {index1.label}"),
    str_glue("- {sum(all_data_summ$index2 == 0, na.rm = TRUE)}: {index2.label}"),

        str_glue("- {sum(all_data_summ$index3 == 0, na.rm = TRUE)}: {index3.label}")),
  collapse = "\n")

all_data_summ |> 
  mutate(categories = if_else((index1==1 & index2 == 1 & index3 == 1),
                              true = sample(factor(1:3, labels = c("established","overshoot","boom-bust")), size = 1),
                              false = NA_character_)) |> 
  as_fc(label = "compiled \n timeseries",
        text_pattern = "{N} {label}") |> 
  fc_filter((index1==1 & index2 == 1 & index3 == 1),
            label = "met inclusion \n criteria",
            text_pattern = "{n} {label}",
            show_exc = TRUE,
            label_exc = label_exc,
            text_pattern_exc = "{label}",
            just_exc = "left",
            offset_exc = -0.1,
            direction_exc = "left") |> 
  fc_split(categories, text_pattern = "{n} {label}",
           label = c("established","overshoot","boom-bust")) |> 
  fc_draw() |> 
  fc_export(filename = here("output","flowchart.pdf"))




###continent_maps###

all_data_summ |> 
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs) |> 
  filter((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7) |> 
  filter(completeness.full == 1) |> 
  #filter(completeness.10yrs >= 0.75) |> 
  filter(measure != "Harvest") |>
  rename(CONTINENT = continent) |> 
  mutate(CONTINENT = case_when(CONTINENT == "Island" ~ "Oceania",
                               CONTINENT == "Antartica"~"Antarctica",
                               .default = CONTINENT),
         categories = sample(factor(1:3, labels = c("established","overshoot","boom-bust")), size = 1)) |> 
  filter(categories == "boom-bust") |> 
  #group_by(CONTINENT, species.names) |> 
  #summarise(n_timeseries = n()) |> 
  group_by(CONTINENT) |> 
  summarise(n_spp = n()) |> 
  right_join(worldmap |>
               mutate(CONTINENT = case_when(CONTINENT == "South America"~"S. America",
                                            CONTINENT == "North America"~"N. America",
                                            .default = CONTINENT)), by = "CONTINENT",
             keep = TRUE) |> 
  ggplot(aes(geometry = geometry))+
  geom_sf(aes(fill = n_spp))+
  theme(axis.text = element_blank())


