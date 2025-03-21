###this script is dedicated to combining the geographic and trait information to the plots
###and obtaining timeseries length, completeness, and performing some first summaries describing
###the time series

#remove all 

rm(list = ls())


#libraries

library(tidyverse)
library(patchwork)
library(here)


theme_set(theme_bw())

##data
load(here("output","literatrure_timeseries.Rdata"))
load(here("output","MDW_mayan-jewelfish.Rdata"))

otherdata <- read_csv(here("data","BoomBust_Review - TimeSeries_Identification.csv")) 

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



#----------------------------
#summary plots
#------------------------------



all_data_summ |> 
  filter(time.series.length<300) |> 
  #filter(completeness.full<1) |> 
  ggplot(aes(x = completeness.full)) +
  geom_histogram()

all_data_summ |> 
  filter(time.series.length<300) |> 
  #filter(completeness.10yrs<1) |> 
  ggplot(aes(x = completeness.10yrs)) +
  geom_histogram()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = time.series.length)) +
  geom_histogram()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = years.surveyed)) +
  geom_histogram()


all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = longevity.yrs)) +
  geom_histogram()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(y = longevity.yrs, x =time.series.length)) +
  geom_point()



all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = years.surveyed, y = time.series.length)) +
  geom_point()


all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = years.surveyed, y = completeness.full)) +
  geom_point()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = years.surveyed, y = completeness.10yrs)) +
  geom_point()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = time.series.length, y = completeness.full)) +
  geom_point()

all_data_summ |> 
  filter(time.series.length<300) |> 
  ggplot(aes(x = time.series.length, y = completeness.10yrs)) +
  geom_point()





all_data_summ |> 
  filter(time.series.length<300) |> 
  group_by(measure) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(measure, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

all_data_summ |> 
  group_by(kingdom) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(kingdom, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

all_data_summ |> 
  group_by(major.group) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(major.group, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()


all_data_summ |> 
  group_by(species.names) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(species.names, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

all_data_summ |> 
  group_by(continent) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(continent, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

all_data_summ |> 
  group_by(ecosystem) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(ecosystem, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()


##final set




final.set <- all_data_summ |> 
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs) |> 
  filter((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7) |> 
  #filter(completeness.full >= 0.75) |> 
  filter(completeness.10yrs >= 0.75) |> 
  filter(measure != "Harvest") 


save(final.set,file = here("output","final_set.Rdata"))




##plots of final sets


final.set |> 
  filter(time.series.length<300) |> 
  group_by(measure) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(measure, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

final.set |> 
  group_by(kingdom) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(kingdom, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

final.set |> 
  group_by(major.group) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(major.group, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

final.set |> 
  group_by(species.names) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(species.names, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

final.set |> 
  group_by(continent) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(continent, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()

final.set |> 
  group_by(ecosystem) |> 
  summarise(count = n()) |> 
  ggplot(aes(x = reorder(ecosystem, count, median), y = count))+
  geom_bar(stat = "identity")+
  coord_flip()
