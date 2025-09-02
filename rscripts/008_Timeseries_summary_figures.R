rm(list = ls())

##libraries

library(tidyverse)
library(patchwork)
library(here)
library(flowchart)
library(sf)
library(forcats)

#### Data

load(here("output","all_data.Rdata"))
load(here("output","regimeclassification.Rdata"))

##global map for visualizations

worldmap <- st_read(here("data/shapefile/WorldMap_Continents/"))

theme_set(theme_bw())

all_data <- all_data_summ
#---------------------------------------
#### Flow Chart ####
#---------------------------------------

all_data_summ <- all_data|>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.10yrs >= 0.75, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group")) |> 
  mutate(class.index1 = case_when(is.na(class)~NA_character_,
                                  class == "\novershoot"~"\novershoot\n",
                                  class == "\nestablished"~"\nestablished\n ",
                                  .default = ">90% decline"),
         class.index2 = case_when(class == "boom &\nbust"~"fast rate",
                                 class =="boom &\nnot sust."~"fast rate",
                                 class =="unk rate &\nbust"~"fast rate",
                                 class =="unk rate &\nnot sust."~"fast rate",
                                 class =="slow rate &\nbust"~"slow rate",
                                 class =="slow rate &\nnot sust."~"slow rate",
                                 class =="\ncrash"~"slow rate",
                                 .default = NA_character_
                                 ),
         class.index3 = case_when(class == "boom &\nbust"~"boom &\nbust",
                                  class =="boom &\nnot sust."~"boom &\nbust sust. unk.",
                                  class =="unk rate &\nbust"~"unk rate &\nbust",
                                  class =="unk rate &\nnot sust."~"unk rate &\nbust sust. unk",
                                  class =="slow rate &\nbust"~"slow rate",
                                  class =="slow rate &\nnot sust."~"slow rate",
                                  class =="\ncrash"~"slow rate",
                                  .default = NA_character_
         ),
         class.forfigure = case_when(class == "boom &\nbust"~"boom &\nbust",
                                  class =="boom &\nnot sust."~"boom &\nbust sust. unk.",
                                  class =="unk rate &\nbust"~"unk rate &\nbust",
                                  class =="unk rate &\nnot sust."~"unk rate &\nbust sust. unk",
                                  class =="slow rate &\nbust"~"slow rate",
                                  class =="slow rate &\nnot sust."~"slow rate",
                                  class =="\ncrash"~"slow rate",
                                  class =="\nestablished" ~"\nestablished",
                                  class =="\novershoot"~"\novershoot",
                                  .default = NA_character_
         ))
 

index1.label <- "years surveyed < 10 years or \n < 8 years if longevity < 1"
index2.label <- "10 consecutive years < 75 % complete"
index3.label <- "harvest"


sum(all_data_summ[["index1"]]==0)
label_exc <- paste(
  c(str_glue("{sum(all_data_summ$index1 == 0 | all_data_summ$index2 == 0 | all_data_summ$index3 == 0, na.rm = T)} excluded:"),
    str_glue("- {sum(all_data_summ$index1 == 0, na.rm = TRUE)}: {index1.label}"),
    str_glue("- {sum(all_data_summ$index2 == 0, na.rm = TRUE)}: {index2.label}"),
    
    str_glue("- {sum(all_data_summ$index3 == 0, na.rm = TRUE)}: {index3.label}")),
  collapse = "\n")

all_data_summ |> 
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
  fc_filter(!is.na(class),
            label = "regime shifts detected",
            text_pattern = "{n} {label}",
            show_exc = TRUE,
            label_exc = "no regime\nshifts detected",
            text_pattern_exc = "{n} {label}",
            just_exc = "left",
            offset_exc = -0.1,
            direction_exc = "left") |> 
  fc_split(class.index1, text_pattern = "{n} {label}") |> 
  fc_split(class.index2, text_pattern = "{n} {label}") |> 
  fc_split(class.index3, text_pattern = "{n} {label}") |> 
  fc_draw() |> 
  fc_export(filename = here("output/figure_editing","flowchart.pdf"))


#----------------------------------------
  ####taxonomic and geogrpahic summaries###
#------------------------------------------

  
  ###continent_maps###
  
p1 <-  all_data_summ |> 
    rename(CONTINENT = continent) |> 
    mutate(CONTINENT = case_when(CONTINENT == "Island" ~ "Oceania",
                                 CONTINENT == "Antartica"~"Antarctica",
                                 .default = CONTINENT)) |> 
  filter(class.index1 == ">90% decline") |> 
  group_by(CONTINENT) |> 
    summarise(n_timeseries = n()) |> 
    #group_by(CONTINENT) |> 
    #summarise(n_spp = n()) |> 
    right_join(worldmap |>
                 mutate(CONTINENT = case_when(CONTINENT == "South America"~"S. America",
                                              CONTINENT == "North America"~"N. America",
                                              .default = CONTINENT)), by = "CONTINENT",
               keep = TRUE) |> 
    ggplot(aes(geometry = geometry))+
    geom_sf(aes(fill = n_timeseries), color = "#50164aff")+
    theme(axis.text = element_blank())+
    scale_fill_gradient2(low = "#50164a80", mid = "#50164abf", high = "#50164aff",
                         na.value = "white")+
    labs(fill = "Count")
  
 
  
p2 <-  all_data_summ |> 
  filter(class.index1 == ">90% decline") |> 
  group_by(kingdom,major.group,ecosystem, species.names) |> 
    summarise(n_timeseries = n()) |> 
    ggplot(aes(x = fct_reorder(species.names, n_timeseries, median),
               y = n_timeseries, fill = ecosystem)) + 
    geom_segment(aes(xend = species.names), yend = 0, color = "#666666") + 
    geom_point(size = 2, color = "black", shape = 21)+
    scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
    coord_flip()+
    labs(y = "Count", x = "Species")+
    theme(legend.position = c(.6,.2),
          legend.title = element_blank(),
          axis.text.y = element_text(face = "italic"),
          legend.text = element_text(size = 8),
          legend.background = element_blank(),
          legend.key = element_blank())
  

p3 <-  all_data_summ |> 
  filter(class.index1 == ">90% decline") |> 
  group_by(kingdom,major.group) |> 
    summarise(n_timeseries = n()) |> 
    ggplot(aes(x = fct_reorder(major.group, n_timeseries, median),
               y = n_timeseries, fill = kingdom)) + 
    geom_segment(aes(xend = major.group), yend = 0, color = "#666666") + 
    geom_point(size = 2, color = "black", shape = 21)+
    scale_fill_manual(values = c("#50164aff","#50164a80"))+
    coord_flip()+
    labs(y = "Count", x = "Order")+
    theme(legend.position = c(.6,.25),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.background = element_blank(),
          legend.key = element_blank()
    )
  
p4 <-  all_data_summ |> 
    filter(class.index1 == ">90% decline") |> 
    group_by(ecosystem) |> 
    summarise(n_timeseries = n()) |> 
    ggplot(aes(x = fct_reorder(ecosystem, n_timeseries, median),
               y = n_timeseries)) + 
    geom_segment(aes(xend = ecosystem), yend = 0, color = "#666666") + 
    geom_point(size = 2, color = "black", shape = 21,
               fill = "#50164aff", show.legend = F)+
    scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60))+
    coord_flip()+
    labs(y = "Count", x = "System")
  
p5 <-  all_data_summ |> 
  filter(class.index1 == ">90% decline") |> 
  group_by(kingdom) |> 
    summarise(n_timeseries = n()) |> 
    ggplot(aes(x = fct_reorder(kingdom, n_timeseries, median),
               y = n_timeseries)) + 
    geom_segment(aes(xend = kingdom), yend = 0, color = "#666666") + 
    geom_point(size = 2, color = "black", shape = 21, 
               fill = "#50164aff", show.legend = F)+
    coord_flip()+
    labs(y = "Count", x = "Kingdom") 

orders <- tibble(longevity.order = c("< 1","> 30","1-2",
                                     paste0(seq(2,28, by = 2),"-",seq(4,30, by = 2))),
                 n_timeseries = rep(0, times = length(c("< 1","> 30","1-2",
                                                        paste0(seq(2,28, by = 2),"-",seq(4,30, by = 2))))))


p6 <- all_data_summ |> 
  filter(class.index1 == ">90% decline") |> 
  mutate(longevity.order= case_when(longevity.yrs < 1 ~ "< 1",
                                     longevity.yrs >= 1 & longevity.yrs < 2~ "1-2",
                                     longevity.yrs >= 2 & longevity.yrs < 4~ "2-4",
                                     longevity.yrs >= 4 & longevity.yrs < 6~ "4-6",
                                     longevity.yrs >= 6 & longevity.yrs < 8~ "6-8",
                                     longevity.yrs >= 8 & longevity.yrs < 10~ "8-10",
                                     longevity.yrs >= 10 & longevity.yrs < 12~ "10-12",
                                     longevity.yrs >= 12 & longevity.yrs < 14~ "12-14",
                                     longevity.yrs >= 14 & longevity.yrs < 16~ "14-16",
                                     longevity.yrs >= 16 & longevity.yrs < 18~ "16-18",
                                    longevity.yrs >= 18 & longevity.yrs < 20~ "18-20",
                                    longevity.yrs >= 20 & longevity.yrs < 22~ "20-22",
                                    longevity.yrs >= 22 & longevity.yrs < 24~ "22-24",
                                    longevity.yrs >= 24 & longevity.yrs < 26~ "24-26",
                                    longevity.yrs >= 26 & longevity.yrs < 28~ "26-28",
                                    longevity.yrs >= 28 & longevity.yrs < 30~ "28-30",
                                    longevity.yrs >= 30~ "> 30")) |> 
  group_by(longevity.order) |> 
  summarise(n_timeseries = n(),.groups = "drop")


p6.complete <- p6 |> 
  bind_rows(orders |> filter( !(longevity.order %in% (unique(p6$longevity.order))))) |> 
  mutate(longevity.order = factor(longevity.order, levels = c("< 1","1-2", paste0(seq(2,28,by = 2),"-",seq(4,30,by = 2)),"> 30"),
                                  ordered = TRUE),
         total.timeseries = sum(n_timeseries),
         prop.timeseries = n_timeseries/total.timeseries) |> 
  ggplot(aes(x = longevity.order, y = n_timeseries))+
  geom_segment(aes(xend = longevity.order), yend = 0, color = "#666666") + 
    geom_point(size = 2, color = "black", shape = 21, 
             fill = "#50164aff", show.legend = F)+
  coord_flip()+
  labs(y = "Count", x = "Longevity (years)")+
  theme(axis.text.y = element_text(size = 7))
  


layout <- "
AAAAAAAA#
AAAAAAAA#
AAAAAAAA#
AAAAAAAA#
AAAAAAAA#
#BBBBBBCC
#BBBBBBCC
#BBBBBBDD
#BBBBBBDD
#BBBBBBEE
#BBBBBBEE
"

fig3_characteristics <- p1+p2+p3+p4+p6.complete+plot_layout(design = layout)+
  plot_annotation(tag_levels = "A", tag_suffix = ")")  

ggsave(filename = here("output/figure_editing","fig3_taxonomygeography.pdf"),
       plot = fig3_characteristics, device = "pdf", units = "mm",
       width = 173, height = (173+((3/5)*173)))



#--------------------------------------
###propotions through categories###
#--------------------------------------


class_names <- levels(as.factor(all_data_summ$class.forfigure))
my_pallette <- c("#d45500ff","#a02c2cff","#50164aff","#501448e5","#50164ab2","#50164acd")
class_pallette <- setNames(my_pallette, nm = class_names)


p7 <- all_data_summ |> 
  ggplot(aes(x = forcats::fct_infreq(major.group)))+
  geom_bar(aes(fill = class.forfigure))+
  scale_fill_manual(values = class_pallette, na.value = "#666666",
                    labels = class_names)+
  coord_flip()+
  labs(x = "Order")+
  theme(legend.position = c(.8,.75),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
  )

p8 <-all_data_summ |> 
  ggplot(aes(x = forcats::fct_infreq(ecosystem)))+
  geom_bar(aes(fill = class.forfigure))+
  scale_fill_manual(values = class_pallette, na.value = "#666666",
                    labels = class_names)+
  coord_flip()+
  labs(x = "System")+
  theme(legend.position = c(.8,.75),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
  )

p9 <-all_data_summ |> 
  ggplot(aes(x = forcats::fct_infreq(continent)))+
  geom_bar(aes(fill = class.forfigure))+
  scale_fill_manual(values = class_pallette, na.value = "#666666",
                    labels = class_names)+
  coord_flip()+
  labs(x = "Continent")+
  theme(legend.position = c(.8,.75),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
  )


fig_4.propotions <- p7+p8+p9+
  plot_annotation(tag_levels = "A", tag_suffix = ")",
                  theme = theme(plot.title = element_text(size = 16)))+
  plot_layout(guides = "collect")&theme(legend.position = "bottom")


ggsave(filename = here("output/figure_editing","fig4_propotions.pdf"),
       plot = fig_4.propotions, device = "pdf", units = "mm",
       width = 385, height = (173*0.75))

ggsave(filename = here("output/figure_editing","fig4_propotions.png"),
       plot = fig_4.propotions, device = "png", units = "mm",
       width = 385, height = (173*0.75))
