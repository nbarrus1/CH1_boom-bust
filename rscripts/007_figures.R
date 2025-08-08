rm(list = ls())

##libraries

library(tidyverse)
library(patchwork)
library(here)
library(flowchart)

#### Data

load(here("output","all_data.Rdata"))
load(here("output","regimeclassification.Rdata"))


#---------------------------------------
#### Flow Chart ####
#---------------------------------------

all_data_summ <- all_data_summ |>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.10yrs >= 0.75, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group")) |> 
  mutate(class.index1 = case_when(is.na(class)~NA_character_,
                                  class == "\novershoot"~"\novershoot\n ",
                                  class == "\nestablished"~"\nestablished\n ",
                                  .default = "some \nevidence for\nboom & bust"),
         class.index2 = case_when(class == "boom &\nbust"~"boom &\nbust",
                                 class =="boom &\nnot sust."~"boom &\nbust sust. unk.",
                                 class =="unk rate &\nbust"~"unk rate &\nbust",
                                 class =="unk rate &\nnot sust."~"unk rate &\nbust sust. unk",
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
  fc_draw() |> 
  fc_export(filename = here("output/figure_editing","flowchart.pdf"))


load(here("output","final_plots.Rdata"))



plot.select <- regimeclassification$plot[regimeclassification$class=="\nestablished"][[1]]
group.select <- regimeclassification$group[regimeclassification$class=="\nestablished"][[1]]


p1 <-final.plots$timeseries[final.plots$plot == plot.select & final.plots$group == group.select][[1]]


p1+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1981, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","established_example.png"), device = "png",
       units = "in", width = 6, height = 4)


plot.select <- regimeclassification$plot[regimeclassification$class=="\novershoot"][[12]]
group.select <- regimeclassification$group[regimeclassification$class=="\novershoot"][[12]]


p2 <-final.plots$timeseries[final.plots$plot == plot.select&final.plots$group == group.select][[1]]


p2+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 13, xmax = Inf, fill = "#666666", alpha = 0.2)+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 29, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","overshoot_example.png"), device = "png",
       units = "in", width = 6, height = 4)



plot.select <- regimeclassification$plot[regimeclassification$class=="boom &\nbust"][[45]]
group.select <- regimeclassification$group[regimeclassification$class=="boom &\nbust"][[45]]


p3 <-final.plots$timeseries[final.plots$plot == plot.select&final.plots$group == group.select][[1]]


p3+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1987, xmax = Inf, fill = "#666666", alpha = 0.2)+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2001, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","boom-bust_example.png"), device = "png",
       units = "in", width = 6, height = 4)



plot.select <- regimeclassification$plot[regimeclassification$class=="boom &\nnot sust."][[2]]
group.select <- regimeclassification$group[regimeclassification$class=="boom &\nnot sust."][[2]]


p4 <-final.plots$timeseries[final.plots$plot == plot.select&final.plots$group == group.select][[1]]


p4+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1987, xmax = Inf, fill = "#666666", alpha = 0.2)+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 2001, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","boom-bustnotsust_example.png"), device = "png",
       units = "in", width = 6, height = 4)



plot.select <- regimeclassification$plot[regimeclassification$class=="unk rate &\nbust"][[4]]
group.select <- regimeclassification$group[regimeclassification$class=="unk rate &\nbust"][[4]]


p5 <-final.plots$timeseries[final.plots$plot == plot.select&final.plots$group == group.select][[1]]


p5+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Inf, fill = "#666666", alpha = 0.2)+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1981, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","unkratebust_example.png"), device = "png",
       units = "in", width = 6, height = 4)



plot.select <- regimeclassification$plot[regimeclassification$class=="unk rate &\nnot sust."][[12]]
group.select <- regimeclassification$group[regimeclassification$class=="unk rate &\nnot sust."][[12]]


p6 <-final.plots$timeseries[final.plots$plot == plot.select&final.plots$group == group.select][[1]]


p6+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = -Inf, xmax = Inf, fill = "#666666", alpha = 0.2)+
  annotate("rect", ymin = -Inf, ymax = Inf, xmin = 1998, xmax = Inf, fill = "#666666", alpha = 0.2)+
  theme(title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))

ggsave(plot = last_plot(), filename = here("output","unkratenotsust_example.png"), device = "png",
       units = "in", width = 6, height = 4)




temp <- regimeclassification |> 
  select(plot, author, class)

