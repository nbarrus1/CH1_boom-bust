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

load(here("output","final_set.Rdata"))
load(here("output","all_data.Rdata"))



###specify functions


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
    
    
    case_when(decline_mag == (-Inf)~"\nestablished",
              decline_mag>=0.9 & max(n_sustained)>3&length(unique(df$regime.class))==3 ~ "boom &\nbust",
              decline_mag>=0.9 & !(max(n_sustained)>3)&length(unique(df$regime.class))==3 ~ "boom &\nnot sust.",
              decline_mag>=0.9 & max(n_sustained)>3&length(unique(df$regime.class))==2 ~ "unk rate &\nbust",
              decline_mag>=0.9 & !(max(n_sustained)>3)&length(unique(df$regime.class))==2~ "unk rate &\nnot sust.",
              decline_mag<0.9~"\novershoot")
    
  } else {
    
    NA_character_
    
    
  }
}



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






#### Flow Chart ####


all_data_summ <- all_data_summ |>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.full==1, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group"))


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
  fc_split(class, text_pattern = "{n} {label}") |> 
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
