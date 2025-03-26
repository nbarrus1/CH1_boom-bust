#### this script is for performing the regime shift analysis using the 
##strucchange package as one suggested in Anderson et al., 2008-Cell

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)

theme_set(theme_bw())


##load in the final set of time series for meta-analysis

load(here("output","final_set.Rdata"))


test.df <- final.set$ls[17:103]

map(test.df,strucchange_bks)

test.df <- final.set$ls[[13]]

strucchange_bks(df = test.df)

fit.breaks <-  function(df, brks = 5) {
  
if(nrow(df)*0.15<2) { 
  
  BP_fit <- breakpoints(y~1, data = df, breaks = brks, h = 2)
  BP_fit
} else {
  BP_fit <- breakpoints(y~1, data = df, breaks = brks)
  BP_fit  
}}
  
  


final.set <- final.set |>
  filter(completeness.full == 1) |> 
  mutate(brks_fit = map(.x = ls, .f = fit.breaks),
         brks_fit_summ = map(.x = brks_fit, .f = summary),
         brks_opt_num = map_int(.x = brks_fit_summ, .f = function(.x) {
           temp <- as_tibble(pluck(.x, "RSS")) |> 
             slice(2)
           temp |> 
             pivot_longer(1:ncol(temp), names_to = "position", values_to = "BIC") |> 
             mutate(position = as.integer(position)) |> 
             filter(BIC == min(BIC)) |> 
             pull(position)
         }),
         brks_fit_opt = map2(.x = ls, .y = brks_opt_num, .f = fit.breaks),
         breaks.ls = map2(.x = ls, .y = brks_fit_opt, .f = function(.x, .y){
           .x |> 
             slice(.y$breakpoints)
         }))




plot.brks <- function(df.data, df.breakpoints) {
  
  df.data |> 
  ggplot(aes(x = x, y = y))+
  geom_vline(data = df.breakpoints,
           aes(xintercept = x),
         color = "red", linetype = "dashed")+
  geom_line()+
  geom_point(size = 2, color = "black", shape = 21, fill = "#666666")+
  #geom_line(aes(y = REGIME.AVE), color = "red", linetype = "dashed")+
  coord_cartesian(ylim = c(min(df.data$y),max(df.data$y)))+
  theme_bw(base_size = 12) +
  theme(
    axis.ticks.length = unit(.25, "cm"),
    axis.title.y = element_text(vjust = 2),
    plot.subtitle = element_text(face = "italic", size = 9),
    plot.title = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7)
    #plot.margin = unit(c(0,0,0,0), "cm")
  )#+
  #labs(x = unique(df$x_variable), y = unique(df$measure),
  #     subtitle = unique(df$species.names),
  #     title = paste0(unique(df$author)))

}


map2(.x = final.set$ls[1:10], .y = final.set$breaks.ls[1:10], .f = plot.brks)
