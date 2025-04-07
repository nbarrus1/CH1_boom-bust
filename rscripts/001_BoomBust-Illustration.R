rm(list = ls())



library(tidyverse)
library(patchwork)
library(here)



sim_data <- tibble(N = c(c(rep(0, times = 10),
                         rnorm(10, mean = 2, sd = 0.1),
                         2^(2:6),
                         rnorm(10, 68, sd = 2),
                         2^(6:2),
                         rnorm(10, mean = 2, sd = 0.1)),
                         c(rep(0, times = 10),
                           rnorm(10, mean = 2, sd = 0.1),
                           2^(2:6),
                           rnorm(10, 68, sd = 2),
                           2^(6:5),
                           rnorm(13, 29, sd = 2)),
                         c(rep(0, times = 10),
                           rnorm(10, mean = 2, sd = 0.1),
                           2^(2:6),
                           rnorm(25, 68, sd = 2))),
                   Year = rep(1:50, times = 3),
                   Type = c(rep("Boom-Bust", times = 50),
                            rep("Overshoot", times = 50),
                            rep("Established", times = 50)))


phases <- tibble(Year_min = c(c(24),c(24,37),c(24,37)),
                 Year_max = c(c(Inf),c(37,Inf),c(37,Inf)),
                 N_max = rep(Inf, times = 5),
                 N_min = rep(-Inf, times = 5),
                 Type = c("Established","Overshoot","Overshoot",
                          "Boom-Bust","Boom-Bust"),
                 Phase = c("High Abundance","High Abundance","Collapse","High Abundance","Collapse"),
                 Year = rep(NA_real_, times = 5),
                 N = rep(NA_real_, times = 5))


p1 <- sim_data |> 
  filter(Type == "Established") |> 
  ggplot(aes(x = Year,y = N))+
  annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "darkred", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
 # annotate(data = sim_data |> filter(Type %in% c("Overshoot","Boom-Bust")),
 #          'rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 37, y = 77, label = "High N")+
  theme_bw()+
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_blank())

  
p2 <- sim_data |> 
  filter(Type == "Overshoot") |> 
  ggplot(aes(x = Year,y = N))+
  annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "yellow", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 30.5, y = 77, label = "High N")+
  annotate("text", x = 44.5, y = 77, label = "Low N")+
  theme_bw()+
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_blank())


p3 <- sim_data |> 
  filter(Type == "Boom-Bust") |> 
  ggplot(aes(x = Year,y = N))+
  annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "darkgreen", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 30.5, y = 77, label = "High N")+
  annotate("text", x = 44.5, y = 77, label = "Low N")+
  scale_y_continuous(limits = c(0,78))+
  theme_bw()+
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_blank())
  

panel1 <- p1+p2+p3

ggsave(plot = panel1, filename = here("output","illustration.png"), device = "png",
       units = "in", width = 24, height = 4)


p4 <- sim_data |> 
  filter(Type == "Established") |> 
  filter(Year < 26) |> 
  ggplot(aes(x = Year,y = N))+
  #annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "#333333", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established"), labels = "Boom"))+
  # annotate(data = sim_data |> filter(Type %in% c("Overshoot","Boom-Bust")),
  #          'rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  #annotate("text", x = 37, y = 77, label = "High N")+
  theme_bw()+
  theme(strip.text = element_text(size = 14, face = "bold"),
        axis.text = element_blank())

ggsave(plot = p4, filename = here("output","BoomOnly.png"), device = "png",
       units = "in", width = 12/3, height = 4)
