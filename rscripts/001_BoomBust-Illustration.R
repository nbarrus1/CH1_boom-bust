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
  geom_point(shape = 21,size = 3, fill = "#d45500ff", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
 # annotate(data = sim_data |> filter(Type %in% c("Overshoot","Boom-Bust")),
 #          'rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 37, y = 77, label = "High N")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())

  
p2 <- sim_data |> 
  filter(Type == "Overshoot") |> 
  ggplot(aes(x = Year,y = N))+
  annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "#a02c2cff", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 30.5, y = 77, label = "High N")+
  annotate("text", x = 44.5, y = 77, label = "Decline")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())


p3 <- sim_data |> 
  filter(Type == "Boom-Bust") |> 
  ggplot(aes(x = Year,y = N))+
  annotate('rect', xmin=24, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=37, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line()+
  geom_point(shape = 21,size = 3, fill = "#50164aff", show.legend = F)+
  facet_wrap(~factor(Type, levels = c("Established","Overshoot","Boom-Bust")))+
  annotate("text", x = 10, y = 77, label = "Growth")+
  annotate("text", x = 30.5, y = 77, label = "High N")+
  annotate("text", x = 44.5, y = 77, label = "Decline")+
  scale_y_continuous(limits = c(0,78))+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())
  


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

#------------------------------------------
####figure for publication####
#------------------------------------------


duration_growthsim <- tibble(N = c(c(rep(0, times = 10),
                                     rnorm(10, mean = 2, sd = 0.1),
                                     2^(2:6)),
                                   c(rep(0, times = 5),
                                     rnorm(5, mean = 2, sd = 0.1),
                                     2^(2:6),
                                     rep(NA_real_,times = 10)),
                                   c(rep(0, times = 2),
                                     rnorm(3, mean = 2, sd = 0.1),
                                     2^(2:6),
                                     rep(NA_real_,times = 15))),
                             Year = rep(1:25, times = 3),
                             Type = c(rep("short", times = 25),
                                      rep("intermediate", times = 25),
                                      rep("long", times = 25)),
                             Title = rep("Growth", times = 25*3))





p5 <- duration_growthsim |> 
  ggplot(aes(x = Year,y = N,  fill = Type))+
  geom_line(show.legend = F)+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  geom_point(shape = 21,size = 3, show.legend = F)+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Duration")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())


rate_growthsim <- tibble(N = c(c(rep(0, times = 10),
                                     rnorm(10, mean = 2, sd = 0.1),
                                     2^(2:6)),
                                   c(rep(0, times = 10),
                                     rnorm(10, mean = 2, sd = 0.1),
                                     4^(1:3),
                                     rep(NA_real_,times = 2)),
                                   c(rep(0, times = 10),
                                     rnorm(10, mean = 2, sd = 0.1),
                                     8^(2),
                                     rep(NA_real_,times = 4))),
                             Year = rep(1:25, times = 3),
                             Type = c(rep("short", times = 25),
                                      rep("intermediate", times = 25),
                                      rep("long", times = 25)),
                             Title = rep("Growth", times = 25*3))


p6 <- rate_growthsim |> 
  ggplot(aes(x = Year,y = N, fill = Type))+
  geom_line(show.legend = F)+
  geom_point(shape = 21,size = 3, show.legend = F)+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Rate")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())


duration_highnsim <- tibble(N = c(c(rnorm(25, mean = 68, sd = 2)),
                                   c(rnorm(15, mean = 64, sd = 2),
                                     rep(NA_real_,times = 10)),
                                  c(rnorm(5, mean = 60, sd = 2),
                                    rep(NA_real_,times = 20))),
                             Year = rep(1:25, times = 3),
                             Type = c(rep("short", times = 25),
                                      rep("intermediate", times = 25),
                                      rep("long", times = 25)),
                             Title = rep("High N", times = 25*3))

p7 <- duration_highnsim |> 
  ggplot(aes(x = Year,y = N, fill = Type))+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line(show.legend = F)+
  geom_point(shape = 21,size = 3, show.legend = F)+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Duration")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())


variability_highnsim <- tibble(N = c(c(rnorm(25, mean = 68, sd = 2)),
                                  c(rnorm(25, mean = 60, sd = 8)),
                                  c(rnorm(25, mean = 50, sd = 14))),
                            Year = rep(1:25, times = 3),
                            Type = c(rep("short", times = 25),
                                     rep("intermediate", times = 25),
                                     rep("long", times = 25)),
                            Title = rep("High N", times = 25*3))

p8 <- variability_highnsim |> 
  ggplot(aes(x = Year,y = N, fill = Type))+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line(show.legend = F)+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  geom_point(shape = 21,size = 3, show.legend = F)+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Variability")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())

rate_lownsim <- tibble(N =  c(rnorm(1, mean = 68, sd = 2),
                              2^(6:2),
                              rnorm(19, mean = 2, sd = 0.1),
                              rnorm(3, mean = 68, sd = 2),
                              4^(3:1),
                              rnorm(19, mean = 2, sd = 0.1),
                              rnorm(5, mean = 68, sd = 2),
                              8^(1),
                              rnorm(19, mean = 2, sd = 0.1)),
                             Year = rep(1:25, times = 3),
                             Type = c(rep("short", times = 25),
                                      rep("intermediate", times = 25),
                                      rep("long", times = 25)),
                             Title = rep("Decline", times = 25*3))


p9 <- rate_lownsim |> 
  ggplot(aes(x = Year,y = N, fill = Type))+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  geom_line(show.legend = F)+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  geom_point(shape = 21,size = 3, show.legend = F)+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Rate")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())


magnitude_lownsim <- tibble(N =  c(rnorm(1, mean = 68, sd = 2),
                              2^(6:2),
                              rnorm(19, mean = 6.8, sd = 0.1),
                              rnorm(1, mean = 68, sd = 2),
                              2^(6:2),
                              rnorm(19, mean = 3, sd = 0.1),
                              rnorm(1, mean = 68, sd = 2),
                              2^(6:2),
                              rnorm(3, mean = 1, sd = 0.1),
                              rep(0, times = 16)),
                       Year = rep(1:25, times = 3),
                       Type = c(rep("short", times = 25),
                                rep("intermediate", times = 25),
                                rep("long", times = 25)),
                       Title = rep("Decline", times = 25*3))


p10 <- magnitude_lownsim |> 
  ggplot(aes(x = Year,y = N, fill = Type))+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=.2, fill='#666666')+
  scale_fill_manual(values = c("#50164aff","#50164abf","#50164a80"))+
  geom_line(show.legend = F)+
  geom_point(shape = 21,size = 3, show.legend = F)+
  facet_grid(~Title)+
  scale_y_continuous(limits = c(0,78))+
  annotate("text", x = 12.5, y = 77, label = "Magnitude")+
  theme_bw()+
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_blank())

panel1 <-(p1+p5+p6)+plot_layout(widths = c(2,1,1))
panel2 <-(p2+p7+p8)+plot_layout(widths = c(2,1,1))
panel3 <-(p3+p9+p10)+plot_layout(widths = c(2,1,1))

panel.complete <- panel1/panel2/panel3


ggsave(filename = here("output/figure_editing","figure1_boombustillustration.pdf"),
       device = "pdf", units = "mm", width = 160, height = 140)

