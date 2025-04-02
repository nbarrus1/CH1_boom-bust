#### this script is for performing the regime shift analysis using the 
##sequential t test method sometimes called STARS

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)

theme_set(theme_bw())

###create the functions designed by Rodionov & Overland, Journal of Marine Science,
###2005


STARS <- function(test.df, l = 5, p.crit=0.05, transform = FALSE) {
  
degrees.freedom = l*2 - 2
t = qt(p.crit/2, df = degrees.freedom, lower.tail = FALSE)


if (transform == FALSE) {
  y.seq = test.df |> 
  ungroup() |> 
  mutate(lamba.ln = log(lead(y)/y)) |> 
  pull(y) 
  } else {
    y.seq = test.df |> 
      ungroup() |> 
      mutate(y = if_else(y == 0, true = 0.001, false = y),
        lamba.ln = log(lead(y)/y)) |> 
      pull(lamba.ln)   
    
  }



y.matrix <- matrix(0, nrow = length(y.seq), ncol = l)
colnames(y.matrix) <- paste0("y",(0:(l-1)))
rownames(y.matrix) <- paste0(test.df$x)

for (i in 1:l) {
  if(i == 1) {
    y.matrix[,i] <-y.seq 
  } else {
  y.matrix[,i] <- lead(y.seq,n = i-1)
  }
}

var.l <- as_tibble(y.matrix) |> 
  mutate(nrow = 1:nrow(y.matrix)) |> 
  pivot_longer(cols = 1:l, names_to = "lead",values_to = "value") |> 
  group_by(nrow) |> 
  summarise(sd = sd(value,na.rm = T)) |> 
  ungroup() |> 
  summarise(sd.l = mean(sd, na.rm = T)) |> 
  mutate(var.l = sd.l^2) |> 
  pull(var.l)

sd.l <- as_tibble(y.matrix) |> 
  mutate(nrow = 1:nrow(y.matrix)) |> 
  pivot_longer(cols = 1:l, names_to = "lead",values_to = "value") |> 
  group_by(nrow) |> 
  summarise(sd = sd(value,na.rm = T)) |> 
  ungroup() |> 
  summarise(sd.l = mean(sd, na.rm = T)) |> 
  mutate(var.l = sd.l^2) |> 
  pull(sd.l)

diff = t*sqrt(2*var.l/l)

x.cur = mean(y.seq[1:l], na.rm = T)
crit.upp = x.cur + diff
crit.low = x.cur - diff
RSI = rep(0, times = l)
test.df$REGIME <- NA_character_
test.df$REGIME.AVE <- 0
h = 1


for (i in 2:length(y.seq)) {
  
#-------------------------------------------------
##first if  if else statement is for determining the
#new value is beyond the crit value range 
#-------------------------------------------------
  
##if yes and is above the upper tail
###calculate RSI
  
  if (is.na(y.seq[i])){
    
    test.df$REGIME[i] <- NA_character_
    
    x.cur = x.cur
    
    test.df$REGIME.AVE[h:i] <- NA_real_
    
    crit.upp = x.cur + diff
    crit.low = x.cur - diff
    
    RSI = rep(0, times = l)
    print(paste0(test.df$x[i],": NA"))
    
    
  } else {
  
  if (y.seq[i] >= crit.upp) {  
    
    
    RSI[1] <- (y.seq[i]-(x.cur+diff))/(l*sd.l)
    
    for(j in 2:l) {  
      
      RSI[j] <- (y.seq[i+j-1]-(x.cur+diff))/(l*sd.l)
      
    }
    
    RSI <- cumsum(RSI)    
    
    ##----------------
    ##second if else is used to evaluate the RSI
    ##----------------
    
    #if there were negatives recalculate the x.cur and
    #critical values
    
    if (any(RSI < 0, na.rm = T)) {
      
      test.df$REGIME[i] <- "no"
      
      x.cur = mean(y.seq[if((i<=(h+l-1))){(h:(h+l-1))}else{h:i}],na.rm =T)
      
      test.df$REGIME.AVE[h:i] <- x.cur
      
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)
      print(paste0(test.df$x[i],": Negative RSI"))
      
      #if there were no negative values recalculate the x.cur using the 
      #the new regime
      
    } else { 
      
      test.df$REGIME[i] <- "yes"
      
      x.cur = mean(y.seq[i:(i+l-1)],na.rm =T)
      
      test.df$REGIME.AVE[i] <- x.cur
      
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)  
      
      h = i
      
      print(paste0(test.df$x[i],": Regime Shift"))
      
      
    }
##if yes and is below the lower tail
###calculate RSI  
      
    
  } else if (y.seq[i] <= crit.low) {
    
    ##calculate the RSI
    
      RSI[1] <- (x.cur-diff-y.seq[i])/(l*sd.l)
      
      for(j in 2:l) {  
        
        RSI[j] <- (x.cur-diff-y.seq[i+j-1])/(l*sd.l)
        
      }
      
      RSI <- cumsum(RSI)  
      
    ##----------------
    ##second if else is used to evaluate the RSI
    ##----------------
    
    #if there were negatives recalculate the x.cur and
    #critical values
    
    if (any(RSI < 0, na.rm = T)) {
      
      test.df$REGIME[i] <- "no"
      
      x.cur = mean(y.seq[if((i<=(h+l-1))){(h:(h+l-1))}else{h:i}],na.rm =T)
      
      test.df$REGIME.AVE[i] <- x.cur
      
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)
      print(paste0(test.df$x[i],": Negative RSI"))
  
      
    #if there were no negative values recalculate the x.cur using the 
    #the new regime
      
    } else { 
      
      test.df$REGIME[i] <- "yes"  
      
    x.cur = mean(y.seq[i:(i+l-1)],na.rm =T)
    
    test.df$REGIME.AVE[i] <- x.cur
    
    crit.upp = x.cur + diff
    crit.low = x.cur - diff
    
    RSI = rep(0, times = l)  
    
    h = i
    
    print(paste0(test.df$x[i],": Regime Shift"))
    
      }
    
##if no and is below the lower tail
###calculate RSI    
    
    
    
  }else{                         #if the next value doesn not change in critical value
                                 #then lump with previous sequence and update cur mean and critical values
    
    test.df$REGIME[i] <- "no"
    
    if(is.na(test.df$REGIME[(i-1)]) | !any(test.df$REGIME == "yes", na.rm = T)) {
    
      x.cur = mean(if(i<=l){y.seq[1:l]}else{y.seq[1:i]},na.rm =T)
      
      test.df$REGIME.AVE[1:i] <- x.cur
      
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      print(paste0(test.df$x[i]))
      
    } else {
      
      x.cur = mean(y.seq[if((i<=(h+l-1))){(h:(h+l-1))}else{h:i}],na.rm =T)
      
      test.df$REGIME.AVE[h:i] <- x.cur
      
      crit.upp = x.cur + diff
      crit.low = x.cur - diff
      
      RSI = rep(0, times = l)  
      print(paste0(test.df$x[i]))
      
    }
    
  } 
}


}

test.df

}


#--------------------------------------------
####apply STARS function to timeseries ####
#--------------------------------------------

##load in the final set of time series for meta-analysis

load(here("output","final_set.Rdata"))


final.set <- final.set |> 
  filter(completeness.full == 1) |> 
  mutate(regime.length = if_else(years.surveyed < 15, true = 3, false = 5),
         ls = map2(.x = ls, .y = regime.length, .f = STARS, transform = FALSE))


###



plot_timeseries <- function(df) {
  df |> 
    ggplot(aes(x = x, y = y))+
    geom_line()+
    geom_point(size = 2, color = "black", shape = 21, fill = "#666666")+
    #geom_line(aes(y = REGIME.AVE), color = "red", linetype = "dashed")+
    geom_vline(data = df |> filter(REGIME == "yes"), aes(xintercept = x), color = "red", linetype = "dashed")+
    theme_bw(base_size = 12) +
    theme(
      axis.ticks.length = unit(.25, "cm"),
      axis.title.y = element_text(vjust = 2),
      plot.subtitle = element_text(face = "italic", size = 9),
      plot.title = element_text(size = 9),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
      #plot.margin = unit(c(0,0,0,0), "cm")
    )+
    labs(x = unique(df$x_variable), y = unique(df$measure),
         subtitle = unique(df$species.names),
         title = paste0(unique(df$author)))
}



final.plots <- final.set |>
  unnest(cols = ls) |> 
  group_by(plot,group) |> 
  nest(.key = "ls") |> 
  mutate(timeseries = map(ls, .f = plot_timeseries)) |> 
  select(-ls)


final.plots$timeseries[11:20]


save(final.plots,file = here("output","literatrure_timeseries_plots.Rdata"))

#---------------------------------------------
###for loop to save my ggplot pannels###
#---------------------------------------------


nrow(final.plots)/12

pdf(here("output","timeseriespanels_STARS.pdf"), width = 11, height = 8)


for(i in 1:(ceiling((nrow(final.plots)/12)-1))) {

  
if (i == 1) {
  
    timeseries.panel <- reduce(final.plots$timeseries[i:(i*12)],`+`)+
      plot_layout(ncol = 4, nrow = 3)
    print(timeseries.panel)
    
} else {
  
   timeseries.panel <- reduce(final.plots$timeseries[((i*12)+1):((i+1)*12)],`+`)+
      plot_layout(ncol = 4, nrow = 3)
   print(timeseries.panel)
   
}
    
}


dev.off()


