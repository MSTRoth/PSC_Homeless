library(colorspace)
library(readr)
library(RColorBrewer)
library(tidyverse)
library(scales)

contract_obs_by_quarter <- function(year, num_size = 3, notyear_prop,
                                    FY_range, title = paste("Contract Obligations by Quarter: ", FY_range, sep = ""),
                                    subtitle = NULL, h = 6, w = 11, file_ext = ".jpg"){
  
  #Location for saving charts
  setwd("~/Market Briefings/Data/Government-Wide data")
  
  data <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Civilian and Defense Data by quarter.csv")
 # data <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Civilian and Defense Services Data by quarter.csv")
  data$Year = as.character(data$Year)
  
  data.civdef_total <- data %>%
    rename(civ_def = "Civ/Def",
           total_obligations = "Contract Obligations (in Billions)") %>%
    group_by(Year, civ_def) %>%
    mutate(label_y = cumsum(total_obligations))
  
  data.civdef_total$Year = as.character(data.civdef_total$Year)
  
  data.civdef <- data %>%
    rename(civ_def = "Civ/Def",
           total_obligations = "Contract Obligations (in Billions)") %>%
    group_by(Year, civ_def) %>%
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations)) %>%
    filter(Year %in% c(2016, 2017, 2018))
  
  
  plot <- ggplot(data.civdef, aes(x = Year, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1"))), color = Year) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), 
                  size = 3, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(subset(data.civdef, (Year == 2016) | (Year == 2017) | (civ_def == "Civilian"))), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = format(..y..,nsmall = 1), group = Year),
                 geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(2, 4, 6, 8)])+
    scale_y_continuous(labels = scales::dollar, limits = c(0, 340))+
    facet_grid(~civ_def, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations by Quarter: FY16-FY18", subtitle = NULL) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  
  plot
  
  ggsave(paste("Contract Obligations Civ-Def ","FY16-FY18", " by quarter", "evens outlined", ".jpg", sep = ""), plot,
         width = 11, height = 6, units = "in")
  

}

contract_obs_by_quarter(year = c(2016, 2107, 2018), num_size = 3, notyear_prop =  2018,
                       FY_range = "FY16-FY18", title = "Contract Obligations by Quarter: FY16-FY18",
                                    subtitle = NULL, h = 8, w = 11, file_ext = ".jpg")
  
x<- subset(data.civdef, (Year == 2016) | (Year == 2017) | (civ_def == "Civilian"))
