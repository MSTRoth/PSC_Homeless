####first and last#####

options(scipen=10000)
library(tidyverse)
####Estimates#####
setwd("X:/1 Marielle Folder/Spending Data/Estimates files")
###actual and estimated values by year####


#####quarter views####

quarters <- read_csv("quarters.csv")

data <- quarters %>% 
  gather("quarter_FY","outlay", 2:57) %>% 
  separate("quarter_FY", c("quarter", "FY"), sep = "_") %>% 
  filter(quarter =="Q3"|quarter == "Q4")


dataDoD <- data %>% 
  filter(X1 == "Department of Defense--Military Programs")

dataDHS <- data %>% 
  filter(X1 == "Department of Homeland Security")

dataVA <- data %>% 
  filter(X1 == "Department of Veterans Affairs")

dataTotal <- data %>% 
  filter(X1 == "Total Outlays")

ggplot(data, aes(x=`quarter`, y=outlay))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(FY~X1) +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "obligated amount by quarter by fiscal year")

#colnames(dataDoD)
ggplot(dataDoD, aes(x=`quarter`, y=outlay, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "obligated amount by quarter by fiscal year - DoD")

ggplot(dataDHS, aes(x=`quarter`, y=outlay, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "obligated amount by quarter by fiscal year - DHS")

ggplot(dataVA, aes(x=`quarter`, y=outlay, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "obligated amount by quarter by fiscal year - VA")

ggplot(dataTotal, aes(x=`quarter`, y=outlay, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "obligated amount by quarter by fiscal year - Total")


######quarter percent total year####

pertot <- read_csv("percent of total year quarter.csv")

data<-pertot %>% 
  gather("quarter_FY", "percent_total", 2:57) %>% 
  separate("quarter_FY",c("quarter","FY"), sep = "_") %>% 
  select(-X58) 

#%>% 
 # filter(quarter =="Q1"|quarter == "Q4")

ggplot(data, aes(x=quarter, y=percent_total, group = FY))+
  geom_line(stat = "identity")+
  facet_grid(FY~X1) +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "quarter - percent of total FY obligations")


dataDoD <- data %>% 
  filter(X1 == "Department of Defense--Military Programs")

dataDHS <- data %>% 
  filter(X1 == "Department of Homeland Security")

dataVA <- data %>% 
  filter(X1 == "Department of Veterans Affairs")

dataTotal <- data %>% 
  filter(X1 == "Total Outlays")


ggplot(dataDoD, aes(x=`quarter`, y=percent_total, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "quarter - percent of total FY obligations - DoD")

ggplot(dataDHS, aes(x=`quarter`, y=percent_total, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "quarter - percent of total FY obligations - DHS")

ggplot(dataVA, aes(x=`quarter`, y=percent_total, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "quarter - percent of total FY obligations - VA")

ggplot(dataTotal, aes(x=`quarter`, y=percent_total, group = FY))+
  geom_line(stat = "identity")+
  facet_wrap("FY") +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "quarter - percent of total FY obligations - Total")

