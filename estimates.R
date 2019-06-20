options(scipen=10000)
library(tidyverse)
####Estimates#####
setwd("X:/1 Marielle Folder/Spending Data/Estimates files")
###actual and estimated values by year####

values <- read_csv("Value.csv")

data <- values %>% 
  gather("FY E/A","total_outlay", 2:13) %>% 
  separate("FY E/A", c("FY", "E/A"))


ggplot(data, aes(x=FY, y=total_outlay, fill = `E/A`))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(~X1) +
  theme(axis.text.x = element_text(angle = -45))


#####%dif####

pdif <- read_csv("%dif.csv")

data <- pdif %>% 
  gather("type FY","percentdif", 2:7) %>% 
  separate("type FY", c("space", "type", "FY")) %>% 
  select(-space, -type)


ggplot(data, aes(x=FY, y=percentdif))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(~X1) +
  theme(axis.text.x = element_text(angle = -45))





#####$dif####

ddif <- read_csv("$dif.csv")

data <- ddif %>% 
  gather("type FY","dollardif", 2:7) %>% 
  separate("type FY", c("space", "type", "FY")) %>% 
  select(-space, -type)


ggplot(data, aes(x=FY, y=dollardif))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(~X1) +
  theme(axis.text.x = element_text(angle = -45))



#####%m:m####

pmm <- read_csv("%mm.csv")

data <- pmm %>% 
  gather("type FY","percentdifmm", 2:4) %>%
  separate("type FY", c("space", "type", "m","mm","FY1", "FY2")) %>% 
  select(-space, -type, -m, -mm) %>% 
  unite("FY:FY", 3:4, sep = ":")


ggplot(data, aes(x=`FY:FY`, y=percentdifmm))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(~X1) +
  theme(axis.text.x = element_text(angle = -45))




######$m:m####

dmm <- read_csv("$mm.csv")

data <- dmm %>% 
  gather("type FY","dollardifmm", 2:6) %>%
  separate("type FY", c("space", "type", "m","mm","FY1", "FY2")) %>% 
  select(-space, -type, -m, -mm) %>% 
  unite("FY:FY", 2:3, sep = ":")


ggplot(data, aes(x=`FY:FY`, y=dollardifmm))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(~X1) +
  theme(axis.text.x = element_text(angle = -45))



####Month views####

month <- read_csv("months.csv")

data <- month %>% 
  gather("month FY","outlay", 2:73) %>% 
  separate("month FY", c("month", "FY"), sep = " FY") %>% 
  filter(FY>2013)

ggplot(data, aes(x=`month`, y=outlay))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(FY~X1) +
  theme(axis.text.x = element_text(angle = -45))


#####quarter views####

quarters <- read_csv("quarters.csv")

data <- quarters %>% 
  gather("quarter_FY","outlay", 2:57) %>% 
  separate("quarter_FY", c("quarter", "FY"), sep = "_") 


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

###q%dif####

qpdif <- read_csv("q%dif.csv")

data <- qpdif %>% 
  gather("type_FY","percentdif", 2:13) %>% 
  separate("type_FY", c("type", "FY"), sep = "_")

ggplot(data, aes(x=type, y=percentdif))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(FY~X1) +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "percent difference in obligated amount")





#####q$dif####

qddif <- read_csv("q$dif.csv")

data <- qddif %>% 
  gather("type_FY","dollardif", 2:13) %>% 
  separate("type_FY", c("type", "FY"), sep = "_") 


ggplot(data, aes(x=type, y=dollardif/1000))+
  geom_bar(stat = "identity", position =position_dodge())+
  facet_grid(FY~X1) +
  theme(axis.text.x = element_text(angle = -45))+
  labs(title = "dollar difference (in thousands) obligated amount")


######quarter percent total year####

pertot <- read_csv("percent of total year quarter.csv")

data<-pertot %>% 
  gather("quarter_FY", "percent_total", 2:57) %>% 
  separate("quarter_FY",c("quarter","FY"), sep = "_") %>% 
  select(-X58)

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

