library(tidyverse)
library(gridExtra)

###Convert to DPAP by PSC
data<- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Funding Agencies and Subsets for DPAP/DOE/DOE Tranactions FY16-18.csv")
PSC <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")



data_DPAP<- data %>% 
  rename(PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  left_join(PSC, by= c("PSC" = "PSC Code")) %>% 
  mutate(product = ifelse(substr(PSC,1,1) %in% c(0,1,2,3,4,5,6,7,8,9), "Product", "Other")) %>% 
  select(`Transaction Value`, `Fiscal Year`, `DPAP`, PSC, product) %>%
  filter(PSC != "UNKN") %>% 
  mutate(full_DPAP = ifelse(!is.na(DPAP), DPAP, product)) %>% 
  group_by(`Fiscal Year`, `full_DPAP`) %>% 
  summarise(sum = sum(`Transaction Value`/1000000)) 

data_DPAP$`Fiscal Year` <- as.character((data_DPAP$`Fiscal Year`))
data_DPAP$full_DPAP <- gsub(" ", "\n", data_DPAP$full_DPAP)
data_DPAP$full_DPAP <- factor(data_DPAP$full_DPAP, levels = c("Facility\nRelated\nServices", 
                                                              "Construction\nServices", "Knowledge\nBased\nServices", 
                                                              "Research\nand\nDevelopment", "Electronic\n&\nCommunication\nServices", 
                                                              "Equipment\nRelated\nServices",
                                                              "Logistics\nManagement\nServices", "Medical\nServices", 
                                                              "Transportation\nServices", "Product"))

  # filter(!is.na(`Product or Service`)) %>% 
  # mutate(category = factor(`Product or Service`, levels = x)) %>% 
  # arrange(category) %>% 
  # mutate(label_y = cumsum(sum),
  #        prop = 100*sum/sum(sum)) 

data_DPAP1 <- data_DPAP %>% 
  filter(full_DPAP == "Facility\nRelated\nServices")

unique(data_DPAP$full_DPAP)

data_DPAP2 <- data_DPAP %>% 
  filter(full_DPAP %in% c("Construction\nServices", "Knowledge\nBased\nServices", "Research\nand\nDevelopment"))

data_DPAP3 <- data_DPAP %>% 
  filter(full_DPAP %in% c("Electronic\n&\nCommunication\nServices", "Equipment\nRelated\nServices",
                          "Logistics\nManagement\nServices", "Medical\nServices", 
                          "Transportation\nServices", "Product") )


plot1 <- ggplot(data_DPAP1, aes(x = full_DPAP, y = sum, fill = `Fiscal Year`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(sum, digits = 0)), size = 5, position = position_dodge(width=1), vjust = -1.5)+
  scale_fill_manual(values = c("2016" = "royalblue3", "2017" = "gold3", "2018" = "deeppink4")) +
  labs(y = paste("Contract Obligations (in ", "millions)", sep = ""))+
  guides(fill = "none")+
  theme(axis.title.x = element_blank(), axis.text = element_text(size = 11), axis.title.y = element_text(size = 11, face = "bold"))

plot2 <- ggplot(data_DPAP2, aes(x = full_DPAP, y = sum, fill = `Fiscal Year`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(sum, digits = 0)), size = 5, position = position_dodge(width=1), vjust = -1.5)+
  scale_fill_manual(values = c("2016" = "royalblue3", "2017" = "gold3", "2018" = "deeppink4")) +
  # labs(y = paste("Contract Obligations (in ", "millions)", sep = ""), x = "DPAP")+
  guides(fill = "none")+
  theme(axis.title = element_blank(), axis.text = element_text(size = 11))

plot3 <- ggplot(data_DPAP3, aes(x = full_DPAP, y = sum, fill = `Fiscal Year`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(sum, digits = 0)), size = 5, position = position_dodge(width=1), vjust = -1.5)+
  scale_fill_manual(values = c("2016" = "royalblue3", "2017" = "gold3", "2018" = "deeppink4")) +
  # labs(y = paste("Contract Obligations (in ", "millions)", sep = ""), x = "DPAP") 
  theme(axis.title = element_blank(), axis.text = element_text(size = 11))



plot<-grid.arrange(plot1, plot2, plot3, nrow = 1, widths = c(1, 2, 4))

data_DPAP_UNRSB<- data %>% 
  rename(PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  left_join(PSC, by= c("PSC" = "PSC Code")) %>% 
  mutate(product = ifelse(substr(PSC,1,1) %in% c(0,1,2,3,4,5,6,7,8,9), "Product", "Other")) %>% 
  filter(`Set-Aside Type` == "No set aside used." & 
           `Contracting Officer's Determination of Business Size` == "S") %>% 
  select(`Transaction Value`, `Fiscal Year`, `DPAP`, PSC, product) %>%
  filter(PSC != "UNKN") %>% 
  mutate(full_DPAP = ifelse(!is.na(DPAP), DPAP, product)) %>% 
  group_by(`Fiscal Year`, `full_DPAP`) %>% 
  summarise(sum = sum(`Transaction Value`/1000000)) %>% 
  left_join(data_DPAP, by= c("Fiscal Year","full_DPAP")) %>% 
  mutate(percent = (sum.x/sum.y)*100)

write.csv(data_DPAP_UNRSB, "data_DPAP_UNRSP.csv")

data_DPAP_FO<- data %>% 
  rename(PSC = `Product Service Code (PSC) / Federal Supply Code (FSC)`) %>% 
  left_join(PSC, by= c("PSC" = "PSC Code")) %>% 
  mutate(product = ifelse(substr(PSC,1,1) %in% c(0,1,2,3,4,5,6,7,8,9), "Product", "Other")) %>% 
  filter(`Set-Aside Type` == "No set aside used.") %>% 
  select(`Transaction Value`, `Fiscal Year`, `DPAP`, PSC, product) %>%
  filter(PSC != "UNKN") %>% 
  mutate(full_DPAP = ifelse(!is.na(DPAP), DPAP, product)) %>% 
  group_by(`Fiscal Year`, `full_DPAP`) %>% 
  summarise(sum = sum(`Transaction Value`/1000000)) %>% 
  left_join(data_DPAP_UNRSB, by= c("Fiscal Year","full_DPAP")) %>% 
  mutate(percentFO = (sum.y/sum.x)*100)

data_DPAP_UNRSB$`Fiscal Year` <- as.character((data_DPAP_UNRSB$`Fiscal Year`))
data_DPAP_UNRSB$full_DPAP <- gsub(" ", "\n", data_DPAP_UNRSB$full_DPAP)
data_DPAP_UNRSB$full_DPAP <- factor(data_DPAP_UNRSB$full_DPAP, levels = c("Facility\nRelated\nServices", 
                                                              "Construction\nServices", "Knowledge\nBased\nServices", 
                                                              "Research\nand\nDevelopment", "Electronic\n&\nCommunication\nServices", 
                                                              "Equipment\nRelated\nServices",
                                                              "Logistics\nManagement\nServices", "Medical\nServices", 
                                                              "Transportation\nServices", "Product"))


# filter(!is.na(`Product or Service`)) %>% 
# mutate(category = factor(`Product or Service`, levels = x)) %>% 
# arrange(category) %>% 
# mutate(label_y = cumsum(sum),
#        prop = 100*sum/sum(sum)) 

ggplot(data_DPAP_UNRSB, aes(x = full_DPAP, y = sum.x, fill = `Fiscal Year`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(sum.x, digits = 2)), size = 4.5, position = position_dodge(width=1), vjust = -2)+
  geom_text(aes(label = sprintf('%.0f%%', percent), y = sum.x), size = 4.5, position = position_dodge(width=1), vjust = -.7)+
  scale_fill_manual(values = c("2016" = "royalblue3", "2017" = "gold3", "2018" = "deeppink4")) +
  labs(y = paste("Contract Obligations (in ", "millions)", sep = ""), x = "DPAP", 
       title = "DOE Contract Obligation FY16-FY18 by DPAP Category in Millions", subtitle = "**No Set Asides Used, Small Business determination")+
  theme(axis.text = element_text(size = 11), plot.title = element_text(size = 20), plot.subtitle = element_text(size = 14, face = "italic"),
        axis.title.y = element_text(size = 11, face = "bold"), axis.title.x = element_blank())

