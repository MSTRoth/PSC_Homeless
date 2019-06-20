library(tidyverse)
library(lubridate)


setwd("~/Other Requests/Alan")
afcap_data <- read_csv("AFCAP Task Orders 14-18.csv")

afcap <- afcap_data %>% 
  filter(`Contracting Title` == "Air Force Contract Augmentation Program (AFCAP IV)") %>% 
  select(`Contract Number`, `Task / Delivery Order Number`, `Contracting Title`, `Transaction Value`, 
         `Contract Type`, `Extent Competed`, `Set-Aside Type`, `Number of Offers Received`,
         `Sum of Contract's Transactions`, `Sum of Task / Delivery Order's Transactions`,
         `Parent Vendor`, `Fiscal Year`, `Contract-Task Order Number`, 
         `Product Service Code (PSC) / Federal Supply Code (FSC)`, `Contract End Date`,
         `Contract Current Completion Date`, `Task / Delivery Order Current Completion Date`,
         `Task / Delivery Order End Date`, `Fiscal Quarter`, `Total Task / Delivery Order Value`, 
         `Current Task / Delivery Order Value`, `Current Task / Delivery Order Burn Rate`,
         `Total Task / Delivery Order Burn Rate`) %>% 
  mutate("num_offers_rec" = ifelse(`Number of Offers Received` == 3|`Number of Offers Received` == 4, "3-4", 
                ifelse(`Number of Offers Received` == 5|`Number of Offers Received` == 6, "5-6",
                       ifelse(`Number of Offers Received` > 7, "7+", 
                              ifelse(`Number of Offers Received` == 2, 2, 1))
  ))) %>% 
  group_by(num_offers_rec) %>% 
 # summarise(number = n())
   summarize(sum = sum(`Transaction Value`))



  group_by(`Contract Number`, `Sum of Contract's Transactions`, `Sum of Task / Delivery Order's Transactions`,
           `Number of Offers Received`, `Parent Vendor`,`Fiscal Year`)


write.csv(afcap, "afcap.csv")



afcap_remain_chart <- afcap_data %>% 
  filter(`Contracting Title` == "Air Force Contract Augmentation Program (AFCAP IV)") %>% 
  select(`Task / Delivery Order Number`, 
         `Sum of Task / Delivery Order's Transactions`,
         `Contract-Task Order Number`, 
         `Task / Delivery Order Current Completion Date`,
         `Task / Delivery Order End Date`, `Fiscal Quarter`, `Total Task / Delivery Order Value`, 
         `Current Task / Delivery Order Value`, `Current Task / Delivery Order Burn Rate`,
         `Total Task / Delivery Order Burn Rate`) %>% 
  mutate(T_DO_end_date = as.Date(`Task / Delivery Order End Date`, format = "%m/%d/%Y")) %>% 
  mutate(FY_end_date = ifelse(T_DO_end_date >= "2015-10-01" & T_DO_end_date<= "2016-09-30", "2016",
                              ifelse(T_DO_end_date >= "2016-10-01" & T_DO_end_date<= "2017-09-30", "2017",
                                     ifelse(T_DO_end_date >= "2017-10-01" & T_DO_end_date<= "2018-09-30", "2018",
                                            ifelse(T_DO_end_date >= "2018-10-01" & T_DO_end_date<= "2019-09-30", "2019",
                                                   ifelse(T_DO_end_date >= "2019-10-01" & T_DO_end_date<= "2020-09-30", "2020",
                                                          ifelse(T_DO_end_date >= "2020-10-01" & T_DO_end_date<= "2021-09-30", "2021",
                                                                 ifelse(T_DO_end_date >= "2021-10-01" & T_DO_end_date<= "2022-09-30", "2022", "other"))
                                                          ))))),
         FQ_end_date = ifelse(month(T_DO_end_date) %in% c(10,11,12), "Q1",
                              ifelse(month(T_DO_end_date) %in% c(01,02,03), "Q2",
                                     ifelse(month(T_DO_end_date) %in% c(04,05,06), "Q3",
                                            ifelse(month(T_DO_end_date) %in% c(07,08,09), "Q4", "other"))))) %>% 
  filter(FY_end_date > 2017) %>% 
  group_by(FY_end_date, FQ_end_date) %>% 
  summarize(current_sum = sum(`Current Task / Delivery Order Value`),
            sum_remaining = sum(`Total Task / Delivery Order Value`)-sum(`Current Task / Delivery Order Value`),
            count = n(), 
            percent_sum_remaining = ((sum(`Total Task / Delivery Order Value`)-sum(`Current Task / Delivery Order Value`))/sum(`Total Task / Delivery Order Value`)*100),
            percent_spent = (sum(`Current Task / Delivery Order Value`)/sum(`Total Task / Delivery Order Value`))*100)


write.csv(afcap_remain_chart, "afcap remaining chart.csv")


  mutate("num_offers_rec" = ifelse(`Number of Offers Received` == 3|`Number of Offers Received` == 4, "3-4", 
                                   ifelse(`Number of Offers Received` == 5|`Number of Offers Received` == 6, "5-6",
                                          ifelse(`Number of Offers Received` > 7, "7+", 
                                                 ifelse(`Number of Offers Received` == 2, 2, 1))
                                   ))) %>% 
  group_by(num_offers_rec) %>% 
  # summarise(number = n())
  summarize(sum = sum(`Transaction Value`))
  
  min(x) 
  
max(x)


###################

### # Offers ####

options(scipen = 999)
#install.packages("scales")
library(scales)

AFCAP IV by num Offers

setwd("~/Other Requests/Alan")

num_offers_data <- read_csv("AFCAP num_offers.csv")

ggplot(num_offers_data, aes(x = offers_cat, y = transaction_value)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = dollar(transaction_value)), vjust = -2, fontface = "bold", size = 5)+
  geom_text(aes(label = paste(num_offers," Task Order(s)")),  vjust = -.5, size = 5) +
  labs(x = "Number of Offers", y = "Transaction Value", title = "AFCAP IV, by Number of Offers", 
       subtitle = "FY14-FY18, 211 Task Orders")+ 
  scale_x_discrete(limits = c("1","2","3-4", "5-6", "7+"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 160000000), expand = c(0, 0),
                     breaks = c(0, 25000000, 50000000, 75000000, 100000000, 125000000, 150000000), labels = dollar)+
  theme(title = element_text(size = 24), axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))

#### PSC ####

PSC_data <- read_csv("AFCAP PSC.csv")

ggplot(PSC_data, aes(x = PSC, y = transaction_value)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = dollar(transaction_value)), vjust = -2, fontface = "bold", size = 5)+
  geom_text(aes(label = paste(num_offers," Task Order(s)")), vjust = -.5,  size = 5) +
  labs(x = "PSC", y = "Transaction Value", title = "AFCAP IV, by PSC", 
       subtitle = "FY14-FY18, 211 Task Orders")+
  scale_x_discrete(limits = c("5216", "3895","C211","4240"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 275000000), expand = c(0, 0),
                     breaks = c(0, 50000000, 100000000, 150000000, 200000000, 250000000), labels = dollar)+
  theme(title = element_text(size = 24), axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))


#### Parent Vendors #####

PV_data <- read_csv("AFCAP parent_vendor.csv")

ggplot(PV_data, aes(x = parent_vendor, y = transaction_value)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = dollar(transaction_value)), vjust = -2, fontface = "bold", size = 6)+
  geom_text(aes(label = paste(num_offers," Task Order(s)")),vjust = -.5, size = 6) +
  labs(x = "Parent Vendors", y = "Transaction Value", title = "AFCAP IV, by Parent Vendors", 
       subtitle = "FY14-FY18, 211 Task Orders")+
  scale_x_discrete(limits = c("DynCorp", "IAP","PAE-Perini", "Vectrus", "Jacob's Engineering","FluorCorp",
                              "AECOM","KBR"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 85000000), expand = c(0, 0), breaks = c(0, 25000000, 50000000, 75000000), labels = dollar) +
  theme(title = element_text(size = 24), axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
  
 

##### remaining#####

remaining_data <-read_csv("afcap remaining chart.csv")

remaining <- remaining_data %>% 
  select(-X1) %>% 
  gather("sum_type", "transaction_value", 3:4)

remaining$sum_type_factor <- factor(remaining$sum_type, levels = c("sum_remaining", "current_sum"))

ggplot(remaining, aes(x = FQ_end_date, y = transaction_value, fill = sum_type_factor, color = sum_type_factor))+
  geom_bar(stat = "identity")+
  facet_wrap(~FY_end_date, nrow = 1, strip.position = "bottom")+
  scale_fill_manual(values = c(current_sum = "dodgerblue", sum_remaining = "ivory"), 
                    labels = c("Transactions to Date", "Remaining Value"),
                    name = NULL)+
                      scale_color_manual(values = c(current_sum = "dodgerblue", sum_remaining = "dodgerblue"), 
                                         guide = FALSE) +
  labs(x = "Task/Delivery Order End Date", y = "Task/Delivery Order Value", 
       title = "Expiring AFCAP IV Task orders")+ 
  scale_y_continuous(labels = dollar) +
  guides(fill = guide_legend(override.aes = list(colour = "black")))+
  theme(title = element_text(size = 24), axis.title = element_text(size = 14), 
                                                                            axis.text = element_text(size = 14))
