####Market Briefings Graphics####


#install.packages("tidyverse")
#install.packages("svglite")
library(tidyverse)
library(svglite)
options(scipen = 999)

setwd("S:/1 Marielle Folder/Data Sets/Vendor Specific/")

####read dataset into environment####
data <- read_csv("LMI Consulting Company Profile.csv")


####Prime Contract Obligations By Agency and Year####
top_6_agencies <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2018) %>% 
  group_by(funding_agency) %>% 
  dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
  arrange(desc(grand_total_transaction_value)) %>% 
  top_n(6) 


top_6_agencies <- top_6_agencies$funding_agency

  

data.agency.year <- data %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(funding_agency %in% top_6_agencies) %>% 
  dplyr::group_by(fiscal_year, funding_agency) %>% 
  dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))

data.agency.year$facet = factor(data.agency.year$funding_agency, levels = c(top_6_agencies))
data.agency.year$fiscal_year = as.character(data.agency.year$fiscal_year)

### Plot: By Agency, By Year####
# 
# ggplot(data.agency.year, aes(fill = fiscal_year, 
#                              x = funding_agency, 
#                              y = total_transaction_value))+
#   geom_bar(stat = "identity", position = position_dodge()) +
#   scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
#   labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "COMPANY NAME Contract Obligations by Agency FY14-FY17")

### Plot: By Agency, By Year, faceted ####
ggplot(data.agency.year, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~facet, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)", 
       title = "COMPANY NAME Contract Obligations by Agency FY14-FY17") +
  theme(plot.title = element_text(hjust = 0.5)) 
  # scale_x_continuous(limits = )
ggsave("Contract Obligation by Agency top 6.jpg", plot, 
        width = 11, height = 6, units = "in")




####By Agency, Year, and services Portfolio group Category####
data <- read_csv("LMI Consulting Company Profile.csv")
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/DPAP Crosswalk.csv")

data.agency.year.pf <- data %>% 
 left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC Code")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", DPAP) %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  #filter(fiscal_year != 2018) %>% 
  filter(funding_agency == "Department of Defense (DOD)") %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  #filter(funding_agency == "Department of Health and Human Services (HHS)") %>% 
  #filter(funding_agency == "Department of Transportation (DOT)") %>% 
  #filter(funding_agency == "Department of Veterans Affairs (VA)") %>% 
  #filter(funding_agency == "Department of Homeland Security (DHS)") %>% 
  dplyr::group_by(fiscal_year, DPAP) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000000)

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year, 
                             x = DPAP, 
                             y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
plot <- ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
       title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

ggsave(paste("Contract Obligations by Agency top.jpg"), plot, 
       width = 11, height = 6, units = "in")





# ####By Agency####
# data.agency <- data %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>%
#   filter(fiscal_year != 2018) %>% 
#   dplyr::group_by(funding_agency) %>% 
#   dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))
# 
# 
# 
# ####By Year####
# data.year <- data %>% 
#   select("Fiscal Year", "Funding Agency", "Transaction Value") %>% 
#   dplyr::rename(fiscal_year = "Fiscal Year", 
#                 funding_agency = "Funding Agency", 
#                 transaction_value = "Transaction Value") %>% 
#   filter(fiscal_year != 2018) %>% 
#   filter(funding_agency = "Department of Defense (DOD)"|
#            funding_agency = "Department of Health and Human Services (HHS)"|
#            funding_agency = "Department of Transportation (DOT)"|
#            funding_agency = "Department of Veterans Affairs (VA)" |
#            funding_agency = "Department of Homeland Security (DHS)") %>% 
#   dplyr::group_by(fiscal_year) %>% 
#   dplyr::summarize(total_transaction_value = (sum(transaction_value)/1000000000))



####Year, and services Portfolio group Category####

PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/Acquisition_services_taxonomy.csv")

data.agency.year.pf <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "Portfolio Group") %>% 
  dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value",
                portfolio_group = "Portfolio Group") %>% 
  filter(fiscal_year != 2018) %>% 
  filter(funding_agency == "Department of Defense (DOD)") %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  #filter(funding_agency == "Department of Health and Human Services (HHS)") %>% 
  #filter(funding_agency == "Department of Transportation (DOT)") %>% 
  #filter(funding_agency == "Department of Veterans Affairs (VA)") %>% 
  #filter(funding_agency == "Department of Homeland Security (DHS)") %>% 
  dplyr::group_by(fiscal_year, portfolio_group) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000000)

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year, 
                                x = portfolio_group, 
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
plot <- ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
       title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")

ggsave(paste("Contract Obligations by Agency top.jpg"), plot, 
       width = 11, height = 6, units = "in")




 