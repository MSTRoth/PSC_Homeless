#install.packages("tidyverse")
#install.packages("svglite")
library(tidyverse)
library(svglite)
options(scipen = 999)

##x <- data.dpap1[is.na(data.dpap1$`DPAP`),]
data <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/DPAP Categories/HHS FY19 Oct 1-Dec 26.csv") 
PSC_portfolio <- read_csv("C:/Users/Roth/Documents/Reference Tables/DPAP Crosswalk.csv")

data.dpap1 <- data %>% 
  left_join(PSC_portfolio, 
            by = c("Product Service Code (PSC) / Federal Supply Code (FSC)" = "PSC Code")) %>% 
  select("Fiscal Year", "Funding Agency", "Transaction Value", "DPAP", "Product Service Code (PSC) / Federal Supply Code (FSC)") %>% 
dplyr::rename(fiscal_year = "Fiscal Year", 
                funding_agency = "Funding Agency", 
                transaction_value = "Transaction Value") %>% 
  dplyr::group_by(fiscal_year, DPAP) %>% 
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000000)

data.agency.year.pf <- data.dpap1[complete.cases(data.dpap1), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)

### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(x = DPAP, 
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
#  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", title = "HHS Services Contracts by Category FY19 (so far)")+
  theme(axis.text.x = )

### Plot: By Agency, By Year, By services Portfolio group Category faceted ####
plot <- ggplot(data.agency.year.pf, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year))+
  geom_bar(stat = "identity") +
  scale_fill_manual("legend", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  labs(x="Fiscal year", y = "Contract Obligations (in Billions)", 
       title = "FUNDING AGENCY Services Contracts by Category FY14-FY17")