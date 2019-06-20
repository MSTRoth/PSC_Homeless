install.packages("devtools")
install.packages("roxygen2")
install.packages("tidyverse")
install.packages("gridExtra")
library(tidyverse)
devtools::create("PSCmbfx")
devtools::load_all()
roxygen2::roxygenise()
library(roxygen2)
sessionInfo()

setInternet2(TRUE)
help("Defunct")

devtools::install(build_vignettes = TRUE)

recover()





------------------------------------------------------------------------------------------------------
  #'DPAP Categories by Funding Agency and FY Range
  #'
  #'@param

  DPAP_Categories <- read_csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/Visible all Services 7-19-18 FY14-18.csv")

data.agency.year.pf <- DPAP_Categories %>%
  dplyr::rename(fiscal_year = "FY",
                funding_agency = "Funding Agency",
                transaction_value = "amount",
                portfolio_group = "DPAP_category") %>%
  filter(funding_agency %in% fund_agency) %>%  ###SELECT ONE (OR SEVERAL) FUNDING AGENCY(IES)
  dplyr::group_by(fiscal_year, portfolio_group) %>%
  dplyr::summarize(total_transaction_value = sum(transaction_value)/1000000) %>%
  filter(fiscal_year %in% year) %>%
  filter(portfolio_group != "Total")

data.agency.year.pf <- data.agency.year.pf[complete.cases(data.agency.year.pf), ]  ##remove NAs

data.agency.year.pf$fiscal_year = as.character(data.agency.year.pf$fiscal_year)
??????????????????????????????????????????
  ### Plot: By Agency, By Year, By services Portfolio group Category####

ggplot(data.agency.year.pf, aes(fill = fiscal_year,
                                x = fiscal_year,
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "FY18" = "olivedrab3")) +
  labs(y = "Contract Obligations (in Millions)")+
  facet_grid(~portfolio_group, labeller = label_wrap_gen(10))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

data.agency.year.pf1 <- data.agency.year.pf %>%
  #filter(fiscal_year != 2018) %>%
  filter(portfolio_group ==  "Electronic & Communication Services"| portfolio_group == "Facility Related Services"|
           portfolio_group ==  "Knowledge Based Services"| portfolio_group == "Medical Services")

data.agency.year.pf2 <- data.agency.year.pf %>%
  #filter(fiscal_year != 2018) %>%
  filter(portfolio_group == "Construction Services" |portfolio_group == "Equipment Related Services" |
           portfolio_group ==  "Logistics Management Services" |
           portfolio_group == "Transportation Services")




plot.first <- ggplot(data.agency.year.pf1, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Millions") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank())+ guides(fill="none")




plot.last <- ggplot(data.agency.year.pf2, aes(x = fiscal_year, y = total_transaction_value, fill = fiscal_year)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
  facet_grid(~portfolio_group, labeller = label_wrap_gen(20))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x=element_blank(),
        axis.title.y = element_blank())


plot<-grid.arrange(plot.first, plot.last, nrow = 1, widths = c(4,4))









