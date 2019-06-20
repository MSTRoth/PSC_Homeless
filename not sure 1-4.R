##Agency quarter comparison#####
library(lubridate)

##Contracts####

setwd("~/Other Requests/Paul/")

FY18_contracts <- read_csv("USAID contract Spend USASpending FY18.csv")
FY17_contracts <- read_csv("USAID contract Spend USASpending FY17.csv")

colnames(FY18_contracts)

FY18 <- FY18_contracts %>% 
  mutate(fiscal_year = "FY18",
         betterdate = ymd(as.Date(action_date, format = "%m/%d/%Y")),
         fiscal_quarter = ifelse(month(betterdate) %in% 
                                   c("10","11","12"), "Q1", 
                                 ifelse(month(betterdate) %in% 
                                          c("1","2","3"), "Q2", 
                                        ifelse(month(betterdate) %in% 
                                                 c("4","5","6"), "Q3", 
                                               ifelse(month(betterdate) %in% 
                                                        c("7","8","9"), "Q4", NA))))) %>%
  group_by(fiscal_year, fiscal_quarter) %>% 
  summarise(sum_action_obligated = sum(federal_action_obligation))
  

new18_rowQ1Q2 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q1+Q2", 
                          sum_action_obligated = sum(FY18[1,3]+FY18[2,3]), stringsAsFactors = FALSE)
new18_rowQ3Q4 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q3+Q4", 
                          sum_action_obligated = sum(FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
new18_rowall <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Total FY", 
                         sum_action_obligated = sum(FY18[1,3]+FY18[2,3]+FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
FY_18 <- bind_rows(FY18,new18_rowQ1Q2, new18_rowQ3Q4, new18_rowall)

FY17 <- FY17_contracts %>% 
  mutate(fiscal_year = "FY17",
         betterdate = ymd(as.Date(action_date, format = "%m/%d/%Y")),
         fiscal_quarter = ifelse(month(betterdate) %in% 
                                   c("10","11","12"), "Q1", 
                                 ifelse(month(betterdate) %in% 
                                          c("1","2","3"), "Q2", 
                                        ifelse(month(betterdate) %in% 
                                                 c("4","5","6"), "Q3", 
                                               ifelse(month(betterdate) %in% 
                                                        c("7","8","9"), "Q4", NA))))) %>%
    group_by(fiscal_year, fiscal_quarter) %>% 
    summarise(sum_action_obligated = sum(federal_action_obligation))

new17_rowQ1Q2 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q1+Q2", 
                          sum_action_obligated = sum(FY18[1,3]+FY18[2,3]), stringsAsFactors = FALSE)
new17_rowQ3Q4 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q3+Q4", 
                          sum_action_obligated = sum(FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
new17_rowall <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Total FY", 
                         sum_action_obligated = sum(FY18[1,3]+FY18[2,3]+FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
FY_17 <- bind_rows(FY18,new17_rowQ1Q2, new17_rowQ3Q4, new17_rowall)  

FY_con_compare <- rbind(FY_17, FY_18)


####Grants####

FY18_grants <- read_csv("USAID Grant Spend FY18.csv")
FY17_grants <- read_csv("USAID Grant Spend FY18.csv")



FY18 <- FY18_grants %>% 
  mutate(fiscal_year = "FY18",
         betterdate = ymd(as.Date(action_date, format = "%m/%d/%Y")),
         fiscal_quarter = ifelse(month(betterdate) %in% 
                                   c("10","11","12"), "Q1", 
                                 ifelse(month(betterdate) %in% 
                                          c("1","2","3"), "Q2", 
                                        ifelse(month(betterdate) %in% 
                                                 c("4","5","6"), "Q3", 
                                               ifelse(month(betterdate) %in% 
                                                        c("7","8","9"), "Q4", NA))))) %>%
  select(fiscal_year, fiscal_quarter, federal_action_obligation, non_federal_funding_amount) %>%   
  mutate(federal_action_obligation = as.numeric(federal_action_obligation),
         non_federal_funding_amount = as.numeric(non_federal_funding_amount)) %>% 
  group_by(fiscal_year, fiscal_quarter) %>% 
  summarize(federal_action_obligation = sum(federal_action_obligation), 
            non_federal_funding_amount = sum(non_federal_funding_amount))
  
aggregate(FY18$federal_action_obligation, FUN = sum)
  summarise(sum = sum(federal_action_obligation), sum2 = sum)
  
  
    sum_action_obligated = sum(federal_action_obligation),
            sum_non_federal_funding_amount = sum(FY18$non_federal_funding_amount))

class(FY18$non_federal_funding_amount)



new18_rowQ1Q2 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q1+Q2", 
                            sum_action_obligated = sum(FY18[1,3]+FY18[2,3]), 
                            sum_non_federal_funding_amount = sum(FY18[1,4]+FY18[2,4]), stringsAsFactors = FALSE)
new18_rowQ3Q4 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q3+Q4", 
                            sum_action_obligated = sum(FY18[3,3]+FY18[4,3]), 
                            sum_non_federal_funding_amount = sum(FY18[3,4]+FY18[4,4]), stringsAsFactors = FALSE)
new18_rowall <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Total FY", 
                           sum_action_obligated = sum(FY18[1,3]+FY18[2,3]+FY18[3,3]+FY18[4,3]), 
                           sum_non_federal_funding_amount = sum(FY18[1,4]+FY18[2,4]+FY18[3,4]+FY18[4,4]), stringsAsFactors = FALSE)
FY_18 <- bind_rows(FY18,new18_rowQ1Q2, new18_rowQ3Q4, new18_rowall)

FY17 <- FY17_contracts %>% 
  mutate(fiscal_year = "FY17",
         betterdate = ymd(as.Date(action_date, format = "%m/%d/%Y")),
         fiscal_quarter = ifelse(month(betterdate) %in% 
                                   c("10","11","12"), "Q1", 
                                 ifelse(month(betterdate) %in% 
                                          c("1","2","3"), "Q2", 
                                        ifelse(month(betterdate) %in% 
                                                 c("4","5","6"), "Q3", 
                                               ifelse(month(betterdate) %in% 
                                                        c("7","8","9"), "Q4", NA))))) %>%
  group_by(fiscal_year, fiscal_quarter) %>% 
  summarise(sum_action_obligated = sum(federal_action_obligation))

new17_rowQ1Q2 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q1+Q2", 
                            sum_action_obligated = sum(FY18[1,3]+FY18[2,3]), stringsAsFactors = FALSE)
new17_rowQ3Q4 <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Q3+Q4", 
                            sum_action_obligated = sum(FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
new17_rowall <- data.frame(fiscal_year = "FY18", fiscal_quarter = "Total FY", 
                           sum_action_obligated = sum(FY18[1,3]+FY18[2,3]+FY18[3,3]+FY18[4,3]), stringsAsFactors = FALSE)
FY_17 <- bind_rows(FY18,new17_rowQ1Q2, new17_rowQ3Q4, new17_rowall)  

FY_con_compare <- rbind(FY_17, FY_18)