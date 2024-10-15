# 1) Description --------------------------- --------------------------- ---------------------------
# project: UNHCR: CBP/APP and GBV dashboard (legacy analysis)
# file: data cleaning II
# author: BULLA@unhcr.org/ dominik.bulla@gmail.com
# September 2022 through June 2023



# 2) Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/Dominik/OneDrive - Nexus365/Work/Consulting/UNHCR/CBP APP GBV Data Analysis Dashboard Consultant")  
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())
options(scipen = 999)



# 3) Packages --------------------------- --------------------------- ---------------------------
library(readxl)
library(dplyr)



# 4) Import data --------------------------- --------------------------- ---------------------------

budget <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_budget_20230708.csv")

expenditures <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_expenditures_20230708.csv")
#expendall <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_expenditures_complete_20230625.csv")
indicators <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_indicators_20230708.csv")
staffing <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_staffing_20230708.csv")
staffing_OL <- read.csv("06 Data/01 Legacy Analysis/UNHCR_legacy_staffing_OL_20230714.csv")
operations <-  read_excel("06 Data/Country list as of 2022 reporting_20230216.xlsx", sheet = "LGBTIQ+")
operations2 <-  read_excel("06 Data/CBPGBV Dashboards_Regions_20230625_V01.xlsx", sheet = "Countries")
indicator_overview <- read_excel("06 Data/01 Legacy Analysis/CBPGBV_Legacy_Indicator overview_20230628_V01.xlsx", sheet = "Sheet1") %>%
  select(Indicator, Aggregation)



# 5) The data structure --------------------------- --------------------------- ---------------------------
# comment. As outlined in the deck 'Analysis of the legacy data_20230216', the general structure of the 
# data is as follows: operation > objective > output > indicator 
# level 1) indicator (RBM data only)
# level 2) output (RBM/ budget/ expenditure)
# level 3) objective (RBM/ budget/ expenditure)
# level 4) operation (RBM/ budget/ expenditure/ staffing)
# thus, we need a long dataset with indicator row wise and OP, OL, expenditure column wise

# 6) prepare the budget data --------------------------- --------------------------- ---------------------------

budget <- budget %>%
  select(Operation, Year, PPG, Objective, Output_Name, Budget_Component, Budget_Category, Cost_USD) %>%
  group_by(Operation, Year, Objective, Output_Name, PPG, Budget_Component, Budget_Category) %>%
  dplyr::summarise(USD = sum(Cost_USD, na.rm = TRUE))
budget <- as.data.frame(budget)
budget <- reshape(budget, 
                  idvar = c("Operation", "Year", "PPG",
                            "Objective", "Output_Name", "Budget_Component"), 
                  timevar = "Budget_Category",
                  direction = "wide")
budget <- as.data.frame(budget) %>%
  rename(Admin = USD.UNHCR_ADMIN,
         Internal_project = USD.UNHCR_PROJECT,
         Staff = USD.UNHCR_STAFF,
         External_project = USD.PARTNER_PROJECT) 
budget <- as.data.frame(budget)


budget <- reshape(budget, 
                  idvar = c("Operation", "Year", 
                            "Objective", "Output_Name",
                            "PPG"), 
                  timevar = "Budget_Component",
                  direction = "wide")
budget <- budget %>%
  group_by(Operation, Year, Objective, Output_Name, PPG) %>%
  dplyr::summarise(`Admin.Above Operating Level` = sum(`Admin.Above Operating Level`, na.rm = TRUE), 
                   `Internal_project.Above Operating Level` = sum(`Internal_project.Above Operating Level`, na.rm = TRUE), 
                   `Staff.Above Operating Level` = sum(`Staff.Above Operating Level`, na.rm = TRUE), 
                   `External_project.Above Operating Level` = sum(`External_project.Above Operating Level`, na.rm = TRUE), 
                   `Admin.Operating Level` = sum(`Admin.Operating Level`, na.rm = TRUE), 
                   `Internal_project.Operating Level` = sum(`Internal_project.Operating Level`, na.rm = TRUE),
                   `Staff.Operating Level` = sum(`Staff.Operating Level`, na.rm = TRUE), 
                   `External_project.Operating Level`  = sum(`External_project.Operating Level`, na.rm = TRUE))

budget <- budget %>%
  rename(Admin_AOL = `Admin.Above Operating Level`,
         Internal_project_AOL = `Internal_project.Above Operating Level`,
         Staff_AOL = `Staff.Above Operating Level`,
         External_project_AOL = `External_project.Above Operating Level`,
         Admin_OL = `Admin.Operating Level`,
         Internal_project_OL = `Internal_project.Operating Level`,
         Staff_OL = `Staff.Operating Level`,
         External_project_OL = `External_project.Operating Level`,
         Output = Output_Name) %>% 
  rowwise() %>%
  mutate(Total_AOL = sum(Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, na.rm = TRUE),
         Total_OL = sum(Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, na.rm = TRUE)) %>% 
  mutate(Admin_OP = sum(Admin_AOL, Admin_OL, na.rm = TRUE),
         Internal_project_OP = sum(Internal_project_AOL, Internal_project_OL, na.rm = TRUE),
         Staff_OP = sum(Staff_AOL, Staff_OL, na.rm = TRUE),
         External_project_OP = sum(External_project_AOL, External_project_OL, na.rm = TRUE)) %>%
  mutate(Total_OP = sum(Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, na.rm = TRUE)) %>%
  select(Operation, Year, PPG, Objective, Output,
         Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
         Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL, 
         Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP) 

expenditures <- expenditures %>%
  select(Operation, Year, PPG, Objective, Output, Budget_Category, Expenditures) %>%
  group_by(Operation, Year, PPG, Objective, Output, Budget_Category) %>%
  dplyr::summarise(Expenditures = sum(Expenditures, na.rm = TRUE))
expenditures <- as.data.frame(expenditures)
expenditures <- reshape(expenditures, 
                  idvar = c("Operation", "Year", 
                            "PPG",
                            "Objective", "Output"), 
                  timevar = "Budget_Category",
                  direction = "wide")
expenditures <- as.data.frame(expenditures) %>%
  rename(Admin_Exp = Expenditures.UNHCR_ADMIN,
         Internal_project_Exp = Expenditures.UNHCR_PROJECT,
         Staff_Exp = Expenditures.UNHCR_STAFF,
         External_project_Exp = Expenditures.PARTNER_PROJECT)  
expenditures <- as.data.frame(expenditures)
expenditures <- expenditures %>%
  rowwise() %>%
  mutate(Total_Exp = sum(Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, na.rm = TRUE))  %>%
  select(Operation, Year, PPG, Objective, Output,
         Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp)
expenditures <- as.data.frame(expenditures)
budget <- merge(budget, expenditures, by = c("Operation", "Year", "Objective", "Output", "PPG"), all = TRUE)

budget <- budget %>%
  arrange(Operation, Year, Objective, Output, PPG)
rm(expenditures)

budget <- merge(budget, operations, by.x = "Operation", by.y = "Country")
colnames(budget)[colnames(budget) == "Sub-region"] <- "Sub_region"
budget <- budget %>%
  select(Region, Sub_region, Operation, Year, PPG,
         Objective, Output, 
         Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL, 
         Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL, 
         Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP, 
         Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp)
budget <- budget %>%
  mutate(selected_budget = ifelse(is.na(Total_OL),0,1)) %>%
  mutate(selected_budget = ifelse(Total_OL == 0 ,0, selected_budget))
budget_impact <- budget %>%
  group_by(Operation, Year, Objective, PPG) %>%
  dplyr::summarise(Admin_OL = sum(Admin_OL, na.rm = TRUE), 
                   Staff_OL = sum(Staff_OL, na.rm = TRUE), 
                   Internal_project_OL = sum(Internal_project_OL, na.rm = TRUE), 
                   External_project_OL = sum(External_project_OL, na.rm = TRUE), 
                   Total_OL = sum(Total_OL, na.rm = TRUE), 
                   Admin_AOL = sum(Admin_AOL, na.rm = TRUE), 
                   Staff_AOL = sum(Staff_AOL, na.rm = TRUE), 
                   Internal_project_AOL = sum(Internal_project_AOL, na.rm = TRUE), 
                   External_project_AOL = sum(External_project_AOL, na.rm = TRUE), 
                   Total_AOL = sum(Total_AOL, na.rm = TRUE), 
                   Admin_OP = sum(Admin_OP, na.rm = TRUE), 
                   Staff_OP = sum(Staff_OP, na.rm = TRUE), 
                   Internal_project_OP = sum(Internal_project_OP, na.rm = TRUE), 
                   External_project_OP = sum(External_project_OP, na.rm = TRUE), 
                   Total_OP = sum(Total_OP, na.rm = TRUE), 
                   Admin_Exp = sum(Admin_Exp, na.rm = TRUE), 
                   Staff_Exp = sum(Staff_Exp, na.rm = TRUE), 
                   Internal_project_Exp = sum(Internal_project_Exp, na.rm = TRUE), 
                   External_project_Exp = sum(External_project_Exp, na.rm = TRUE), 
                   Total_Exp = sum(Total_Exp, na.rm = TRUE),
                   selected_budget = max(selected_budget, na.rm = TRUE))



# 7) prepare the staffing [DHR] data --------------------------- --------------------------- ---------------------------

categories <- unique(staffing$Categories)
contracts <- unique(staffing$Contract)
staffing$Gender <- ifelse(staffing$Gender == "F", "1", "0") 
staffing$Gender <- as.numeric(staffing$Gender)
colnames(staffing)[colnames(staffing)== "Gender"] <- "Female"

staffingT <- staffing %>%
  group_by(Operation, Year, Unit) %>%
  dplyr::summarise(Total = n())

for (elem in categories) {
  NEW <- staffing[staffing$Categories == elem,] %>%
    group_by(Operation, Year, Unit) %>%
    dplyr::summarise(Count = n())
  colnames(NEW)[colnames(NEW) == "Count"] <- elem
 assign(paste0("staffing", elem), NEW) 
}

for (elem in contracts) {
  NEW <- staffing[staffing$Contract == elem,] %>%
    group_by(Operation, Year, Unit) %>%
    dplyr::summarise(Count = n())
  colnames(NEW)[colnames(NEW) == "Count"] <- elem
  assign(paste0("staffing", elem), NEW) 
}
rm(categories, contracts, elem, NEW)

staffingFemale <- staffing %>%
  group_by(Operation, Year, Unit) %>%
  dplyr::summarise(Female = sum(Female, na.rm = TRUE))

staffingCount <- merge(staffingT, staffingAWF, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingG, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingJPO, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingN, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingP, by = c("Operation", "Unit", "Year"), all = TRUE)
colnames(staffingAWF)[colnames(staffingAWF) == "AWF"] <- "AWF_contract"
staffingCount <- merge(staffingCount, staffingAFF, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingFTA, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingIND, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingTA, by = c("Operation", "Unit", "Year"), all = TRUE)
staffingCount <- merge(staffingCount, staffingFemale, by = c("Operation", "Unit", "Year"), all = TRUE)

staffingCount <- staffingCount %>%
  arrange(Operation, Unit, Year)

staffingCount$Total[is.na(staffingCount$Total)] <- 0
staffingCount$AWF[is.na(staffingCount$AWF)] <- 0
staffingCount$G[is.na(staffingCount$G)] <- 0
staffingCount$JPO[is.na(staffingCount$JPO)] <- 0
staffingCount$N[is.na(staffingCount$N)] <- 0
staffingCount$P[is.na(staffingCount$P)] <- 0
staffingCount$AFF[is.na(staffingCount$AFF)] <- 0
staffingCount$FTA[is.na(staffingCount$FTA)] <- 0
staffingCount$IND[is.na(staffingCount$IND)] <- 0
staffingCount$TA[is.na(staffingCount$TA)] <- 0
rm(staffingAWF, 
   staffingG, staffingJPO, staffingN, staffingP, 
   staffingAFF, staffingFTA, staffingIND, staffingTA,
   staffingT,staffingFemale,
   staffing)

colnames(staffingCount)[colnames(staffingCount) == "Unit"] <- "Objective"
colnames(staffingCount)[colnames(staffingCount) == "Total"] <- "Total_staff"
staffingCount$Objective[staffingCount$Objective == "GBV"] <- "Risk of SGBV is reduced and quality of response improved"
staffingCount$Objective[staffingCount$Objective == "CBP"] <- "Community mobilization strengthened and expanded"
staffingCount$selected_staffing <- 1



# 8) prepare the RBM data --------------------------- --------------------------- ---------------------------

indicators <- indicators %>%
  select(Operation, Year, PPG,
         Objective, Output,
         Type, Indicator, 
         Baseline, Mid_Year, Year_End, OL_Target, OP_Target)
indicators <- indicators %>%
  mutate(selected_indicators = ifelse(!is.na(OL_Target), 1,0))
indicators$selected_indicators <- 1
indicators_performance <- indicators %>%
  filter(Type == "Performance")
indicators_impact <- indicators %>%
  filter(Type == "Impact")
indicators <- merge(indicators, indicator_overview, by = "Indicator", all.x = TRUE)



# 9) merge data --------------------------- --------------------------- ---------------------------

legacy_performance <- merge(budget, indicators_performance, by = c("Year", "Operation", "Objective", "Output", "PPG"), all = TRUE)
legacy_impact <- merge(budget_impact, indicators_impact, by = c("Year", "Operation", "Objective", "PPG"), all = TRUE)
legacy_impact$Region <- ""
legacy_impact$Sub_region <- ""

table(legacy$PPG, useNA = "always")
write.csv(legacy,"legacy.csv")
write.csv(staffingCount,"staffingCount.csv")
write.csv(legacy,"legacy2.csv")

legacy <- rbind(legacy_performance, legacy_impact)
legacy <- merge(legacy, staffingCount, by = c("Operation", "Objective", "Year"), all = TRUE)
legacy <- legacy %>%
  mutate(Total_staff = ifelse(is.na(Total_staff), 0, Total_staff),	
         AWF = ifelse(is.na(AWF), 0, AWF),	
         G = ifelse(is.na(G), 0, G),	
         JPO = ifelse(is.na(JPO), 0, JPO),	
         N = ifelse(is.na(N), 0, N),	
         P = ifelse(is.na(P), 0, P),
         AFF = ifelse(is.na(AFF), 0, AFF),
         FTA = ifelse(is.na(FTA), 0, FTA),
         IND = ifelse(is.na(IND), 0, IND),
         TA = ifelse(is.na(TA), 0, TA),
         Admin_OL = ifelse(is.na(Admin_OL), 0, Admin_OL),
         Staff_OL = ifelse(is.na(Staff_OL), 0, Staff_OL),
         Internal_project_OL = ifelse(is.na(Internal_project_OL), 0, Internal_project_OL),
         External_project_OL = ifelse(is.na(External_project_OL), 0, External_project_OL),
         Total_OL = ifelse(is.na(Total_OL), 0, Total_OL),
         Admin_AOL = ifelse(is.na(Admin_AOL), 0, Admin_AOL),
         Staff_AOL = ifelse(is.na(Staff_AOL), 0, Staff_AOL),
         Internal_project_AOL = ifelse(is.na(Internal_project_AOL), 0, Internal_project_AOL),
         External_project_AOL = ifelse(is.na(External_project_AOL), 0, External_project_AOL),
         Total_AOL = ifelse(is.na(Total_AOL), 0, Total_AOL),
         Admin_OP = ifelse(is.na(Admin_OP), 0, Admin_OP),
         Staff_OP = ifelse(is.na(Staff_OP), 0, Staff_OP),
         Internal_project_OP = ifelse(is.na(Internal_project_OP), 0, Internal_project_OP),
         External_project_OP = ifelse(is.na(External_project_OP), 0, External_project_OP),
         Total_OP = ifelse(is.na(Total_OP), 0, Total_OP),
         Admin_Exp = ifelse(is.na(Admin_Exp), 0, Admin_Exp),
         Staff_Exp = ifelse(is.na(Staff_Exp), 0, Staff_Exp),
         Internal_project_Exp = ifelse(is.na(Internal_project_Exp), 0, Internal_project_Exp),
         External_project_Exp = ifelse(is.na(External_project_Exp), 0, External_project_Exp),
         Total_Exp = ifelse(is.na(Total_Exp), 0, Total_Exp))
legacy <- legacy %>%
  mutate(selected_budget = ifelse(is.na(selected_budget), 0, selected_budget),
         selected_staffing = ifelse(is.na(selected_staffing), 0, selected_staffing),
         selected_indicators = ifelse(is.na(selected_indicators), 0, selected_indicators))
legacy <- legacy %>%
  select(selected_budget, selected_staffing, selected_indicators,
         Operation, Objective, Output, Year, 
         PPG, 
         Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
         Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL,
         Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP, 
         Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp, 
         Total_staff, Female, AWF, G, JPO,N, P, 
         AFF, FTA, IND, TA,
         Type, Indicator, 
         Baseline, Mid_Year, Year_End, OL_Target, OP_Target) %>%
  mutate(Output = ifelse((is.na(Output) & !is.na(Type)), "Impact", Output))

legacy <- legacy %>%
  mutate(Baseline = ifelse(is.infinite(Baseline), NA, Baseline),
         Mid_Year = ifelse(is.infinite(Mid_Year), NA, Mid_Year),
         Year_End = ifelse(is.infinite(Year_End), NA, Year_End),
         OL_Target = ifelse(is.infinite(OL_Target), NA, OL_Target),
         OP_Target = ifelse(is.infinite(OP_Target), NA, OP_Target))

legacy <- merge(legacy, operations2, by.x = "Operation", by.y = "Alternative spelling", all.x = TRUE)

legacy <- legacy %>%
  select(selected_budget, selected_staffing, selected_indicators,
         `Regional bureau`, Country, 
         Objective, Output, Year, PPG,
         Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
         Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL,
         Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP, 
         Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp,
         Total_staff, Female, AWF, G, JPO, N, P, 
         AFF, FTA, IND, TA,
         Type, Indicator, Baseline, Mid_Year, Year_End, OL_Target, OP_Target) %>%
  filter(!is.na(Country))



# 10) save data --------------------------- --------------------------- ---------------------------

write.csv(staffingCount, "06 Data/01 Legacy Analysis/UNHCR_legacy_staffCount_clean_20230708.csv", row.names = FALSE)
write.csv(budget, "06 Data/01 Legacy Analysis/UNHCR_legacy_budget_clean_20230708.csv", row.names = FALSE)
write.csv(legacy, "06 Data/01 Legacy Analysis/UNHCR_legacy_clean_combined_20230708.csv", row.names = FALSE)


