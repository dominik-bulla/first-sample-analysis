# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of CBP/ APP legacy data (i.e., 2015-2021)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-07-23
# Background: The client was interested to explore the relations between achievement of programmatic targets (i.e., yes/ no) and 
# top-level factors associated with operations across the world. To answer the question, a multilevel-regression analysis 
# was performed. Impact targets refer to two units (i.e., GBV as well as CBP) within the organization. There are also two types 
# of targets, i.e., impact as well as performance (output) targets. The example analysis below is only about GBV impact targets. 
# The other three analyses are not presented here.    
# Purpose: Clean up and make the data set ready for multilevel logistic analysis


# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/first-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(readxl)
library(tidyverse)



# Import data --------------------------- --------------------------- ---------------------------
budget_data <- read.csv("01 raw data/legacy_budget_20230708.csv")
expenditure_data <- read.csv("01 raw data/legacy_expenditures_20230708.csv")
rbm_data <- read.csv("01 raw data/legacy_indicators_20230708.csv")
staffing_data <- read.csv("01 raw data/legacy_staffing_20230708.csv")
staffing_OL <- read.csv("01 raw data/legacy_staffing_OL_20230714.csv")
operations <- read_excel("01 raw data/regions_20230720_V01.xlsx", sheet = "Countries")
indicator_overview <- read_excel("01 raw data/legacy_Indicator overview_20230628_V01.xlsx", sheet = "Sheet1")



# Clean up budget data --------------------------- --------------------------- ---------------------------

# reduce budget data down to variables actually needed for analysis  
budget_data <- budget_data %>%
  select(Operation, Year, PPG, Objective, Output_Name, Budget_Component, Budget_Category, Cost_USD) %>%
  group_by(Operation, Year, Objective, Output_Name, PPG, Budget_Component, Budget_Category) %>%
  dplyr::summarise(USD = sum(Cost_USD, na.rm = TRUE)) %>%
  as.data.frame()

# the budget data is in long format. ie, budget data for an operation is across several rows to cover allocations to
# admin, partner projects, internal projects, and staff. These budget categories will now be allocated across columns. 
budget_data <- reshape(budget_data,
  idvar = c(
    "Operation", "Year", "PPG",
    "Objective", "Output_Name", "Budget_Component"
  ),
  timevar = "Budget_Category",
  direction = "wide"
)

budget_data <- as.data.frame(budget_data) %>%
  rename(
    Admin = USD.UNHCR_ADMIN,
    Internal_project = USD.UNHCR_PROJECT,
    Staff = USD.UNHCR_STAFF,
    External_project = USD.PARTNER_PROJECT
  ) %>%
  as.data.frame()

# In terms of budget components, the budget data is still in long format. ie AOL and OL. Keep in mind, OP = AOL + OL
# These budget components will now be allocated across columns. 
budget_data <- reshape(budget_data,
  idvar = c(
    "Operation", "Year",
    "Objective", "Output_Name",
    "PPG"
  ),
  timevar = "Budget_Component",
  direction = "wide"
)

# Budget data still to granular. The data will therefore be aggregated up to the PPG level.
budget_data <- budget_data %>%
  group_by(Operation, Year, Objective, Output_Name, PPG) %>%
  dplyr::summarise(
    `Admin.Above Operating Level` = sum(`Admin.Above Operating Level`, na.rm = TRUE),
    `Internal_project.Above Operating Level` = sum(`Internal_project.Above Operating Level`, na.rm = TRUE),
    `Staff.Above Operating Level` = sum(`Staff.Above Operating Level`, na.rm = TRUE),
    `External_project.Above Operating Level` = sum(`External_project.Above Operating Level`, na.rm = TRUE),
    `Admin.Operating Level` = sum(`Admin.Operating Level`, na.rm = TRUE),
    `Internal_project.Operating Level` = sum(`Internal_project.Operating Level`, na.rm = TRUE),
    `Staff.Operating Level` = sum(`Staff.Operating Level`, na.rm = TRUE),
    `External_project.Operating Level` = sum(`External_project.Operating Level`, na.rm = TRUE)
  )

# AOL and OL as well as OP (i.e., AOL + OL) data will be summed up across budget categories
budget_data <- budget_data %>%
  rename(
    Admin_AOL = `Admin.Above Operating Level`,
    Internal_project_AOL = `Internal_project.Above Operating Level`,
    Staff_AOL = `Staff.Above Operating Level`,
    External_project_AOL = `External_project.Above Operating Level`,
    Admin_OL = `Admin.Operating Level`,
    Internal_project_OL = `Internal_project.Operating Level`,
    Staff_OL = `Staff.Operating Level`,
    External_project_OL = `External_project.Operating Level`,
    Output = Output_Name
  ) %>%
  rowwise() %>%
  mutate(
    Total_AOL = sum(Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, na.rm = TRUE),
    Total_OL = sum(Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, na.rm = TRUE)
  ) %>%
  mutate(
    Admin_OP = sum(Admin_AOL, Admin_OL, na.rm = TRUE),
    Internal_project_OP = sum(Internal_project_AOL, Internal_project_OL, na.rm = TRUE),
    Staff_OP = sum(Staff_AOL, Staff_OL, na.rm = TRUE),
    External_project_OP = sum(External_project_AOL, External_project_OL, na.rm = TRUE)
  ) %>%
  mutate(Total_OP = sum(Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, na.rm = TRUE)) %>%
  select(
    Operation, Year, PPG, Objective, Output,
    Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
    Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL,
    Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP
  )



# Clean up expenditure data --------------------------- --------------------------- ---------------------------

# reduce expenditure data down to variables actually needed for analysis  
expenditure_data <- expenditure_data %>%
  select(Operation, Year, PPG, Objective, Output, Budget_Category, Expenditures) %>%
  group_by(Operation, Year, PPG, Objective, Output, Budget_Category) %>%
  dplyr::summarise(Expenditures = sum(Expenditures, na.rm = TRUE)) %>%
  as.data.frame()

# the expenditure data is in long format. ie, budget data for an operation is across several rows to cover allocations 
# to admin, partner projects, internal projects, and staff. These budget categories will now be allocated across 
# columns. 
expenditure_data <- reshape(expenditure_data,
  idvar = c(
    "Operation", "Year",
    "PPG",
    "Objective", "Output"
  ),
  timevar = "Budget_Category",
  direction = "wide"
)
expenditure_data <- expenditure_data %>%
  rename(
    Admin_Exp = Expenditures.UNHCR_ADMIN,
    Internal_project_Exp = Expenditures.UNHCR_PROJECT,
    Staff_Exp = Expenditures.UNHCR_STAFF,
    External_project_Exp = Expenditures.PARTNER_PROJECT
  )

# Create a total sum of expenditures
expenditure_data <- expenditure_data %>%
  rowwise() %>%
  mutate(Total_Exp = sum(Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, na.rm = TRUE)) %>%
  select(
    Operation, Year, PPG, Objective, Output,
    Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp
  )



# Merge budget and expenditure data --------------------------- --------------------------- ---------------------------
budget_data <- merge(budget_data, expenditure_data, by = c("Operation", "Year", "Objective", "Output", "PPG"), all = TRUE)
rm(expenditure_data)

budget_data <- budget_data %>%
  arrange(Operation, Year, Objective, Output, PPG)

# Ensuring that we use official names of countries and operations
budget_data <- merge(budget_data, operations, by.x = "Operation", by.y = "Alternative spelling")
budget_data <- budget_data %>%
  select(
    Region, `Sub region`, `Regional bureau`, Country, Type, 
    Year, PPG, 
    Objective, Output,
    Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
    Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL,
    Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP,
    Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp
  ) %>%
  rename("office.type" = "Type")


# Create dummies for whether or not country has allocated any budget
budget_data <- budget_data %>%
  mutate(budget_allocated = ifelse(is.na(Total_OL), 0, 1)) %>%
  mutate(budget_allocated = ifelse(!is.na(Total_OL) & Total_OL == 0, 0, budget_allocated)) 

# We get impact-level data by aggregating budget data beyond output data  
budget_impact <- budget_data %>%
  group_by(Country, Year, Objective, PPG) %>%
  dplyr::summarise(
    Admin_OL = sum(Admin_OL, na.rm = TRUE),
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
    budget_allocated = max(budget_allocated, na.rm = TRUE)
  ) %>%
  as.data.frame()
budget_data2 <- budget_data[, c("Region", "Sub region", "Regional bureau", "Country", "office.type")] %>% 
  distinct(Country, .keep_all = TRUE)
budget_impact <- merge(budget_data2, budget_impact, by  = "Country", all.y = TRUE)
rm(budget_data2)



# Clean up  staffing data --------------------------- --------------------------- ---------------------------

staffing_data <- staffing_data %>%
  rename("Objective" = Unit) %>%
  mutate(Objective = ifelse(Objective == "GBV", "Risk of SGBV is reduced and quality of response improved", Objective)) %>%
  mutate(Objective = ifelse(Objective == "CBP", "Community mobilization strengthened and expanded", Objective)) 

# Create summary stats on how many staff members per unit
staffing_data_total <- staffing_data %>%
  group_by(Operation, Year, Objective) %>%
  dplyr::summarise(`Total_staff` = n())

staffing_data_categories <- staffing_data %>%
  group_by(Operation, Year, Objective, Categories) %>%
  dplyr::summarise(Category = n()) %>%
  as.data.frame()

staffing_data_categories  <- reshape(staffing_data_categories,
                                     idvar = c("Operation", "Objective", "Year"),
                                     timevar = "Categories",
                                     v.names = "Category", 
                                     direction = "wide")
  
staffing_data_contract <- staffing_data %>%
  group_by(Operation, Year, Objective, Contract) %>%
  rename("Contract2" = Contract) %>%
  dplyr::summarise(Contract = n()) %>%
  as.data.frame()

staffing_data_contract  <- reshape(staffing_data_contract,
                                  idvar = c("Operation", "Objective", "Year"),
                                  timevar = "Contract2",
                                  v.names = "Contract", 
                                  direction = "wide")

staffing_data_female <- staffing_data %>%
  mutate(Gender = ifelse(Gender == "F", 1, 0)) %>%
  group_by(Operation, Year, Objective) %>%
  dplyr::summarise(Female = sum(Gender, na.rm = TRUE))

staffing_data <- list(staffing_data_categories, staffing_data_contract, staffing_data_female, staffing_data_total)
staffing_data <- staffing_data %>% 
  reduce(full_join, by = c("Operation", "Year", "Objective")) %>%
    mutate(across(starts_with("C"), ~ case_when(is.na(.)  ~ 0,
                                                .default = as.numeric(.)))) %>%
  mutate(staff_allocated = ifelse(Total_staff > 0 , 1, 0)) 

staffing_data <- merge(staffing_data, operations[, c("Country", "Alternative spelling")], by.x = "Operation", by.y = "Alternative spelling")
staffing_data <- staffing_data %>%
  select(-c(Operation)) 
rm(operations, staffing_data_categories, staffing_data_contract, staffing_data_female, staffing_data_total, staffing_OL)



# Clean up RBM data --------------------------- --------------------------- ---------------------------
# RBM stands for results-based measurement

# Incorporate indicator meta-data, including forms of aggregation. There are 3 types of indicators:
# 1) indicators that are aggregated through averaging across sub-units 
# 2) indicators that are aggregated through choosing the maximum value 
# 3) indicators that are aggregated through summing up sub-units



rbm_data <- merge(rbm_data, indicator_overview, by = c("Objective", "Type", "Output", "Indicator"), all.x = TRUE)
rm(indicator_overview)
rbm_data <- rbm_data %>%
  select(Objective, Type, Output, Output.code, Indicator, indicator.code, Aggregation,
         Operation, Year, PPG, 
         Baseline, Mid_Year, Year_End, OL_Target, OP_Target) %>%
  mutate(indicators_chosen = ifelse(!is.na(OL_Target), 1, 0)) %>%
  rename("indicator.type" = Type,
         "Country" = Operation)

# Create an indicator to determine the size of the end-of-year targets in relation to baseline
# only for impact data possible
rbm_data <- rbm_data %>%
  mutate(prop_change = round(((OL_Target - Baseline)/Baseline), 4)*100) %>%
  mutate(prop_change = ifelse(is.infinite(prop_change), NA, prop_change)) %>%
  mutate(prop_change = ifelse((Indicator == "# of reported incidents of SGBV" & prop_change < 0), prop_change *-1, prop_change))

# create dummy whether or not indicator targets have been reached; i.e., the dependent variable in the logistic regression analysis 
rbm_data <- rbm_data %>%
  mutate(targetsreached = ifelse(((Year_End - OL_Target) < 0) &  Indicator != "# of reported incidents of SGBV", 0, 1)) %>%
  mutate(targetsreached = ifelse(Indicator == "# of reported incidents of SGBV", 0, targetsreached)) %>%
  mutate(targetsreached = ifelse(((Year_End - OL_Target) <= 0) &  Indicator == "# of reported incidents of SGBV", 1, targetsreached)) %>%
  mutate(targetsreached = ifelse(((Year_End - OL_Target) > 0) &  Indicator == "# of reported incidents of SGBV", 0, targetsreached))

# Divide RBM data into impact and performance data 
rbm_data_performance <- rbm_data %>%
  filter(indicator.type == "Performance")
rbm_data_impact <- rbm_data %>%
  filter(indicator.type == "Impact")



# Merge data to create combined legacy data --------------------------- --------------------------- ---------------------------
# The combined data will be divided into impact-related and performance-related data

legacy_performance <- list(budget_data, rbm_data_performance)
legacy_performance <- legacy_performance %>% 
  reduce(full_join, by = c("Year", "Country", "Objective", "Output", "PPG"))

legacy_impact <- list(budget_impact, rbm_data_impact)
legacy_impact <- legacy_impact %>% 
  reduce(full_join, by = c("Year", "Country", "Objective", "PPG")) %>%
  mutate(Output = "")
rm(budget_impact, rbm_data_impact, rbm_data_performance)

legacy <- rbind(legacy_performance, legacy_impact)
rm(legacy_performance, legacy_impact)
legacy <- merge(legacy, staffing_data, by = c("Country", "Objective", "Year"), all = TRUE)



# Clean up legacy data --------------------------- --------------------------- ---------------------------

legacy <- legacy %>%
  mutate(
    Total_staff = ifelse(is.na(Total_staff), 0, Total_staff),
    Category.AWF = ifelse(is.na(Category.AWF), 0, Category.AWF),
    Category.G = ifelse(is.na(Category.G), 0, Category.G),
    Category.JPO = ifelse(is.na(Category.JPO), 0, Category.JPO),
    Category.N = ifelse(is.na(Category.N), 0, Category.N),
    Category.P = ifelse(is.na(Category.P), 0, Category.P),
    Contract.AFF = ifelse(is.na(Contract.AFF), 0, Contract.AFF),
    Contract.FTA = ifelse(is.na(Contract.FTA), 0, Contract.FTA),
    Contract.IND = ifelse(is.na(Contract.IND), 0, Contract.IND),
    Contract.TA = ifelse(is.na(Contract.TA), 0, Contract.TA),
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
    Total_Exp = ifelse(is.na(Total_Exp), 0, Total_Exp)
  )

legacy <- legacy %>%
  mutate(
    budget_allocated = ifelse(is.na(budget_allocated), 0, budget_allocated),
    staff_allocated = ifelse(is.na(staff_allocated), 0, staff_allocated),
    indicators_chosen = ifelse(is.na(indicators_chosen), 0, indicators_chosen)
  )

legacy <- legacy %>%
  mutate(
    Baseline = ifelse(is.infinite(Baseline), NA, Baseline),
    Mid_Year = ifelse(is.infinite(Mid_Year), NA, Mid_Year),
    Year_End = ifelse(is.infinite(Year_End), NA, Year_End),
    OL_Target = ifelse(is.infinite(OL_Target), NA, OL_Target),
    OP_Target = ifelse(is.infinite(OP_Target), NA, OP_Target)
  )

# Create numeric country codes 
country_codes <- as.data.frame(matrix( nrow = length(unique(legacy$Country)),ncol = 2))
colnames(country_codes) <- c("Country", "Country.code")
country_codes$Country <- unique(legacy$Country)
country_codes$Country.code <- 1:length(unique(legacy$Country))
legacy <- merge(legacy, country_codes, by = "Country")
rm(country_codes)

legacy <- legacy %>%
  select(
    budget_allocated, staff_allocated, indicators_chosen,
    `Regional bureau`, Region, `Sub region`, Country, Country.code, office.type,
    Objective, Output, Output.code, Year, PPG,
    Admin_OL, Staff_OL, Internal_project_OL, External_project_OL, Total_OL,
    Admin_AOL, Staff_AOL, Internal_project_AOL, External_project_AOL, Total_AOL,
    Admin_OP, Staff_OP, Internal_project_OP, External_project_OP, Total_OP,
    Admin_Exp, Staff_Exp, Internal_project_Exp, External_project_Exp, Total_Exp,
    Total_staff, Female, Category.AWF, Category.G, Category.JPO, Category.N, Category.P,
    Contract.AFF, Contract.FTA, Contract.IND, Contract.TA,
    indicator.type, Indicator, indicator.code, Aggregation, Baseline, Mid_Year, Year_End, 
    OL_Target, OP_Target, prop_change, targetsreached) %>%
  filter(!is.na(Country)) %>%
  mutate(Output = ifelse((is.na(Output) & !is.na(indicator.type)), "Impact", Output))



# Add dummy variables for the logistics regression models --------------------------- --------------------------- ---------------------------

legacy <- legacy %>%
  mutate(Americas = factor(ifelse(`Regional bureau` == "Americas" , 0.5, -0.5)),
         Asia = factor(ifelse(`Regional bureau` == "Asia and the Pacific" , 0.5, -0.5)),
         EHAGL = factor(ifelse(`Regional bureau` == "East and Horn of Africa and Great Lakes" , 0.5, -0.5)),
         Europe = factor(ifelse(`Regional bureau` == "Europe" , 0.5, -0.5)),
         MENA = factor(ifelse(`Regional bureau` == "Middle East and North Africa" , 0.5, -0.5)),
         NAmerica = factor(ifelse(`Regional bureau` == "Northern America" , 0.5, -0.5)),
         SAfrica = factor(ifelse(`Regional bureau` == "Southern Africa" , 0.5, -0.5)),
         WCA = factor(ifelse(`Regional bureau` == "West and Central Africa" , 0.5, -0.5)),
         Year2 = Year - 2015,
         Year2sq = (Year - 2015)^2,
         impact00 = factor(ifelse(indicator.type == "Impact" & indicator.code == 0, 0.5, -0.5)),
         impact01 = factor(ifelse(indicator.type == "Impact" & indicator.code == 1, 0.5, -0.5)),
         impact02 = factor(ifelse(indicator.type == "Impact" & indicator.code == 2, 0.5, -0.5)),
         impact03 = factor(ifelse(indicator.type == "Impact" & indicator.code == 3, 0.5, -0.5)),
         impact03 = factor(ifelse(indicator.type == "Impact" & indicator.code == 3, 0.5, -0.5)),
         output00 = factor(ifelse(Output.code == 0, 0.5, -0.5)),
         output01 = factor(ifelse(Output.code == 1, 0.5, -0.5)),
         output02 = factor(ifelse(Output.code == 2, 0.5, -0.5)),
         output03 = factor(ifelse(Output.code == 3, 0.5, -0.5)),
         output04 = factor(ifelse(Output.code == 4, 0.5, -0.5)),
         output05 = factor(ifelse(Output.code == 5, 0.5, -0.5)),
         output06 = factor(ifelse(Output.code == 6, 0.5, -0.5)),
         output07 = factor(ifelse(Output.code == 7, 0.5, -0.5)),
         output08 = factor(ifelse(Output.code == 8, 0.5, -0.5)),
         performance01 = factor(ifelse(indicator.type == "Performance" & indicator.code == 1, 0.5, -0.5)),
         performance02 = factor(ifelse(indicator.type == "Performance" & indicator.code == 2, 0.5, -0.5)),
         performance03 = factor(ifelse(indicator.type == "Performance" & indicator.code == 3, 0.5, -0.5)),
         performance04 = factor(ifelse(indicator.type == "Performance" & indicator.code == 4, 0.5, -0.5)),
         performance05 = factor(ifelse(indicator.type == "Performance" & indicator.code == 5, 0.5, -0.5)),
         performance06 = factor(ifelse(indicator.type == "Performance" & indicator.code == 6, 0.5, -0.5)),
         performance07 = factor(ifelse(indicator.type == "Performance" & indicator.code == 7, 0.5, -0.5)),
         performance08 = factor(ifelse(indicator.type == "Performance" & indicator.code == 8, 0.5, -0.5)),
         performance09 = factor(ifelse(indicator.type == "Performance" & indicator.code == 9, 0.5, -0.5)),
         performance10 = factor(ifelse(indicator.type == "Performance" & indicator.code == 10, 0.5, -0.5)),
         performance11 = factor(ifelse(indicator.type == "Performance" & indicator.code == 11, 0.5, -0.5)),
         performance12 = factor(ifelse(indicator.type == "Performance" & indicator.code == 12, 0.5, -0.5)),
         performance13 = factor(ifelse(indicator.type == "Performance" & indicator.code == 13, 0.5, -0.5)),
         performance14 = factor(ifelse(indicator.type == "Performance" & indicator.code == 14, 0.5, -0.5)),
         performance15 = factor(ifelse(indicator.type == "Performance" & indicator.code == 15, 0.5, -0.5)),
         performance16 = factor(ifelse(indicator.type == "Performance" & indicator.code == 16, 0.5, -0.5)),
         performance17 = factor(ifelse(indicator.type == "Performance" & indicator.code == 17, 0.5, -0.5)),
         PPG1 = factor(ifelse(PPG == 1, 0.5, -0.5)),
         PPG2 = factor(ifelse(PPG == 2, 0.5, -0.5)),
         PPG3 = factor(ifelse(PPG == 3, 0.5, -0.5)),
         PPG4 = factor(ifelse(PPG == 4, 0.5, -0.5)),
         PPG5 = factor(ifelse(PPG == 5, 0.5, -0.5)),
         Type.CO = ifelse(office.type == "Country office", 1, 0),
         Type.LO = ifelse(office.type == "Liaison office", 1, 0),
         Type.MCO = ifelse(office.type == "Multi-country office", 1, 0),
         Type.NO = ifelse(office.type == "National office", 1, 0),
         Type.Oocm = ifelse(office.type == "Office of chief of mission", 1, 0),
         Type.P = ifelse(office.type == "Presence", 1, 0))



# Create GBV-specific subsets --------------------------- --------------------------- ---------------------------
# It is assumed that data is hierarchical (i.e., performance within country) 
# A multilevel logistic  regression model is therefore necessary

legacy_GBV <- legacy %>%
  filter(Objective == "Risk of SGBV is reduced and quality of response improved")

# First, we create an impact-specific data set
legacy_impact_GBV <- legacy_GBV %>%
  filter(indicator.type == "Impact")

# It is necessary to remove those countries that have less than three data points 
country_impact <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(delete = ifelse(Count < 3, 1,0))

legacy_impact_GBV <- merge(legacy_impact_GBV, country_impact, by = "Country") %>%
  filter(delete == 0)

# Second, we create a performance-specific data set
legacy_performance_GBV <- legacy_GBV %>%
  filter(indicator.type == "Performance")

# It is necessary to remove those countries that have less than three data points 
country_performance <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(delete = ifelse(Count < 3, 1,0))

legacy_performance_GBV <- merge(legacy_performance_GBV, country_performance, by = "Country") %>%
  filter(delete == 0)

# we also remove those impact indicators that have less than 30 data points
table(legacy_performance_GBV$indicator.code, useNA = "always")
which(table(legacy_performance_GBV$indicator.code) < 30)
legacy_performance_GBV <- legacy_performance_GBV %>%
  filter(indicator.code !=   4) %>%
  filter(indicator.code !=   5) %>%
  filter(indicator.code !=  11) %>%
  filter(indicator.code !=  14)



# Create CBP-specific subsets --------------------------- --------------------------- ---------------------------
# It is assumed that data is hierarchical (i.e., performance within country) 
# A multilevel logistic  regression model is therefore necessary

legacy_CBP <- legacy %>%
  filter(Objective == "Community mobilization strengthened and expanded")
length(unique(legacy_CBP$Country))

# First, we create an impact-specific data set
legacy_impact_CBP <- legacy_CBP %>%
  filter(indicator.type == "Impact")

# It is necessary to remove those countries that have less than three data points 
country_impact <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(delete = ifelse(Count < 3, 1,0))

legacy_impact_CBP <- merge(legacy_impact_CBP, country_impact, by = "Country") %>%
  filter(delete == 0) %>%
  select(-c(Count, delete))



# we also remove those impact indicators that have less than 30 data points
table(legacy_impact_CBP$indicator.code, useNA = "always")
which(table(legacy_impact_CBP$indicator.code) < 30)
legacy_impact_CBP <- legacy_impact_CBP %>%
  filter(indicator.code !=   2)

# Second, we create a performance-specific data set
legacy_performance_CBP <- legacy_CBP %>%
  filter(indicator.type == "Performance")

country_performance <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(Count = n()) %>%
  mutate(delete = ifelse(Count < 3, 1,0))
legacy_performance_CBP <- merge(legacy_performance_CBP, country_performance, by = "Country") %>%
  filter(delete == 0) %>%
  select(-c(Count, delete))

# we also remove those impact indicators that have less than 30 data points
table(legacy_performance_CBP$indicator.code, useNA = "always")
which(table(legacy_performance_CBP$indicator.code) < 30)

rm(country_impact, country_performance)

# Cluster-mean centering --------------------------- --------------------------- ---------------------------
# we apply grand mean centering

# GBV
cm <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(Total_OL_cm = mean(Total_OL, na.rm = TRUE),
                   Admin_OL_cm = mean(Admin_OL, na.rm = TRUE),
                   Staff_OL_cm = mean(Staff_OL, na.rm = TRUE),
                   External_project_OL_cm = mean(External_project_OL, na.rm = TRUE),
                   Internal_project_OL_cm = mean(Internal_project_OL, na.rm = TRUE),
                   prop_change_cm = mean(prop_change, na.rm = TRUE),
                   Total_staff_cm = mean(Total_staff, na.rm = TRUE),
                   Category.AWF_cm = mean(Category.AWF, na.rm = TRUE),
                   Category.G_cm = mean(Category.G, na.rm = TRUE),
                   Category.JPO_cm = mean(Category.JPO, na.rm = TRUE),
                   Category.N_cm = mean(Category.N, na.rm = TRUE),
                   Category.P_cm = mean(Category.P, na.rm = TRUE),
                   Contract.AFF_cm = mean(Contract.AFF, na.rm = TRUE),
                   Contract.FTA_cm = mean(Contract.FTA, na.rm = TRUE),
                   Contract.IND_cm = mean(Contract.IND, na.rm = TRUE),
                   Contract.TA_cm = mean(Contract.TA, na.rm = TRUE)) 
legacy_impact_GBV <- merge(legacy_impact_GBV, cm, by = "Country") 

for (elem in 2 : ncol(cm)) {
  colnames(legacy_impact_GBV)[colnames(legacy_impact_GBV) == colnames(cm)[elem]] <- "var" 
  colnames(legacy_impact_GBV)[colnames(legacy_impact_GBV) == gsub("_cm", "", colnames(cm)[elem])] <- "var2"
  legacy_impact_GBV <- legacy_impact_GBV %>%
    mutate(var = var2 - var) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  legacy_impact_GBV <- legacy_impact_GBV %>%
    group_by(Country) %>%
    mutate(var = scale(var)) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  colnames(legacy_impact_GBV)[colnames(legacy_impact_GBV) == "var"] <- colnames(cm)[elem]
  colnames(legacy_impact_GBV)[colnames(legacy_impact_GBV) == "var2"] <- gsub("_cm", "", colnames(cm)[elem])
}



cm <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(Total_OL_cm = mean(Total_OL, na.rm = TRUE),
                   Admin_OL_cm = mean(Admin_OL, na.rm = TRUE),
                   Staff_OL_cm = mean(Staff_OL, na.rm = TRUE),
                   External_project_OL_cm = mean(External_project_OL, na.rm = TRUE),
                   Internal_project_OL_cm = mean(Internal_project_OL, na.rm = TRUE),
                   prop_change_cm = mean(prop_change, na.rm = TRUE),
                   Total_staff_cm = mean(Total_staff, na.rm = TRUE),
                   Category.AWF_cm = mean(Category.AWF, na.rm = TRUE),
                   Category.G_cm = mean(Category.G, na.rm = TRUE),
                   Category.JPO_cm = mean(Category.JPO, na.rm = TRUE),
                   Category.N_cm = mean(Category.N, na.rm = TRUE),
                   Category.P_cm = mean(Category.P, na.rm = TRUE),
                   Contract.AFF_cm = mean(Contract.AFF, na.rm = TRUE),
                   Contract.FTA_cm = mean(Contract.FTA, na.rm = TRUE),
                   Contract.IND_cm = mean(Contract.IND, na.rm = TRUE),
                   Contract.TA_cm = mean(Contract.TA, na.rm = TRUE)) 
legacy_performance_GBV <- merge(legacy_performance_GBV, cm, by = "Country") 

for (elem in 2 : ncol(cm)) {
  colnames(legacy_performance_GBV)[colnames(legacy_performance_GBV) == colnames(cm)[elem]] <- "var" 
  colnames(legacy_performance_GBV)[colnames(legacy_performance_GBV) == gsub("_cm", "", colnames(cm)[elem])] <- "var2"
  legacy_performance_GBV <- legacy_performance_GBV %>%
    mutate(var = var2 - var) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  legacy_performance_GBV <- legacy_performance_GBV %>%
    group_by(Country) %>%
    mutate(var = scale(var)) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  colnames(legacy_performance_GBV)[colnames(legacy_performance_GBV) == "var"] <- colnames(cm)[elem]
  colnames(legacy_performance_GBV)[colnames(legacy_performance_GBV) == "var2"] <- gsub("_cm", "", colnames(cm)[elem])
}



# CBP
cm <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(Total_OL_cm = mean(Total_OL, na.rm = TRUE),
                   Admin_OL_cm = mean(Admin_OL, na.rm = TRUE),
                   Staff_OL_cm = mean(Staff_OL, na.rm = TRUE),
                   External_project_OL_cm = mean(External_project_OL, na.rm = TRUE),
                   Internal_project_OL_cm = mean(Internal_project_OL, na.rm = TRUE),
                   prop_change_cm = mean(prop_change, na.rm = TRUE),
                   Total_staff_cm = mean(Total_staff, na.rm = TRUE),
                   Category.AWF_cm = mean(Category.AWF, na.rm = TRUE),
                   Category.G_cm = mean(Category.G, na.rm = TRUE),
                   Category.JPO_cm = mean(Category.JPO, na.rm = TRUE),
                   Category.N_cm = mean(Category.N, na.rm = TRUE),
                   Category.P_cm = mean(Category.P, na.rm = TRUE),
                   Contract.AFF_cm = mean(Contract.AFF, na.rm = TRUE),
                   Contract.FTA_cm = mean(Contract.FTA, na.rm = TRUE),
                   Contract.IND_cm = mean(Contract.IND, na.rm = TRUE),
                   Contract.TA_cm = mean(Contract.TA, na.rm = TRUE)) 
legacy_impact_CBP <- merge(legacy_impact_CBP, cm, by = "Country") 

for (elem in 2 : ncol(cm)) {
  colnames(legacy_impact_CBP)[colnames(legacy_impact_CBP) == colnames(cm)[elem]] <- "var" 
  colnames(legacy_impact_CBP)[colnames(legacy_impact_CBP) == gsub("_cm", "", colnames(cm)[elem])] <- "var2"
  legacy_impact_CBP <- legacy_impact_CBP %>%
    mutate(var = var2 - var) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  legacy_impact_CBP <- legacy_impact_CBP %>%
    group_by(Country) %>%
    mutate(var = scale(var)) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  colnames(legacy_impact_CBP)[colnames(legacy_impact_CBP) == "var"] <- colnames(cm)[elem]
  colnames(legacy_impact_CBP)[colnames(legacy_impact_CBP) == "var2"] <- gsub("_cm", "", colnames(cm)[elem])
}



cm <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(Total_OL_cm = mean(Total_OL, na.rm = TRUE),
                   Admin_OL_cm = mean(Admin_OL, na.rm = TRUE),
                   Staff_OL_cm = mean(Staff_OL, na.rm = TRUE),
                   External_project_OL_cm = mean(External_project_OL, na.rm = TRUE),
                   Internal_project_OL_cm = mean(Internal_project_OL, na.rm = TRUE),
                   prop_change_cm = mean(prop_change, na.rm = TRUE),
                   Total_staff_cm = mean(Total_staff, na.rm = TRUE),
                   Category.AWF_cm = mean(Category.AWF, na.rm = TRUE),
                   Category.G_cm = mean(Category.G, na.rm = TRUE),
                   Category.JPO_cm = mean(Category.JPO, na.rm = TRUE),
                   Category.N_cm = mean(Category.N, na.rm = TRUE),
                   Category.P_cm = mean(Category.P, na.rm = TRUE),
                   Contract.AFF_cm = mean(Contract.AFF, na.rm = TRUE),
                   Contract.FTA_cm = mean(Contract.FTA, na.rm = TRUE),
                   Contract.IND_cm = mean(Contract.IND, na.rm = TRUE),
                   Contract.TA_cm = mean(Contract.TA, na.rm = TRUE)) 
legacy_performance_CBP <- merge(legacy_performance_CBP, cm, by = "Country") 

for (elem in 2 : ncol(cm)) {
  colnames(legacy_performance_CBP)[colnames(legacy_performance_CBP) == colnames(cm)[elem]] <- "var" 
  colnames(legacy_performance_CBP)[colnames(legacy_performance_CBP) == gsub("_cm", "", colnames(cm)[elem])] <- "var2"
  legacy_performance_CBP <- legacy_performance_CBP %>%
    mutate(var = var2 - var) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  legacy_performance_CBP <- legacy_performance_CBP %>%
    group_by(Country) %>%
    mutate(var = scale(var)) %>%
    mutate(var = ifelse(is.na(var), 0, var))
  colnames(legacy_performance_CBP)[colnames(legacy_performance_CBP) == "var"] <- colnames(cm)[elem]
  colnames(legacy_performance_CBP)[colnames(legacy_performance_CBP) == "var2"] <- gsub("_cm", "", colnames(cm)[elem])
}
rm(cm, elem)



# Save data --------------------------- --------------------------- ---------------------------

# All data sets are saved even though only the GBV impact data set is required.
write.csv(legacy, "02 processed data/legacy_20230708.csv", row.names = FALSE)
write.csv(legacy_performance_CBP, "02 processed data/legacy_CBP_performance_20230708.csv", row.names = FALSE)
write.csv(legacy_performance_GBV, "02 processed data/legacy_GBV_performance_20230708.csv", row.names = FALSE)
write.csv(legacy_impact_CBP, "02 processed data/legacy_CBP_impact_20230708.csv", row.names = FALSE)
write.csv(legacy_impact_GBV, "02 processed data/legacy_GBV_impact_20230708.csv", row.names = FALSE)


