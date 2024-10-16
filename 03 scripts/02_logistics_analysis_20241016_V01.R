# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of CBP/ APP legacy data (i.e., 2015-2021)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2024-10-14
# Purpose: Peform multi-level logistic regression



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/first-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(unhcrthemes)
library(tidyverse)
library(scales)
library(lubridate)
library(directlabels)
library(lme4)
library(stargazer)
library(sjstats)



# Import data --------------------------- --------------------------- ---------------------------

legacy <- read.csv("02 processed data/legacy_20230708.csv")
legacy_performance_CBP <- read.csv("02 processed data/legacy_CBP_performance_20230708.csv")
legacy_performance_GBV <- read.csv("02 processed data/legacy_GBV_performance_20230708.csv")
legacy_impact_CBP <- read.csv("02 processed data/legacy_CBP_impact_20230708.csv")
legacy_impact_GBV <- read.csv("02 processed data/legacy_GBV_impact_20230708.csv")



# Zero Models to predict ICC  --------------------------- --------------------------- ---------------------------
# It is assumed that data is hierarchical (i.e., performance within country) 
# We check this assumption through the ICC

# GBV
m_0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_impact_GBV, family = "binomial")
summary(m_0)

icc_impact <- m_0@theta[1]^2/ (m_0@theta[1]^2 + (pi^2/3))
round(icc_impact, 3)
# icc_impact = 0.044 
# It is not necessarily hierarchical 

model_GBV_performance_0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial")
summary(model_GBV_performance_0)
icc_performance <- model_GBV_performance_0@theta[1]^2/ (model_GBV_performance_0@theta[1]^2 + (pi^2/3))
round(icc_performance, 3)
# icc_impact = 0.078 
# it is hierarchical 



# CBP

model_CBP_impact_0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_impact_CBP, family = "binomial")
summary(model_CBP_impact_0)
icc_impact <- model_CBP_impact_0@theta[1]^2/ (model_CBP_impact_0@theta[1]^2 + (pi^2/3))
round(icc_impact, 3)
# icc_impact = 0.19
# it is hierarchical 

MCP0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial")
summary(MCP0)
icc_performance <- MCP0@theta[1]^2/ (MCP0@theta[1]^2 + (pi^2/3))
round(icc_performance, 3)
# icc_performance = 0.054
# It is not necessarily hierarchical 

### By and large, data shows some hierarchy. Thus multilevel regression models



# Impact Models [GBV]  --------------------------- --------------------------- ---------------------------

m_1 <- glmer(targetsreached ~ impact01 + impact02 +
              (1 | Country.code), 
              data = legacy_impact_GBV, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(m_1)
anova(m_0, m_1)
# Choose model 1

# To compare model 1 and model 2, we need to make sure they both contain the same data points
m_1.2 <- glmer(targetsreached ~ impact01 + impact02 +
             (1 | Country.code), 
             data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Americas), ], 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))

# To compare model 1 and model 3, we need to make sure they both contain the same data points
m_1.3 <- glmer(targetsreached ~ impact01 + impact02 +
              (1 | Country.code), 
              data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Type.CO), ], 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))

m_2 <- glmer(targetsreached ~ impact01 + impact02 +
               (1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA  || Country.code), 
               data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Americas), ], 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(m_2)
anova(m_1.2, m_2)
# Choose model 1

m_3 <- glmer(targetsreached ~ impact01 + impact02 +
                 (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.code), 
               data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Type.CO), ], 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(m_3)
anova(m_1.3, m_3)
# Choose model 1

m_4 <- glmer(targetsreached ~ impact01 + impact02 +
               Year2 +
               ( 1 | Country.code), data = legacy_impact_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_4)
anova(m_1, m_4)
# Choose model 1

m_5 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_5)
anova(m_1, m_5)
# Choose model 5

m_6 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_6)
anova(m_5, m_6)
# Choose model 5

m_7 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Total_OL_cm + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_7)
anova(m_5, m_7)
# Choose model 5

m_8 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Admin_OL_cm + Staff_OL_cm + Internal_project_OL_cm + External_project_OL_cm + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_8)
anova(m_5, m_8)
# Choose model 5

m_9 <- glmer(targetsreached ~ impact01 + impact02 +
               PPG2 + PPG3 + PPG4 + PPG5 + 
               Internal_project_OL_cm +  
               (1  | Country.code), 
               data = legacy_impact_GBV, 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(m_9)
anova(m_5, m_9)
# Choose model 5

m_10 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Total_staff_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_10)
anova(m_5, m_10)
# Choose model 5

m_11 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
               Category.AWF_cm + Category.G_cm + Category.JPO_cm + Category.N_cm + Category.P_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_11)
anova(m_5, m_11)
# Choose model 5

m_12 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
               Contract.AFF_cm + Contract.FTA_cm + Contract.IND_cm + Contract.TA_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(m_12)
anova(m_5, m_12)
# Choose model 5

stargazer(m_0, 
          m_1, 
          m_4, 
          m_5, 
          m_6, 
          m_7, 
          m_8, 
          m_10,
          m_11,
          m_12, 
          type="html",
          column.labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
          dep.var.labels=c("Impact target reached (yes/no)"),
          model.numbers = FALSE,
          covariate.labels=c("Impact indicator 1" , "Impact indicator 2", "Year",
                             "Stateless persons", "returnees", "Internally displaced", "populations of concern",
                             "Target as %-change over baseline",
                             "Total OL (in USD)", "Admin OL (in USD)", "staff OL (in USD)", 
                             "Int. project OL (in USD)", "Ext. project OL (in USD)", 
                             "# of staff", "# of AWF", "# of G", "# of JPO", "# of N", "# of P", 
                             "# of AFF", "# of FTA", "# of Ind", "# of TA"),
          title = "Multilevel logistics regression results",
          digits = 2,
          out = "04 results/GBV_impact_models_20230710_V01.htm")



# Impact Models [CBP]  --------------------------- --------------------------- ---------------------------

MCI1 <- glmer(targetsreached ~ impact01 + impact03 +
               (1 | Country.code), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MCI1)

MCI1.1 <- glmer(targetsreached ~ impact01 + impact03 +
                 (1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA  || Country.code), 
               data = legacy_impact_CBP, 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(MCI1.1)
anova(MCI1, MCI1.1)

MCI1.2 <- glmer(targetsreached ~ impact01 + impact03 +
                (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.code), 
                data = legacy_impact_CBP, 
                family = "binomial",
                control = glmerControl(optimizer = "bobyqa"))
summary(MCI1.2)
anova(MCI1, MCI1.2)

MCI2 <- glmer(targetsreached ~ impact01 + impact03 +
             Year2 +
             ( 1 | Country.code), data = legacy_impact_CBP, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MCI2)
anova(MCI1, MCI2)

MCI3 <- glmer(targetsreached ~ impact01 + impact03 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             (1  | Country.code), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MCI3)
anova(MCI1, MCI3)

MCI4 <- glmer(targetsreached ~ impact01 + impact03 +
             prop_change_cmc +
             (1  | Country.code), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MCI4)
anova(MCI1, MCI4)

MCI5 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              (1  | Country.code), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCI5)
anova(MCI4, MCI5)

MCI6 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
              (1  | Country.code), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCI6)
anova(MCI5, MCI6)

MCI7 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              Total_staff_cmc +
              (1  | Country.code), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCI7)
anova(MCI5, MCI7)

MCI8 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              AWF_cmc + G_cmc + JPO_cmc + N_cmc + P_cmc +
              (1  | Country.code), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCI8)
anova(MCI5, MCI8)

MCI9 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              AFF_cmc + FTA_cmc + IND_cmc + TA_cmc +
              (1  | Country.code), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCI9)
anova(MCI5, MCI9)

stargazer(model_CBP_impact_0, MCI1, MCI2, MCI3, MCI4, MCI5, MCI6, MCI7, MCI8, MCI9, type="html", 
          dep.var.labels=c("Impact target reached (yes/no)"),
          covariate.labels=c("Impact indicator 1" , "Impact indicator 3", "Year",
                             "Stateless persons", "returnees", "Internally displaced", "populations of concern",
                             "Target as %-change over baseline",
                             "Total OL (in USD)", "Admin OL (in USD)", "staff OL (in USD)", 
                             "Int. project OL (in USD)", "Ext. project OL (in USD)", 
                             "# of staff", "# of AWF", "# of G", "# of JPO", "# of N", "# of P", 
                             "# of AFF", "# of FTA", "# of Ind", "# of TA"),
          title = "Multilevel logistics regression results",
          digits = 2,
          out = "04 results/CBP_impact_models_20230719_V01.htm")





# performance Models [GBV]  --------------------------- --------------------------- ---------------------------

MP1 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             ( 1 | Country.code), 
             data = legacy_performance_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))

MP1.1 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               ( 1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA || Country.code), 
               data = legacy_performance_GBV, 
               family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP1.1)
anova(MP1, MP1.1)

MP1.2 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.code), 
               data = legacy_performance_GBV, 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(MP1.2)
anova(MP1, MP1.2)

MP2 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             Year2 +
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP2)
anova(MP1, MP2)

MP3 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP3)
anova(MP1, MP3)

MP4 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Total_OL_cmc + 
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP4)
anova(MP3, MP4)

MP5 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP5)
anova(MP4, MP5)

MP5.1 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               PPG2 + PPG3 + PPG4 + PPG5 + 
               External_project_OL_cmc + 
               ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(MP5.1)
anova(MP4, MP5.1)

MP6 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             External_project_OL_cmc + 
             Total_staff_cmc +
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MP6)
anova(MP5.1, MP6)

MP7 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             External_project_OL_cmc + 
             AWF_cmc + G_cmc + JPO_cmc + N_cmc + P_cmc +
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(MP7)
anova(MP5.1, MP7)

MP7.1 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               PPG2 + PPG3 + PPG4 + PPG5 + 
               External_project_OL_cmc +
               AWF_cmc + N_cmc + 
               ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
               control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(MP7.1)
anova(MP5.1, MP7.1)

MP8 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             External_project_OL_cmc +
             AFF_cmc + FTA_cmc + IND_cmc + TA_cmc +
             ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(MP8)
anova(MP7.1, MP8)

stargazer(model_GBV_performance_0, MP1, MP2, MP3, MP4, MP5, MP6, MP7, MP7.1, MP8, type="html", 
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(6)", "(7)", "(8)", "(9)", "(9.1)", "(10)"),
          dep.var.labels=c("Performance target reached (yes/no)"),
          model.numbers = FALSE,
          covariate.labels=c("Performance indicator 1" , "Performance indicator 2", "Performance indicator 3",  
                             "Performance indicator 6" , "Performance indicator 7", "Performance indicator 8",
                             "Performance indicator 9" , "Performance indicator 10","Performance indicator 12",
                             "Performance indicator 13" ,"Performance indicator 15","Performance indicator 16",
                             "Performance indicator 17",
                             "Year",
                             "Stateless persons", "returnees", "Internally displaced", "populations of concern",
                             "OL (in USD)", "OL (in USD) allocated to Admin", "OL (in USD) allocated to staff", 
                             "OL (in USD) allocated to internal project", "OL (in USD) allocated to external project", 
                             "# of staff", "# of AWF", "# of G", "# of JPO", "# of N", "# of P", 
                             "# of AFF", "# of FTA", "# of Ind", "# of TA"),
          title = "Multilevel logistics regression results",
          font.size = c("tiny"),
          digits = 2,
          out = "04 results/GBV_performance_models_20230719_V01.htm")



# performance Models [CBP]  --------------------------- --------------------------- ---------------------------

MCP1 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              ( 1 | Country.code), 
              data = legacy_performance_CBP, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))

MCP1.1 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                ( 1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA || Country.code), 
                data = legacy_performance_CBP, 
                family = "binomial",
                control = glmerControl(optimizer = "bobyqa"))
summary(MCP1.1)
anova(MCP1, MCP1.1)

MCP1.2 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.code), 
                data = legacy_performance_CBP, 
                family = "binomial",
                control = glmerControl(optimizer = "bobyqa"))
summary(MCP1.1)
anova(MCP1, MCP1.2)

MCP2 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              Year2 +
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCP2)
anova(MCP1, MCP2)

MCP3 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCP3)
anova(MCP1, MCP3)

MCP4 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              Total_OL_cmc + 
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCP4)
anova(MCP3, MCP4)

MCP5 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(MCP5)
anova(MCP3, MCP5)

MCP5.1 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                PPG2 + PPG3 + PPG4 + PPG5 + 
                External_project_OL_cmc + 
                ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCP5.1)
anova(MCP3, MCP5.1)

MCP6 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              External_project_OL_cmc + 
              Total_staff_cmc +
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(MCP6)
anova(MCP5.1, MCP6)

MCP7 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              External_project_OL_cmc + 
              AWF_cmc + G_cmc + JPO_cmc + N_cmc + P_cmc +
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(MCP7)
anova(MCP5.1, MCP7)

MCP8 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              External_project_OL_cmc + 
              AFF_cmc + FTA_cmc + IND_cmc + TA_cmc +
              ( 1 | Country.code), data = legacy_performance_CBP, family = "binomial",
             control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(MCP8)
anova(MCP5.1, MCP8)

stargazer(MCP0, MCP1, MCP2, MCP3, MCP4, MCP5, MCP5.1, MCP6, MCP7, MCP8, type="html", 
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(6)", "(7)", "(7.1)","(8)", "(9)", "(10)"),
          dep.var.labels=c("Performance target reached (yes/no)"),
          model.numbers = FALSE,
          covariate.labels=c("Performance indicator 1" , "Performance indicator 2", "Performance indicator 3",  
                             "Performance indicator 4" , "Performance indicator 5", "Performance indicator 6",
                             "Performance indicator 7" , "Performance indicator 8", "Performance indicator 9",
                             "Performance indicator 10" , "Performance indicator 11","Performance indicator 12",
                             "Performance indicator 13" ,"Performance indicator 14",
                             "Year",
                             "Stateless persons", "returnees", "Internally displaced", "populations of concern",
                             "OL (in USD)", "OL (in USD) allocated to Admin", "OL (in USD) allocated to staff", 
                             "OL (in USD) allocated to internal project", "OL (in USD) allocated to external project", 
                             "# of staff", "# of AWF", "# of G", "# of JPO", "# of N", "# of P", 
                             "# of AFF", "# of FTA", "# of Ind", "# of TA"),
          title = "Multilevel logistics regression results",
          font.size = c("tiny"),
          digits = 2,
          out = "04 results/CBP_performance_models_20230719_V01.htm")



# Odds-Ratios --------------------------- --------------------------- ---------------------------

# GBV (impact)
# Model 5 best fitting model, which is MI4 

ORI4 <- exp(fixef(MI4))
CII4 <- exp(confint(MI4,parm="beta_",method="Wald"))
OR.CI4 <- rbind(cbind(ORI4, CII4))
OR.CI4 <- round(OR.CI4,2)
colnames(OR.CI4)[1] <- "Odds"
rownames(OR.CI4) <- c("(Intercept)", 
                      "Impact indicator 1 (yes/no)" , "Impact indicator 2 (yes/no)", 
                      "Stateless persons (yes/no)", "returnees (yes/no)", 
                      "Internally displaced (yes/no)", "populations of concern (yes/no)",
                      "Target as %-change over baseline")
performance::r2(m_0)
performance::r2(MI4)
write.csv(OR.CI4, "06 Data/01 Legacy Analysis/0106 Tables/OR.CI4_20230724_V01.csv")



# GBV (performance)
# Model 6 best fitting model, which is MP5 

ORP7.1 <- exp(fixef(MP7.1))
CIP7.1 <- exp(confint(MP7.1,parm="beta_",method="Wald"))
OR.CP7.1 <- rbind(cbind(ORP7.1, CIP7.1))
OR.CP7.1 <- round(OR.CP7.1,2)
colnames(OR.CP7.1)[1] <- "Odds"
rownames(OR.CP7.1) <- c("(Intercept)", 
                      "Performance indicator 1 (yes/no)" ,
                      "Performance indicator 2 (yes/no)" , "Performance indicator 3 (yes/no)",  
                      "Performance indicator 6 (yes/no)" , "Performance indicator 7 (yes/no)", 
                      "Performance indicator 8 (yes/no)", "Performance indicator 9 (yes/no)" , 
                      "Performance indicator 10 (yes/no)","Performance indicator 12 (yes/no)",
                      "Performance indicator 13 (yes/no)" ,"Performance indicator 15 (yes/no)",
                      "Performance indicator 16 (yes/no)", "Performance indicator 17 (yes/no)",
                      "Stateless persons (yes/no)", "returnees (yes/no)", 
                      "Internally displaced (yes/no)", "populations of concern (yes/no)",
                      "OL (in USD)")
performance::r2(model_GBV_performance_0)
performance::r2(MP7.1)
write.csv(OR.CP7.1, "06 Data/01 Legacy Analysis/0106 Tables/OR.CP7.1_20230718_V01.csv")



# CBP (impact)
# Model 5 best fitting model, which is MI4 

ORCI5 <- exp(fixef(MCI5))
CPCII5 <- exp(confint(MCI5,parm="beta_",method="Wald"))
OR.CPI5 <- rbind(cbind(ORCI5,CPCII5))
OR.CPI5 <- round(OR.CPI5,2)
colnames(OR.CPI5)[1] <- "Odds"
rownames(OR.CPI5) <- c("(Intercept)", 
                      "Impact indicator 1 (yes/no)" , "Impact indicator 3 (yes/no)", 
                      "Target as %-change over baseline",
                      "OL (in USD)")
performance::r2(model_CBP_impact_0)
performance::r2(MCI5)
write.csv(OR.CPI5, "06 Data/01 Legacy Analysis/0106 Tables/OR.CI5_CBP_20230718_V01.csv")



# CBP (performance)
# Model 6 best fitting model, which is MP5 

ORCP5.1 <- exp(fixef(MCP5.1))
CICP5.1 <- exp(confint(MCP5.1,parm="beta_",method="Wald"))
OR.CCP5.1 <- rbind(cbind(ORCP5.1,CICP5.1))
OR.CCP5.1 <- round(OR.CCP5.1,2)
colnames(OR.CCP5.1)[1] <- "Odds"
rownames(OR.CCP5.1) <- c("(Intercept)", 
                      "Performance indicator 1 (yes/no)", "Performance indicator 2 (yes/no)" , 
                      "Performance indicator 3 (yes/no)", "Performance indicator 4 (yes/no)" , 
                      "Performance indicator 5 (yes/no)", "Performance indicator 6 (yes/no)" , 
                      "Performance indicator 7 (yes/no)", "Performance indicator 8 (yes/no)", 
                      "Performance indicator 9 (yes/no)", "Performance indicator 10 (yes/no)",
                      "Performance indicator 11 (yes/no)", "Performance indicator 12 (yes/no)",
                      "Performance indicator 13 (yes/no)", "Performance indicator 14 (yes/no)", 
                      "Stateless persons (yes/no)", "returnees (yes/no)", 
                      "Internally displaced (yes/no)", "populations of concern (yes/no)",
                      "OL (in USD) to external projects")
performance::r2(MCP0)
performance::r2(MCP5.1)
write.csv(OR.CCP5.1, "06 Data/01 Legacy Analysis/0106 Tables/OR.CCP5.1_CBP_20230718_V01.csv")



