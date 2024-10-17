# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of CBP/ APP legacy data (i.e., 2015-2021)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-07-23
# Background: The client was interested to explore the relations between achievement of programmatic targets (i.e., yes/ no) and 
# top-level factors associated with operations across the world. To answer the question, a multilevel-regression analysis 
# was performed. Impact targets refer to two units (i.e., GBV as well as CBP) within the organization. There are also two types 
# of targets, i.e., impact as well as performance (output) targets. The example analysis below is only about GBV impact targets. 
# The other three analyses are not presented here.    
# Purpose of script: to perform the multilevel logistic analysis of the cleaned-up data.



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
Gi0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_impact_GBV, family = "binomial")
summary(Gi0)

icc_impact <- Gi0@theta[1]^2/ (Gi0@theta[1]^2 + (pi^2/3))
round(icc_impact, 3)
# icc_impact = 0.044 
# It is not necessarily hierarchical 

gp0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_performance_GBV, family = "binomial")
summary(gp0)
icc_performance <- gp0@theta[1]^2/ (gp0@theta[1]^2 + (pi^2/3))
round(icc_performance, 3)
# icc_impact = 0.078 
# it is hierarchical 



# CBP

ci0 <- glmer(targetsreached ~ ( 1 | Country.code), data = legacy_impact_CBP, family = "binomial")
summary(ci0)
icc_impact <- ci0@theta[1]^2/ (ci0@theta[1]^2 + (pi^2/3))
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



# Impact Models [Impact] [GBV] --------------------------- --------------------------- ---------------------------

Gi1 <- glmer(targetsreached ~ impact01 + impact02 +
              (1 | Country.code), 
              data = legacy_impact_GBV, 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))
summary(Gi1)
anova(Gi0, Gi1)
# Choose model 1

# To compare model 1 and model 2, we need to make sure they both contain the same data points
Gi1.2 <- glmer(targetsreached ~ impact01 + impact02 +
             (1 | Country.code), 
             data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Americas), ], 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))

# To compare model 1 and model 3, we need to make sure they both contain the same data points
Gi1.3 <- glmer(targetsreached ~ impact01 + impact02 +
              (1 | Country.code), 
              data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Type.CO), ], 
              family = "binomial",
              control = glmerControl(optimizer = "bobyqa"))

Gi2 <- glmer(targetsreached ~ impact01 + impact02 +
               (1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA  || Country.code), 
               data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Americas), ], 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(Gi2)
anova(Gi1.2, Gi2)
# Choose model 1

Gi3 <- glmer(targetsreached ~ impact01 + impact02 +
                 (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.code), 
               data = legacy_impact_GBV[!is.na(legacy_impact_GBV$Type.CO), ], 
               family = "binomial",
               control = glmerControl(optimizer = "bobyqa"))
summary(Gi3)
anova(Gi1.3, Gi3)
# Choose model 1

Gi4 <- glmer(targetsreached ~ impact01 + impact02 +
               Year2 +
               ( 1 | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi4)
anova(Gi1, Gi4)
# Choose model 1

Gi5 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi5)
anova(Gi1, Gi5)
# Choose model 5

Gi6 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi6)
anova(Gi5, Gi6)
# Choose model 6

Gi7 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +
             Total_OL_cm + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi7)
anova(Gi6, Gi7)
# Choose model 6

Gi8 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +
             Admin_OL_cm + Staff_OL_cm + Internal_project_OL_cm + External_project_OL_cm + 
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi8)
anova(Gi6, Gi8)
# Choose model 6

Gi9 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +               
             Total_staff_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi9)
anova(Gi6, Gi9)
# Choose model 6

Gi10 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +               
             Category.AWF_cm + Category.G_cm + Category.JPO_cm + Category.N_cm + Category.P_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi10)
anova(Gi6, Gi10)
# Choose model 6

Gi11 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cm +               
             Contract.AFF_cm + Contract.FTA_cm + Contract.IND_cm + Contract.TA_cm +
             (1  | Country.code), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer = "bobyqa"))
summary(Gi11)
anova(Gi6, Gi11)
# Choose model 6

stargazer(Gi0, Gi1, Gi4, Gi5, Gi6, Gi7, Gi8, Gi10, Gi11, 
        type="html",
          column.labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
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



# Calculate Odds-Ratios [Impact] [GBV] --------------------------- --------------------------- ---------------------------
# Model 5 best fitting model, which is MI4 

ORI6 <- exp(fixef(Gi6))
CII6 <- exp(confint(Gi6,parm="beta_",method="Wald"))
OR.CI6 <- rbind(cbind(ORI6, CII6))
OR.CI6 <- round(OR.CI6,2)
colnames(OR.CI6)[1] <- "Odds"
rownames(OR.CI6) <- c("(Intercept)", 
                      "Impact indicator 1 (yes/no)" , "Impact indicator 2 (yes/no)", 
                      "Stateless persons (yes/no)", "returnees (yes/no)", 
                      "Internally displaced (yes/no)", "populations of concern (yes/no)",
                      "Target as %-change over baseline")
write.csv(OR.CI6, "04 results/OR.CI4_20230724_V01.csv")



