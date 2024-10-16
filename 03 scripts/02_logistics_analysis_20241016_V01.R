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

legacy <- read.csv("02 processed data/legacy_clean_combined_20230708.csv")


# Zero Models to predict ICC --------------------------- --------------------------- ---------------------------
# 6.1) ICC [GBV] --------------------------- --------------------------- ---------------------------

MI0 <- glmer(targetsreached ~ ( 1 | Country.codes), data = legacy_impact_GBV, family = "binomial")
summary(MI0)
icc_impact <- MI0@theta[1]^2/ (MI0@theta[1]^2 + (3.14159^2/3))
icc_impact
# icc_impact = 0.0443709

MP0 <- glmer(targetsreached ~ ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial")
summary(MP0)
icc_performance <- MP0@theta[1]^2/ (MP0@theta[1]^2 + (3.14159^2/3))
icc_performance
# icc_performance = 0.07667754



# 6.2) ICC [CBP] --------------------------- --------------------------- ---------------------------

MCI0 <- glmer(targetsreached ~ ( 1 | Country.codes), data = legacy_impact_CBP, family = "binomial")
summary(MCI0)
icc_impact <- MCI0@theta[1]^2/ (MCI0@theta[1]^2 + (3.14159^2/3))
icc_impact
# icc_impact = 0.1790499

MCP0 <- glmer(targetsreached ~ ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial")
summary(MCP0)
icc_performance <- MCP0@theta[1]^2/ (MCP0@theta[1]^2 + (3.14159^2/3))
icc_performance
# icc_performance = 0.05510748



# 7) Cluster-mean centering --------------------------- --------------------------- ---------------------------
# 7.1) Cluster-mean centering predictors [impact] [GBV] --------------------------- --------------------------- ---------------------------

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_total_OL = mean(Total_OL, na.rm = TRUE)) 
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$Total_OL_cmc <- legacy_impact_GBV$Total_OL - legacy_impact_GBV$cluster_mean_total_OL

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Admin_OL = mean(Admin_OL, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$Admin_OL_cmc <- legacy_impact_GBV$Admin_OL - legacy_impact_GBV$cluster_mean_Admin_OL

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Staff_OL = mean(Staff_OL, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$Staff_OL_cmc <- legacy_impact_GBV$Staff_OL - legacy_impact_GBV$cluster_mean_Staff_OL

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_External_project_OL = mean(External_project_OL, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$External_project_OL_cmc <- legacy_impact_GBV$External_project_OL - legacy_impact_GBV$cluster_mean_External_project_OL

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Internal_project_OL = mean(Internal_project_OL, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$Internal_project_OL_cmc <- legacy_impact_GBV$Internal_project_OL - legacy_impact_GBV$cluster_mean_Internal_project_OL

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_prop_change = mean(prop_change, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$prop_change_cmc <- legacy_impact_GBV$prop_change - legacy_impact_GBV$cluster_mean_prop_change

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_staff = mean(Total_staff, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$Total_staff_cmc <- legacy_impact_GBV$Total_staff - legacy_impact_GBV$cluster_mean_staff

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AWF = mean(AWF, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$AWF_cmc <- legacy_impact_GBV$AWF - legacy_impact_GBV$cluster_mean_AWF

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_G = mean(G, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$G_cmc <- legacy_impact_GBV$G - legacy_impact_GBV$cluster_mean_G

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_JPO = mean(JPO, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$JPO_cmc <- legacy_impact_GBV$JPO - legacy_impact_GBV$cluster_mean_JPO

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_N = mean(N, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$N_cmc <- legacy_impact_GBV$N - legacy_impact_GBV$cluster_mean_N

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_P = mean(P, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$P_cmc <- legacy_impact_GBV$P - legacy_impact_GBV$cluster_mean_P

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AFF = mean(AFF, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$AFF_cmc <- legacy_impact_GBV$AFF - legacy_impact_GBV$cluster_mean_AFF

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_FTA = mean(P, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$FTA_cmc <- legacy_impact_GBV$FTA - legacy_impact_GBV$cluster_mean_FTA

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_IND = mean(IND, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$IND_cmc <- legacy_impact_GBV$IND - legacy_impact_GBV$cluster_mean_IND

cluster_mean_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_TA = mean(TA, na.rm = TRUE))
legacy_impact_GBV <- merge(legacy_impact_GBV, cluster_mean_impact_GBV, by= c("Country"))
legacy_impact_GBV$TA_cmc <- legacy_impact_GBV$TA - legacy_impact_GBV$cluster_mean_TA

legacy_impact_GBV <- legacy_impact_GBV %>%
  group_by(Country) %>%
  mutate(prop_change_cmc = scale(prop_change_cmc),
         Total_staff_cmc = scale(Total_staff_cmc),
         AWF_cmc = scale(AWF_cmc),
         G_cmc = scale(G_cmc), 
         JPO_cmc = scale(JPO_cmc), 
         N_cmc = scale(N_cmc), 
         P_cmc = scale(P_cmc),
         AFF_cmc = scale(AFF_cmc),
         FTA_cmc = scale(FTA_cmc),
         IND_cmc = scale(IND_cmc),
         TA_cmc = scale(TA_cmc),
         Total_OL_cmc = scale(Total_OL_cmc),
         Admin_OL_cmc = scale(Admin_OL_cmc),
         Staff_OL_cmc = scale(Staff_OL_cmc),
         Internal_project_OL_cmc = scale(Internal_project_OL_cmc),
         External_project_OL_cmc = scale(External_project_OL_cmc)) %>%
  mutate(prop_change_cmc = ifelse(is.na(prop_change_cmc),0,prop_change_cmc),
         Total_staff_cmc = ifelse(is.na(Total_staff_cmc),0,Total_staff_cmc),
         AWF_cmc = ifelse(is.na(AWF_cmc),0,AWF_cmc),
         G_cmc = ifelse(is.na(G_cmc),0,G_cmc), 
         JPO_cmc = ifelse(is.na(JPO_cmc),0,JPO_cmc), 
         N_cmc = ifelse(is.na(N_cmc),0,N_cmc), 
         P_cmc = ifelse(is.na(P_cmc),0,P_cmc),
         AFF_cmc = ifelse(is.na(AFF_cmc),0,AFF_cmc),
         FTA_cmc = ifelse(is.na(FTA_cmc),0,FTA_cmc),
         IND_cmc = ifelse(is.na(IND_cmc),0,IND_cmc),
         TA_cmc = ifelse(is.na(TA_cmc),0,TA_cmc),
         Total_OL_cmc = ifelse(is.na(Total_OL_cmc),0,Total_OL_cmc),
         Admin_OL_cmc = ifelse(is.na(Admin_OL_cmc),0,Admin_OL_cmc),
         Staff_OL_cmc = ifelse(is.na(Staff_OL_cmc),0,Staff_OL_cmc),
         Internal_project_OL_cmc = ifelse(is.na(Internal_project_OL_cmc),0,Internal_project_OL_cmc),
         External_project_OL_cmc = ifelse(is.na(External_project_OL_cmc),0,External_project_OL_cmc))



# 7.2) Cluster-mean centering predictors [performance] [GBV] --------------------------- --------------------------- ---------------------------

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_total_OL = mean(Total_OL, na.rm = TRUE)) 
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$Total_OL_cmc <- legacy_performance_GBV$Total_OL - legacy_performance_GBV$cluster_mean_total_OL

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Admin_OL = mean(Admin_OL, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$Admin_OL_cmc <- legacy_performance_GBV$Admin_OL - legacy_performance_GBV$cluster_mean_Admin_OL

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Staff_OL = mean(Staff_OL, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$Staff_OL_cmc <- legacy_performance_GBV$Staff_OL - legacy_performance_GBV$cluster_mean_Staff_OL

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_External_project_OL = mean(External_project_OL, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$External_project_OL_cmc <- legacy_performance_GBV$External_project_OL - legacy_performance_GBV$cluster_mean_External_project_OL

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Internal_project_OL = mean(Internal_project_OL, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$Internal_project_OL_cmc <- legacy_performance_GBV$Internal_project_OL - legacy_performance_GBV$cluster_mean_Internal_project_OL

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_staff = mean(Total_staff, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$Total_staff_cmc <- legacy_performance_GBV$Total_staff - legacy_performance_GBV$cluster_mean_staff

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AWF = mean(AWF, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$AWF_cmc <- legacy_performance_GBV$AWF - legacy_performance_GBV$cluster_mean_AWF

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_G = mean(G, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$G_cmc <- legacy_performance_GBV$G - legacy_performance_GBV$cluster_mean_G

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_JPO = mean(JPO, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$JPO_cmc <- legacy_performance_GBV$JPO - legacy_performance_GBV$cluster_mean_JPO

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_N = mean(N, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$N_cmc <- legacy_performance_GBV$N - legacy_performance_GBV$cluster_mean_N

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_P = mean(P, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$P_cmc <- legacy_performance_GBV$P - legacy_performance_GBV$cluster_mean_P

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AFF = mean(AFF, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$AFF_cmc <- legacy_performance_GBV$AFF - legacy_performance_GBV$cluster_mean_AFF

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_FTA = mean(P, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$FTA_cmc <- legacy_performance_GBV$FTA - legacy_performance_GBV$cluster_mean_FTA

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_IND = mean(IND, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$IND_cmc <- legacy_performance_GBV$IND - legacy_performance_GBV$cluster_mean_IND

cluster_mean_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_TA = mean(TA, na.rm = TRUE))
legacy_performance_GBV <- merge(legacy_performance_GBV, cluster_mean_performance_GBV, by= c("Country"))
legacy_performance_GBV$TA_cmc <- legacy_performance_GBV$TA - legacy_performance_GBV$cluster_mean_TA

legacy_performance_GBV <- legacy_performance_GBV %>%
  group_by(Country) %>%
  mutate(Total_staff_cmc = scale(Total_staff_cmc),
         AWF_cmc = scale(AWF_cmc),
         G_cmc = scale(G_cmc), 
         JPO_cmc = scale(JPO_cmc), 
         N_cmc = scale(N_cmc), 
         P_cmc = scale(P_cmc),
         AFF_cmc = scale(AFF_cmc),
         FTA_cmc = scale(FTA_cmc),
         IND_cmc = scale(IND_cmc),
         TA_cmc = scale(TA_cmc),
         Total_OL_cmc = scale(Total_OL_cmc),
         Admin_OL_cmc = scale(Admin_OL_cmc),
         Staff_OL_cmc = scale(Staff_OL_cmc),
         Internal_project_OL_cmc = scale(Internal_project_OL_cmc),
         External_project_OL_cmc = scale(External_project_OL_cmc)) %>%
  mutate(Total_staff_cmc = ifelse(is.na(Total_staff_cmc),0,Total_staff_cmc),
         AWF_cmc = ifelse(is.na(AWF_cmc),0,AWF_cmc),
         G_cmc = ifelse(is.na(G_cmc),0,G_cmc), 
         JPO_cmc = ifelse(is.na(JPO_cmc),0,JPO_cmc), 
         N_cmc = ifelse(is.na(N_cmc),0,N_cmc), 
         P_cmc = ifelse(is.na(P_cmc),0,P_cmc),
         AFF_cmc = ifelse(is.na(AFF_cmc),0,AFF_cmc),
         FTA_cmc = ifelse(is.na(FTA_cmc),0,FTA_cmc),
         IND_cmc = ifelse(is.na(IND_cmc),0,IND_cmc),
         TA_cmc = ifelse(is.na(TA_cmc),0,TA_cmc),
         Total_OL_cmc = ifelse(is.na(Total_OL_cmc),0,Total_OL_cmc),
         Admin_OL_cmc = ifelse(is.na(Admin_OL_cmc),0,Admin_OL_cmc),
         Staff_OL_cmc = ifelse(is.na(Staff_OL_cmc),0,Staff_OL_cmc),
         Internal_project_OL_cmc = ifelse(is.na(Internal_project_OL_cmc),0,Internal_project_OL_cmc),
         External_project_OL_cmc = ifelse(is.na(External_project_OL_cmc),0,External_project_OL_cmc))


# 7.3) Cluster-mean centering predictors [impact] [CBP] --------------------------- --------------------------- ---------------------------

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_total_OL = mean(Total_OL, na.rm = TRUE)) 
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$Total_OL_cmc <- legacy_impact_CBP$Total_OL - legacy_impact_CBP$cluster_mean_total_OL

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Admin_OL = mean(Admin_OL, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$Admin_OL_cmc <- legacy_impact_CBP$Admin_OL - legacy_impact_CBP$cluster_mean_Admin_OL

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Staff_OL = mean(Staff_OL, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$Staff_OL_cmc <- legacy_impact_CBP$Staff_OL - legacy_impact_CBP$cluster_mean_Staff_OL

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_External_project_OL = mean(External_project_OL, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$External_project_OL_cmc <- legacy_impact_CBP$External_project_OL - legacy_impact_CBP$cluster_mean_External_project_OL

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Internal_project_OL = mean(Internal_project_OL, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$Internal_project_OL_cmc <- legacy_impact_CBP$Internal_project_OL - legacy_impact_CBP$cluster_mean_Internal_project_OL

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_prop_change = mean(prop_change, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$prop_change_cmc <- legacy_impact_CBP$prop_change - legacy_impact_CBP$cluster_mean_prop_change

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_staff = mean(Total_staff, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$Total_staff_cmc <- legacy_impact_CBP$Total_staff - legacy_impact_CBP$cluster_mean_staff

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AWF = mean(AWF, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$AWF_cmc <- legacy_impact_CBP$AWF - legacy_impact_CBP$cluster_mean_AWF

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_G = mean(G, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$G_cmc <- legacy_impact_CBP$G - legacy_impact_CBP$cluster_mean_G

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_JPO = mean(JPO, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$JPO_cmc <- legacy_impact_CBP$JPO - legacy_impact_CBP$cluster_mean_JPO

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_N = mean(N, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$N_cmc <- legacy_impact_CBP$N - legacy_impact_CBP$cluster_mean_N

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_P = mean(P, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$P_cmc <- legacy_impact_CBP$P - legacy_impact_CBP$cluster_mean_P

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AFF = mean(AFF, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$AFF_cmc <- legacy_impact_CBP$AFF - legacy_impact_CBP$cluster_mean_AFF

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_FTA = mean(P, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$FTA_cmc <- legacy_impact_CBP$FTA - legacy_impact_CBP$cluster_mean_FTA

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_IND = mean(IND, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$IND_cmc <- legacy_impact_CBP$IND - legacy_impact_CBP$cluster_mean_IND

cluster_mean_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_TA = mean(TA, na.rm = TRUE))
legacy_impact_CBP <- merge(legacy_impact_CBP, cluster_mean_impact_CBP, by= c("Country"))
legacy_impact_CBP$TA_cmc <- legacy_impact_CBP$TA - legacy_impact_CBP$cluster_mean_TA

legacy_impact_CBP <- legacy_impact_CBP %>%
  group_by(Country) %>%
  mutate(prop_change_cmc = scale(prop_change_cmc),
         Total_staff_cmc = scale(Total_staff_cmc),
         AWF_cmc = scale(AWF_cmc),
         G_cmc = scale(G_cmc), 
         JPO_cmc = scale(JPO_cmc), 
         N_cmc = scale(N_cmc), 
         P_cmc = scale(P_cmc),
         AFF_cmc = scale(AFF_cmc),
         FTA_cmc = scale(FTA_cmc),
         IND_cmc = scale(IND_cmc),
         TA_cmc = scale(TA_cmc),
         Total_OL_cmc = scale(Total_OL_cmc),
         Admin_OL_cmc = scale(Admin_OL_cmc),
         Staff_OL_cmc = scale(Staff_OL_cmc),
         Internal_project_OL_cmc = scale(Internal_project_OL_cmc),
         External_project_OL_cmc = scale(External_project_OL_cmc)) %>%
  mutate(prop_change_cmc = ifelse(is.na(prop_change_cmc),0,prop_change_cmc),
         Total_staff_cmc = ifelse(is.na(Total_staff_cmc),0,Total_staff_cmc),
         AWF_cmc = ifelse(is.na(AWF_cmc),0,AWF_cmc),
         G_cmc = ifelse(is.na(G_cmc),0,G_cmc), 
         JPO_cmc = ifelse(is.na(JPO_cmc),0,JPO_cmc), 
         N_cmc = ifelse(is.na(N_cmc),0,N_cmc), 
         P_cmc = ifelse(is.na(P_cmc),0,P_cmc),
         AFF_cmc = ifelse(is.na(AFF_cmc),0,AFF_cmc),
         FTA_cmc = ifelse(is.na(FTA_cmc),0,FTA_cmc),
         IND_cmc = ifelse(is.na(IND_cmc),0,IND_cmc),
         TA_cmc = ifelse(is.na(TA_cmc),0,TA_cmc),
         Total_OL_cmc = ifelse(is.na(Total_OL_cmc),0,Total_OL_cmc),
         Admin_OL_cmc = ifelse(is.na(Admin_OL_cmc),0,Admin_OL_cmc),
         Staff_OL_cmc = ifelse(is.na(Staff_OL_cmc),0,Staff_OL_cmc),
         Internal_project_OL_cmc = ifelse(is.na(Internal_project_OL_cmc),0,Internal_project_OL_cmc),
         External_project_OL_cmc = ifelse(is.na(External_project_OL_cmc),0,External_project_OL_cmc))



# 7.4) Cluster-mean centering predictors [performance] [CBP] --------------------------- --------------------------- ---------------------------

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_total_OL = mean(Total_OL, na.rm = TRUE)) 
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$Total_OL_cmc <- legacy_performance_CBP$Total_OL - legacy_performance_CBP$cluster_mean_total_OL

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Admin_OL = mean(Admin_OL, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$Admin_OL_cmc <- legacy_performance_CBP$Admin_OL - legacy_performance_CBP$cluster_mean_Admin_OL

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Staff_OL = mean(Staff_OL, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$Staff_OL_cmc <- legacy_performance_CBP$Staff_OL - legacy_performance_CBP$cluster_mean_Staff_OL

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_External_project_OL = mean(External_project_OL, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$External_project_OL_cmc <- legacy_performance_CBP$External_project_OL - legacy_performance_CBP$cluster_mean_External_project_OL

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_Internal_project_OL = mean(Internal_project_OL, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$Internal_project_OL_cmc <- legacy_performance_CBP$Internal_project_OL - legacy_performance_CBP$cluster_mean_Internal_project_OL

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_staff = mean(Total_staff, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$Total_staff_cmc <- legacy_performance_CBP$Total_staff - legacy_performance_CBP$cluster_mean_staff

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AWF = mean(AWF, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$AWF_cmc <- legacy_performance_CBP$AWF - legacy_performance_CBP$cluster_mean_AWF

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_G = mean(G, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$G_cmc <- legacy_performance_CBP$G - legacy_performance_CBP$cluster_mean_G

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_JPO = mean(JPO, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$JPO_cmc <- legacy_performance_CBP$JPO - legacy_performance_CBP$cluster_mean_JPO

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_N = mean(N, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$N_cmc <- legacy_performance_CBP$N - legacy_performance_CBP$cluster_mean_N

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_P = mean(P, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$P_cmc <- legacy_performance_CBP$P - legacy_performance_CBP$cluster_mean_P

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_AFF = mean(AFF, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$AFF_cmc <- legacy_performance_CBP$AFF - legacy_performance_CBP$cluster_mean_AFF

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_FTA = mean(P, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$FTA_cmc <- legacy_performance_CBP$FTA - legacy_performance_CBP$cluster_mean_FTA

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_IND = mean(IND, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$IND_cmc <- legacy_performance_CBP$IND - legacy_performance_CBP$cluster_mean_IND

cluster_mean_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  dplyr::summarise(cluster_mean_TA = mean(TA, na.rm = TRUE))
legacy_performance_CBP <- merge(legacy_performance_CBP, cluster_mean_performance_CBP, by= c("Country"))
legacy_performance_CBP$TA_cmc <- legacy_performance_CBP$TA - legacy_performance_CBP$cluster_mean_TA

legacy_performance_CBP <- legacy_performance_CBP %>%
  group_by(Country) %>%
  mutate(Total_staff_cmc = scale(Total_staff_cmc),
         AWF_cmc = scale(AWF_cmc),
         G_cmc = scale(G_cmc), 
         JPO_cmc = scale(JPO_cmc), 
         N_cmc = scale(N_cmc), 
         P_cmc = scale(P_cmc),
         AFF_cmc = scale(AFF_cmc),
         FTA_cmc = scale(FTA_cmc),
         IND_cmc = scale(IND_cmc),
         TA_cmc = scale(TA_cmc),
         Total_OL_cmc = scale(Total_OL_cmc),
         Admin_OL_cmc = scale(Admin_OL_cmc),
         Staff_OL_cmc = scale(Staff_OL_cmc),
         Internal_project_OL_cmc = scale(Internal_project_OL_cmc),
         External_project_OL_cmc = scale(External_project_OL_cmc)) %>%
  mutate(Total_staff_cmc = ifelse(is.na(Total_staff_cmc),0,Total_staff_cmc),
         AWF_cmc = ifelse(is.na(AWF_cmc),0,AWF_cmc),
         G_cmc = ifelse(is.na(G_cmc),0,G_cmc), 
         JPO_cmc = ifelse(is.na(JPO_cmc),0,JPO_cmc), 
         N_cmc = ifelse(is.na(N_cmc),0,N_cmc), 
         P_cmc = ifelse(is.na(P_cmc),0,P_cmc),
         AFF_cmc = ifelse(is.na(AFF_cmc),0,AFF_cmc),
         FTA_cmc = ifelse(is.na(FTA_cmc),0,FTA_cmc),
         IND_cmc = ifelse(is.na(IND_cmc),0,IND_cmc),
         TA_cmc = ifelse(is.na(TA_cmc),0,TA_cmc),
         Total_OL_cmc = ifelse(is.na(Total_OL_cmc),0,Total_OL_cmc),
         Admin_OL_cmc = ifelse(is.na(Admin_OL_cmc),0,Admin_OL_cmc),
         Staff_OL_cmc = ifelse(is.na(Staff_OL_cmc),0,Staff_OL_cmc),
         Internal_project_OL_cmc = ifelse(is.na(Internal_project_OL_cmc),0,Internal_project_OL_cmc),
         External_project_OL_cmc = ifelse(is.na(External_project_OL_cmc),0,External_project_OL_cmc))



# 8) Models to predict impact --------------------------- --------------------------- ---------------------------
# 8.1) Estimating [ML logistics - impact] [GBV] --------------------------- --------------------------- ---------------------------

MI1 <- glmer(targetsreached ~ impact01 + impact02 +
             (1 | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI1)
anova(MI0, MI1)

MI1.1 <- glmer(targetsreached ~ impact01 + impact02 +
               (1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA  || Country.codes), 
               data = legacy_impact_GBV, 
               family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
summary(MI1.1)
anova(MI1, MI1.1)

MI1.2 <- glmer(targetsreached ~ impact01 + impact02 +
                 (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.codes), 
               data = legacy_impact_GBV, 
               family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
summary(MI1.1)
anova(MI1, MI1.2)

MI2 <- glmer(targetsreached ~ impact01 + impact02 +
               Year2 +
               ( 1 | Country.codes), data = legacy_impact_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI2)
anova(MI1, MI2)

MI3 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI3)
anova(MI1, MI3)

MI4 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI4)
anova(MI3, MI4)

MI5 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             Total_OL_cmc + 
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI5)
anova(MI4, MI5)

MI6 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI6)
anova(MI4, MI6)

MI6.1 <- glmer(targetsreached ~ impact01 + impact02 +
               PPG2 + PPG3 + PPG4 + PPG5 + 
               prop_change_cmc +
               Internal_project_OL_cmc +  
               (1  | Country.codes), 
               data = legacy_impact_GBV, 
               family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
summary(MI6.1)
anova(MI4, MI6.1)

MI7 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             Total_staff_cmc +
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI7)
anova(MI4, MI7)

MI8 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             AWF_cmc + G_cmc + JPO_cmc + N_cmc + P_cmc +
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI8)
anova(MI4, MI8)

MI9 <- glmer(targetsreached ~ impact01 + impact02 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             prop_change_cmc +
             AFF_cmc + FTA_cmc + IND_cmc + TA_cmc +
             (1  | Country.codes), 
             data = legacy_impact_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MI9)
anova(MI4, MI9)

stargazer(MI0, MI1, MI2, MI3, MI4, MI5, MI6, MI7, MI8, MI9, type="html",
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)"),
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
          out="07 Analysis/0701 Legacy analysis/MLM models/GBV_impact_models_20230710_V01.htm")



# 8.1) Estimating [ML logistics - impact] [CBP] --------------------------- --------------------------- ---------------------------

MCI1 <- glmer(targetsreached ~ impact01 + impact03 +
               (1 | Country.codes), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MCI1)

MCI1.1 <- glmer(targetsreached ~ impact01 + impact03 +
                 (1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA  || Country.codes), 
               data = legacy_impact_CBP, 
               family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
summary(MCI1.1)
anova(MCI1, MCI1.1)

MCI1.2 <- glmer(targetsreached ~ impact01 + impact03 +
                (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.codes), 
                data = legacy_impact_CBP, 
                family = "binomial",
                control = glmerControl(optimizer="bobyqa"))
summary(MCI1.2)
anova(MCI1, MCI1.2)

MCI2 <- glmer(targetsreached ~ impact01 + impact03 +
             Year2 +
             ( 1 | Country.codes), data = legacy_impact_CBP, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MCI2)
anova(MCI1, MCI2)

MCI3 <- glmer(targetsreached ~ impact01 + impact03 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             (1  | Country.codes), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MCI3)
anova(MCI1, MCI3)

MCI4 <- glmer(targetsreached ~ impact01 + impact03 +
             prop_change_cmc +
             (1  | Country.codes), 
             data = legacy_impact_CBP, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MCI4)
anova(MCI1, MCI4)

MCI5 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              (1  | Country.codes), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCI5)
anova(MCI4, MCI5)

MCI6 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
              (1  | Country.codes), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCI6)
anova(MCI5, MCI6)

MCI7 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              Total_staff_cmc +
              (1  | Country.codes), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCI7)
anova(MCI5, MCI7)

MCI8 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              AWF_cmc + G_cmc + JPO_cmc + N_cmc + P_cmc +
              (1  | Country.codes), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCI8)
anova(MCI5, MCI8)

MCI9 <- glmer(targetsreached ~ impact01 + impact03 +
              prop_change_cmc +
              Total_OL_cmc + 
              AFF_cmc + FTA_cmc + IND_cmc + TA_cmc +
              (1  | Country.codes), 
              data = legacy_impact_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCI9)
anova(MCI5, MCI9)

stargazer(MCI0, MCI1, MCI2, MCI3, MCI4, MCI5, MCI6, MCI7, MCI8, MCI9, type="html", 
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
          out="07 Analysis/0701 Legacy analysis/MLM models/CBP_impact_models_20230719_V01.htm")



# 9) Models to predict performance --------------------------- --------------------------- ---------------------------
# 9.1) Models to predict performance [GBV] --------------------------- --------------------------- ---------------------------

MP1 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             ( 1 | Country.codes), 
             data = legacy_performance_GBV, 
             family = "binomial",
             control = glmerControl(optimizer="bobyqa"))

MP1.1 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               ( 1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA || Country.codes), 
               data = legacy_performance_GBV, 
               family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MP1.1)
anova(MP1, MP1.1)

MP1.2 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.codes), 
               data = legacy_performance_GBV, 
               family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
summary(MP1.2)
anova(MP1, MP1.2)

MP2 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             Year2 +
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MP2)
anova(MP1, MP2)

MP3 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MP3)
anova(MP1, MP3)

MP4 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Total_OL_cmc + 
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MP4)
anova(MP3, MP4)

MP5 <- glmer(targetsreached ~ performance01 + performance02 + 
             performance03 + performance06 + performance07 + 
             performance08 + performance09 + performance10 + 
             performance12 + performance13 + performance15 + 
             performance16 + performance17 +
             PPG2 + PPG3 + PPG4 + PPG5 + 
             Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MP5)
anova(MP4, MP5)

MP5.1 <- glmer(targetsreached ~ performance01 + performance02 + 
               performance03 + performance06 + performance07 + 
               performance08 + performance09 + performance10 + 
               performance12 + performance13 + performance15 + 
               performance16 + performance17 +
               PPG2 + PPG3 + PPG4 + PPG5 + 
               External_project_OL_cmc + 
               ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
               control = glmerControl(optimizer="bobyqa"))
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
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
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
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))
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
               ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
               control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))
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
             ( 1 | Country.codes), data = legacy_performance_GBV, family = "binomial",
             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))
summary(MP8)
anova(MP7.1, MP8)

stargazer(MP0, MP1, MP2, MP3, MP4, MP5, MP6, MP7, MP7.1, MP8, type="html", 
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
          out="07 Analysis/0701 Legacy analysis/MLM models/GBV_performance_models_20230719_V01.htm")



# 9.2) Models to predict performance [CBP] --------------------------- --------------------------- ---------------------------

MCP1 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              ( 1 | Country.codes), 
              data = legacy_performance_CBP, 
              family = "binomial",
              control = glmerControl(optimizer="bobyqa"))

MCP1.1 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                ( 1 + Americas + Asia + EHAGL + MENA + SAfrica + WCA || Country.codes), 
                data = legacy_performance_CBP, 
                family = "binomial",
                control = glmerControl(optimizer="bobyqa"))
summary(MCP1.1)
anova(MCP1, MCP1.1)

MCP1.2 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                (1 + Type.CO + Type.LO + Type.MCO + Type.NO + Type.Oocm  || Country.codes), 
                data = legacy_performance_CBP, 
                family = "binomial",
                control = glmerControl(optimizer="bobyqa"))
summary(MCP1.1)
anova(MCP1, MCP1.2)

MCP2 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              Year2 +
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCP2)
anova(MCP1, MCP2)

MCP3 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCP3)
anova(MCP1, MCP3)

MCP4 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              Total_OL_cmc + 
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
summary(MCP4)
anova(MCP3, MCP4)

MCP5 <- glmer(targetsreached ~ performance01 + performance02 + 
              performance03 + performance04 + performance05 + 
              performance06 + performance07 + performance08 + 
              performance09 + performance10 + performance11 + 
              performance12 + performance13 + performance14 +
              PPG2 + PPG3 + PPG4 + PPG5 + 
              Admin_OL_cmc + Staff_OL_cmc + Internal_project_OL_cmc + External_project_OL_cmc + 
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
             control = glmerControl(optimizer="bobyqa"))
summary(MCP5)
anova(MCP3, MCP5)

MCP5.1 <- glmer(targetsreached ~ performance01 + performance02 + 
                performance03 + performance04 + performance05 + 
                performance06 + performance07 + performance08 + 
                performance09 + performance10 + performance11 + 
                performance12 + performance13 + performance14 +
                PPG2 + PPG3 + PPG4 + PPG5 + 
                External_project_OL_cmc + 
                ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
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
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa"))
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
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
              control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))
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
              ( 1 | Country.codes), data = legacy_performance_CBP, family = "binomial",
             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))
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
          out="07 Analysis/0701 Legacy analysis/MLM models/CBP_performance_models_20230719_V01.htm")



# 10) Calculate Odds-Ratios --------------------------- --------------------------- ---------------------------
# 10.1) Calculate Odds-Ratios [Impact] [GBV] --------------------------- --------------------------- ---------------------------
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
performance::r2(MI0)
performance::r2(MI4)
write.csv(OR.CI4, "06 Data/01 Legacy Analysis/0106 Tables/OR.CI4_20230724_V01.csv")



# 10.2) Calculate Odds-Ratios [performance] [GBV] --------------------------- --------------------------- ---------------------------
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
performance::r2(MP0)
performance::r2(MP7.1)
write.csv(OR.CP7.1, "06 Data/01 Legacy Analysis/0106 Tables/OR.CP7.1_20230718_V01.csv")



# 10.3) Calculate Odds-Ratios [Impact] [CBP] --------------------------- --------------------------- ---------------------------
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
performance::r2(MCI0)
performance::r2(MCI5)
write.csv(OR.CPI5, "06 Data/01 Legacy Analysis/0106 Tables/OR.CI5_CBP_20230718_V01.csv")



# 10.4) Calculate Odds-Ratios [performance] [CBP] --------------------------- --------------------------- ---------------------------
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



