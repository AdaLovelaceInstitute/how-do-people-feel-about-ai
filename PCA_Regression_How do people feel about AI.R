#
###################################################################################################
# Attitudes to AI Survey 2023
# Full R script for combined regression plot
###################################################################################################
#
####################################################################################################
# 1. General data management to create initial 'analysis_data' dataset
####################################################################################################
#
remove(list = ls())
setwd("..")
install.packages("questionr")
install.packages("gmodels")
install.packages("psych")
install.packages("GPArotation")
install.packages("summarytools") 
install.packages("foreign")
install.packages("fmsb")
install.packages("labelled")
install.packages("dotwhisker")
install.packages("tidyverse")


library(questionr)
library(gmodels)
library(psych)
library(GPArotation)
library(summarytools) 
library(foreign)
library(fmsb)
library(labelled)
library(dotwhisker)
library(tidyverse)
#
# Read in original data files from Kantar
ai_raw_data <- read_csv("262400371_Public Attitudes to AI Raw Data NA.csv")
# 
# rename items to more interpretable names
ai_raw_data <- rename(ai_raw_data, ben_face_unlock = BENA_MP
                          , con_face_unlock = CONA_MP
                          , ben_face_border = BENA_BC
                          , con_face_border = CONA_BC
                          , ben_face_police = BENA_PS
                          , con_face_police = CONA_PS
                          , ben_welfare = BENA_WEL
                          , con_welfare = CONA_WEL
                          , ben_job = BENA_JOB
                          , con_job = CONA_JOB
                          , ben_cancer = BENA_CAN
                          , con_cancer = CONA_CAN
                          , ben_loan = BENA_LOAN
                          , con_loan = CONA_LOAN
                          , ben_socmedia_consum = BENA_SMC                        
                          , con_socmedia_consum = CONA_SMC
                          , ben_socmedia_politad = BENA_SMP                        
                          , con_socmedia_politad = CONA_SMP                         
                          , ben_virt_assist_speaker = BENA_VASS
                          , con_virt_assist_speaker = CONA_VASS
                          , ben_virt_assist_health = BENA_VAH
                          , con_virt_assist_health = CONA_VAH                        
                          , ben_rob_vacuum = BENA_RVC
                          , con_rob_vacuum = CONA_RVC
                          , ben_rob_care_assist = BENA_RCA
                          , con_rob_care_assist = CONA_RCA
                          , ben_driverless_cars = BENA_DC
                          , con_driverless_cars = CONA_DC
                          , ben_auto_weapons = BENA_AW
                          , con_auto_weapons = CONA_AW
                          , ben_climate_res = BENA_CCR
                          , con_climate_res = CONA_CCR
                          , ben_educ = BENA_VR
                          , con_educ = CONA_VR
)
# 
group1 <- c("ben_face_unlock", "con_face_unlock", "ben_face_police", "con_face_police", "ben_job", "con_job", "ben_loan", "con_loan", "ben_socmedia_consum", "con_socmedia_consum", "ben_virt_assist_speaker", "con_virt_assist_speaker", "ben_rob_vacuum", "con_rob_vacuum", "ben_driverless_cars", "con_driverless_cars", "ben_climate_res", "con_climate_res") 
group2 <- c("ben_face_unlock", "con_face_unlock", "ben_face_border", "con_face_border", "ben_welfare", "con_welfare", "ben_cancer", "con_cancer", "ben_socmedia_politad", "con_socmedia_politad", "ben_virt_assist_health", "con_virt_assist_health", "ben_rob_care_assist", "con_rob_care_assist", "ben_auto_weapons", "con_auto_weapons", "ben_educ", "con_educ") 
# 
# recode codes 5 and 6 (DK and Ref) as missing
ai_raw_data <- ai_raw_data %>%
  mutate_at(group1, funs(na_if(.,6)))

ai_raw_data <- ai_raw_data %>%
  mutate_at(group1, funs(na_if(.,5)))

ai_raw_data <- ai_raw_data %>%
  mutate_at(group2, funs(na_if(.,6)))

ai_raw_data <- ai_raw_data %>%
  mutate_at(group2, funs(na_if(.,5)))
#
keep.these <- c(
  "Respondent_ID",
  "PV25_Weight",
  "CS_Age",
  "CS_Age_Band",
  "RS_Sex",
  "RS_EducationLevel",
  "RS_EmploymentStatus_MostRecent",
  "RS_NSSEC_SummaryCat",   
  "DR_Socio_Eco",
  "RS_InternetUseFrequency",
  "RS_NumTechDevices",
  "TECHINTE",
  "TECHINFO",
  "DIG_LIT_1",
  "DIG_LIT_2",
  "DIG_LIT_3",
  "DIG_LIT_4",
  "DIG_LIT_5",
  "DIG_LIT_6",
  "DIG_LIT_7",
  "DIG_LIT_8",
  "TECHSELF_1",
  "TECHSELF_2",
  "TECHSELF_3",
  "TECHSELF_4",
  "TECHSELF_5",
  "TECHSOCIAL_1",
  "TECHSOCIAL_2",
  "TECHINTE_NET",                                 
  "TECHINFO_NET",
  "DR_DIGLIT_CONFIDENCE",                         
  "DR_TECHCOMFORT",
  "RS_RespectUKValues",
  "RS_BanInternetSites",
  "RS_ProtectWorkingConditionsWages",
  "RS_BusinessOverregulated",
  "RS_NationalisePublicServicesIndustries",
  "RS_HumanRightProtest",
  "RS_TolerateUnconventional",
  "RS_TougherSentences",
  "RS_RedistributeIncome",
  "RS_BigBusinessBenefitsOwners",
  "RS_FewerImmigrants",
  "RS_AbolishMonarchy",
  "RS_BFI_2_XS_ExtraversionScore",
  "RS_BFI_2_XS_AgreeablenessScore",
  "RS_BFI_2_XS_ConscientiousnessScore",
  "RS_BFI_2_XS_NegativeEmotionalityScore",
  "RS_BFI_2_XS_OpenMindednessScore",
  "AWARE_MP",
  "AWARE_BC",
  "AWARE_PS",
  "AWARE_WEL",
  "AWARE_JOB",
  "AWARE_CAN",
  "AWARE_LOAN",
  "AWARE_SMC",
  "AWARE_SMP",
  "AWARE_VASS",
  "AWARE_VAH",
  "AWARE_RVC",
  "AWARE_RCA",
  "AWARE_DC",
  "AWARE_AW",
  "AWARE_CCR",
  "AWARE_VR",
  "EXP_MP",
  "EXP_BC",
  "EXP_PS",
  "EXP_WEL",
  "EXP_JOB",
  "EXP_CAN",
  "EXP_LOAN",
  "EXP_SMC",
  "EXP_SMP",
  "EXP_VASS",
  "EXP_VAH",
  "EXP_RVC",
  "EXP_VR",
  "ben_face_unlock", 
  "con_face_unlock", 
  "ben_face_police", 
  "con_face_police", 
  "ben_job", 
  "con_job", 
  "ben_loan", 
  "con_loan", 
  "ben_virt_assist_speaker", 
  "con_virt_assist_speaker", 
  "ben_rob_vacuum", 
  "con_rob_vacuum", 
  "ben_driverless_cars", 
  "con_driverless_cars", 
  "ben_climate_res", 
  "con_climate_res",
  "ben_face_border", 
  "con_face_border", 
  "ben_welfare", 
  "con_welfare", 
  "ben_cancer", 
  "con_cancer", 
  "ben_socmedia_politad", 
  "con_socmedia_politad", 
  "ben_virt_assist_health", 
  "con_virt_assist_health", 
  "ben_rob_care_assist", 
  "con_rob_care_assist", 
  "ben_auto_weapons", 
  "con_auto_weapons", 
  "ben_educ", 
  "con_educ",
  "ben_socmedia_consum", 
  "con_socmedia_consum" 
  )
ai_raw_data <- ai_raw_data[keep.these]
# 
# Saving the file
saveRDS(ai_raw_data, "analysis_data.rds")
# 
#################################################################################
# 2. Demographic variables recoding
#################################################################################
# 
analysis_data <- readRDS("analysis_data.rds")
# 
library(questionr)
library(tidyverse)
library(dplyr)
library(gmodels)
#
# delete 16yrd old and Sex=Other
analysis_data<-analysis_data[!(analysis_data$CS_Age==16 | analysis_data$RS_Sex==3),]
#
# age band dummies
analysis_data$Age_band_factor <- factor(analysis_data$CS_Age_Band)
library(forcats)
# Combine age bands 
analysis_data %>%
  mutate(age_band_new = fct_collapse(Age_band_factor,
                                     "16-34" = c("1", "2"), 
                                     "35-64" = c("3", "4", "5"),
                                     "65 and above" = c("6", "7")
  )) -> analysis_data 
#
# dummies for age
analysis_data$age65_and_over <- ifelse(analysis_data$age_band_new=="65 and above",1,0)
freq(analysis_data$age65_and_over, cum = TRUE, total = TRUE)
#
# recode RS_Sex into dummy
analysis_data$Male <- analysis_data$RS_Sex
analysis_data %>%
  mutate(Male=ifelse(Male==1,1,0),
  ) -> analysis_data
#
# Techinfo and Techinte recoding
analysis_data$TECHINFO[analysis_data$TECHINFO==5] <- NA
analysis_data$TECHINFO[analysis_data$TECHINFO==6] <- NA
analysis_data$TECHINTE[analysis_data$TECHINTE==5] <- NA
analysis_data$TECHINTE[analysis_data$TECHINTE==6] <- NA
# 
# reverse code
temp_vars <- c("TECHINFO","TECHINTE")
analysis_data[ , temp_vars] = 5 - analysis_data[ , temp_vars]
#
# Education variable recoding
analysis_data %>%
  mutate(Degree=ifelse(RS_EducationLevel==1,1,0),
  ) -> analysis_data
#
# analysis_data1 includes all key variables recoded 
saveRDS(analysis_data, "analysis_data1.rds")
#
#################################################################################
# 3. Principle Component Analysis
#################################################################################
# 
analysis_data1 <- readRDS("analysis_data1.rds")
# PCA1: Digital literacy
itemsdig <- c("DIG_LIT_1", "DIG_LIT_2", "DIG_LIT_3", "DIG_LIT_4", "DIG_LIT_5", "DIG_LIT_6", "DIG_LIT_7", "DIG_LIT_8")
# Don't know and Prefer not to say recoded into missing
analysis_data1 <- analysis_data1 %>%
  mutate_at(itemsdig, funs(na_if(.,5)))
analysis_data1 <- analysis_data1 %>%
  mutate_at(itemsdig, funs(na_if(.,6)))
apply((analysis_data1[c(itemsdig)]), 2, table)
#
# reverse code
analysis_data1[ , itemsdig] = 5 - analysis_data1[ , itemsdig]
apply((analysis_data1[c(itemsdig)]), 2, table)
# correlation
round(cor(analysis_data1[,itemsdig],use="complete.obs"),4)
# PCA
PCA_DigLit <- princomp(~DIG_LIT_1+DIG_LIT_2+DIG_LIT_3+DIG_LIT_4+DIG_LIT_5+DIG_LIT_6+DIG_LIT_7+DIG_LIT_8,
                          data=analysis_data1, cor = TRUE, scores = TRUE, na.action=na.exclude)
summary(PCA_DigLit)
PCA_DigLit$sdev^2
PCA_DigLit.weights <- loadings(PCA_DigLit)
print(PCA_DigLit.weights,cutoff=0,digits=4)
sqrt.lambda <- PCA_DigLit$sdev
#
# Loadings
print(t(t(PCA_DigLit.weights)*sqrt.lambda),cutoff = 0,digits=4)
#
# Responses and PC scores 1 and 2 for first 10 respondents 
cbind(analysis_data1[,itemsdig], round(PCA_DigLit$score[,1:2],3))[1:10,]
# 
analysis_data1 <- cbind(analysis_data1, PCA_DigLit$score)
analysis_data1 <- analysis_data1 %>% 
  rename(digital_comp1 = Comp.1)
summary(analysis_data1$digital_comp1)
#
drop <- c("Comp.2","Comp.3","Comp.4","Comp.5","Comp.6","Comp.7","Comp.8")
analysis_data1 = analysis_data1[,!(names(analysis_data1) %in% drop)] 
#
# PCA2: Comfortable with technology (7 items)
itemscomf <- c("TECHSELF_1", "TECHSELF_2", "TECHSELF_3", "TECHSELF_4", "TECHSELF_5", "TECHSOCIAL_1", "TECHSOCIAL_2")
# correlation
round(cor(analysis_data1[,itemscomf],use="complete.obs"),4)
# PCA
PCA_TechComf <- princomp(~TECHSELF_1+TECHSELF_2+TECHSELF_3+TECHSELF_4+TECHSELF_5+TECHSOCIAL_1+TECHSOCIAL_2,
                         data=analysis_data1, cor = TRUE, scores = TRUE, na.action=na.exclude)
summary(PCA_TechComf)
PCA_TechComf$sdev^2
PCA_TechComf.weights <- loadings(PCA_TechComf)
print(PCA_TechComf.weights,cutoff=0,digits=4)
#
sqrt.lambda <- PCA_TechComf$sdev
# Loadings
print(t(t(PCA_TechComf.weights)*sqrt.lambda),cutoff = 0,digits=4)
#
# Responses and PC scores 1 and 2 for first 10 respondents 
cbind(analysis_data1[,itemscomf], round(PCA_TechComf$score[,1:2],3))[1:10,]
#
analysis_data1 <- cbind(analysis_data1, PCA_TechComf$score)
analysis_data1 <- analysis_data1 %>% 
  rename(techcomf7_comp1 = Comp.1)
summary(analysis_data1$techcomf7_comp1)
#
drop <- c("Comp.2","Comp.3","Comp.4","Comp.5","Comp.6","Comp.7")
analysis_data1 = analysis_data1[,!(names(analysis_data1) %in% drop)] 
names(analysis_data1)
# analysis_data2 includes all key variables recoded and PCA Comp1 scores for diglit and techcomf
saveRDS(analysis_data1, "analysis_data2.rds")
#
#################################################################################
# 4. Further recoding for combined regression coefficients plot
#################################################################################
#
# Open data file for regression analysis
analysis_data2 <- readRDS("analysis_data2.rds")
#
# create a new variable 'net_benefit'=benefit - concern
net_vars <- c("ben_face_unlock","con_face_unlock","ben_face_police","con_face_police","ben_job","con_job","ben_loan"
              ,"con_loan","ben_virt_assist_speaker","con_virt_assist_speaker","ben_rob_vacuum","con_rob_vacuum"
              ,"ben_driverless_cars","con_driverless_cars","ben_climate_res","con_climate_res","ben_face_border"
              ,"con_face_border","ben_welfare","con_welfare","ben_cancer","con_cancer","ben_socmedia_politad"
              ,"con_socmedia_politad","ben_virt_assist_health","con_virt_assist_health","ben_rob_care_assist"
              ,"con_rob_care_assist","ben_auto_weapons","con_auto_weapons","ben_educ","con_educ","ben_socmedia_consum"
              ,"con_socmedia_consum")
# reverse code all benefit and concern variables
analysis_data2[ , net_vars] = 5 - analysis_data2[ , net_vars]
# 
mutate(analysis_data2,
       net_ben_face_unlock = ben_face_unlock - con_face_unlock,
       net_ben_face_police = ben_face_police - con_face_police,
       net_ben_job = ben_job - con_job, 
      net_ben_loan = ben_loan - con_loan, 
      net_ben_virt_assist_speaker = ben_virt_assist_speaker - con_virt_assist_speaker,
      net_ben_rob_vacuum = ben_rob_vacuum - con_rob_vacuum,
      net_ben_driverless_cars = ben_driverless_cars - con_driverless_cars, 
      net_ben_climate_res = ben_climate_res - con_climate_res, 
      net_ben_face_border = ben_face_border - con_face_border, 
      net_ben_welfare = ben_welfare - con_welfare,
      net_ben_cancer = ben_cancer - con_cancer,
      net_ben_socmedia_politad = ben_socmedia_politad - con_socmedia_politad, 
      net_ben_virt_assist_health = ben_virt_assist_health - con_virt_assist_health, 
      net_ben_rob_care_assist = ben_rob_care_assist - con_rob_care_assist,
      net_ben_auto_weapons = ben_auto_weapons - con_auto_weapons, 
      net_ben_educ = ben_educ - con_educ,
      net_ben_socmedia_consum = ben_socmedia_consum - con_socmedia_consum
      ) -> analysis_data2
#  
new_net_vars <- c("net_ben_face_unlock","net_ben_face_police","net_ben_job","net_ben_loan"
              ,"net_ben_virt_assist_speaker","net_ben_rob_vacuum","net_ben_driverless_cars","net_ben_climate_res","net_ben_face_border"
              ,"net_ben_welfare","net_ben_cancer","net_ben_socmedia_politad","net_ben_virt_assist_health","net_ben_rob_care_assist"
              ,"net_ben_auto_weapons","net_ben_educ","net_ben_socmedia_consum")
#
# analysis_data3 includes net_benefit for all 17 techs (previous version analysis_data2 includes variables recoded and PCA Comp1 scores for diglit, techcomf
saveRDS(analysis_data2, "analysis_data3.rds")
#
# Open cleaned data file for regression analysis
analysis_data3 <- readRDS("analysis_data3.rds")
#
# Recoding variable Aware of individual technology to a dummy
#
analysis_data3$AWARE_MP_dummy	<-	analysis_data3$AWARE_MP
analysis_data3$AWARE_BC_dummy	<-	analysis_data3$AWARE_BC
analysis_data3$AWARE_PS_dummy	<-	analysis_data3$AWARE_PS
analysis_data3$AWARE_WEL_dummy	<-	analysis_data3$AWARE_WEL
analysis_data3$AWARE_JOB_dummy	<-	analysis_data3$AWARE_JOB
analysis_data3$AWARE_CAN_dummy	<-	analysis_data3$AWARE_CAN
analysis_data3$AWARE_LOAN_dummy	<-	analysis_data3$AWARE_LOAN
analysis_data3$AWARE_SMC_dummy	<-	analysis_data3$AWARE_SMC
analysis_data3$AWARE_SMP_dummy	<-	analysis_data3$AWARE_SMP
analysis_data3$AWARE_VASS_dummy	<-	analysis_data3$AWARE_VASS
analysis_data3$AWARE_VAH_dummy	<-	analysis_data3$AWARE_VAH
analysis_data3$AWARE_RVC_dummy	<-	analysis_data3$AWARE_RVC
analysis_data3$AWARE_RCA_dummy	<-	analysis_data3$AWARE_RCA
analysis_data3$AWARE_DC_dummy	<-	analysis_data3$AWARE_DC
analysis_data3$AWARE_AW_dummy	<-	analysis_data3$AWARE_AW
analysis_data3$AWARE_CCR_dummy	<-	analysis_data3$AWARE_CCR
analysis_data3$AWARE_VR_dummy	<-	analysis_data3$AWARE_VR
#
# vector with variables
aware_vars_dummy <- c("AWARE_MP_dummy","AWARE_BC_dummy","AWARE_PS_dummy","AWARE_WEL_dummy","AWARE_JOB_dummy","AWARE_CAN_dummy","AWARE_LOAN_dummy","AWARE_SMC_dummy"
                ,"AWARE_SMP_dummy","AWARE_VASS_dummy","AWARE_VAH_dummy","AWARE_RVC_dummy","AWARE_RCA_dummy","AWARE_DC_dummy","AWARE_AW_dummy","AWARE_CCR_dummy","AWARE_VR_dummy")
#
# create a function ro recoding into dummy all Aware variables
dummy_funct <- function(x) {
  case_when(
    {{x}} == 1 ~ 1,
    {{x}} == 2 | {{x}} == 3 ~ 0,
    TRUE ~ NA
  )
}
# use dummy_function across all variables
analysis_data3 %>%
  mutate(across(all_of(aware_vars_dummy), ~ dummy_funct(.x))) -> analysis_data3
#
# Recoding of Experience of technology variables
analysis_data3$EXP_MP_dummy	<-	analysis_data3$EXP_MP
analysis_data3$EXP_BC_dummy	<-	analysis_data3$EXP_BC
analysis_data3$EXP_PS_dummy	<-	analysis_data3$EXP_PS
analysis_data3$EXP_WEL_dummy	<-	analysis_data3$EXP_WEL
analysis_data3$EXP_JOB_dummy	<-	analysis_data3$EXP_JOB
analysis_data3$EXP_CAN_dummy	<-	analysis_data3$EXP_CAN
analysis_data3$EXP_LOAN_dummy	<-	analysis_data3$EXP_LOAN
analysis_data3$EXP_SMC_dummy	<-	analysis_data3$EXP_SMC
analysis_data3$EXP_SMP_dummy	<-	analysis_data3$EXP_SMP
analysis_data3$EXP_VASS_dummy	<-	analysis_data3$EXP_VASS
analysis_data3$EXP_VAH_dummy	<-	analysis_data3$EXP_VAH
analysis_data3$EXP_RVC_dummy	<-	analysis_data3$EXP_RVC
analysis_data3$EXP_VR_dummy	<-	analysis_data3$EXP_VR
#
exp_vars_dummy <- c("EXP_MP_dummy","EXP_BC_dummy","EXP_PS_dummy","EXP_WEL_dummy","EXP_JOB_dummy","EXP_CAN_dummy","EXP_LOAN_dummy","EXP_SMC_dummy"
                    ,"EXP_SMP_dummy","EXP_VASS_dummy","EXP_VAH_dummy","EXP_RVC_dummy","EXP_VR_dummy")
#
# create a function
dummy_funct1 <- function(x) {
  case_when(
    {{x}} == 1 | {{x}} == 2 ~ 1,
    {{x}} == 3 | {{x}} == 4 ~ 0,
    TRUE ~ NA
  )
}
# use dummy_function across all variables
analysis_data3 %>%
  mutate(across(all_of(exp_vars_dummy), ~ dummy_funct1(.x))) -> analysis_data3
#
# Social class recoding RS_NSSEC_SummaryCat
analysis_data3$RS_NSSEC_SummaryCat_factor <- factor(analysis_data3$RS_NSSEC_SummaryCat)
#
# Combine age bands 
library(forcats)
freq(analysis_data3$RS_NSSEC_SummaryCat_factor, cum = TRUE, total = TRUE)
analysis_data3 %>%
  mutate(RS_NSSEC_SummaryCat_new = fct_collapse(RS_NSSEC_SummaryCat_factor,
                                     "NSSEC_1-3" = c("1", "2"), 
                                     "NSSEC_4-5" = c("3", "4", "5"),
                                     "NSSEC_6-7" = c("6", "7")
  )) -> analysis_data3 
#
# dummy for NC SEC
analysis_data3$NSSEC_1_3 <- ifelse(analysis_data3$RS_NSSEC_SummaryCat_new=="NSSEC_1-3",1,0)
#
# save as analysis_data4
saveRDS(analysis_data3, "analysis_data4.rds")
#
#################################################################################
# 5. Combined regression plot
#################################################################################
# open analysis_data4 to run the code for the combined plot
#
analysis_data4 <- readRDS("analysis_data4.rds")
#
# new 17 dummy variables on whether respondent was asked about a technology
analysis_data4$AWARE_MP_asked	<-	analysis_data4$AWARE_MP
analysis_data4$AWARE_BC_asked	<-	analysis_data4$AWARE_BC
analysis_data4$AWARE_PS_asked	<-	analysis_data4$AWARE_PS
analysis_data4$AWARE_WEL_asked	<-	analysis_data4$AWARE_WEL
analysis_data4$AWARE_JOB_asked	<-	analysis_data4$AWARE_JOB
analysis_data4$AWARE_CAN_asked	<-	analysis_data4$AWARE_CAN
analysis_data4$AWARE_LOAN_asked	<-	analysis_data4$AWARE_LOAN
analysis_data4$AWARE_SMC_asked	<-	analysis_data4$AWARE_SMC
analysis_data4$AWARE_SMP_asked	<-	analysis_data4$AWARE_SMP
analysis_data4$AWARE_VASS_asked	<-	analysis_data4$AWARE_VASS
analysis_data4$AWARE_VAH_asked	<-	analysis_data4$AWARE_VAH
analysis_data4$AWARE_RVC_asked	<-	analysis_data4$AWARE_RVC
analysis_data4$AWARE_RCA_asked	<-	analysis_data4$AWARE_RCA
analysis_data4$AWARE_DC_asked	<-	analysis_data4$AWARE_DC
analysis_data4$AWARE_AW_asked	<-	analysis_data4$AWARE_AW
analysis_data4$AWARE_CCR_asked	<-	analysis_data4$AWARE_CCR
analysis_data4$AWARE_VR_asked	<-	analysis_data4$AWARE_VR
#
# vector with variables
aware_vars_asked <- c("AWARE_MP_asked","AWARE_BC_asked","AWARE_PS_asked","AWARE_WEL_asked","AWARE_JOB_asked","AWARE_CAN_asked","AWARE_LOAN_asked"
                      ,"AWARE_SMC_asked","AWARE_SMP_asked","AWARE_VASS_asked","AWARE_VAH_asked","AWARE_RVC_asked","AWARE_RCA_asked","AWARE_DC_asked"
                      ,"AWARE_AW_asked","AWARE_CCR_asked","AWARE_VR_asked")
#
# create a function for a new dummy variable (Aware Q asked or not)
dummy_funct2 <- function(x) {
  case_when(
    {{x}} == 1 | {{x}} == 2 | {{x}} == 3 | {{x}} == 4 ~ 1,
    TRUE ~ 0
  )
}
# use dummy_function across all variables
analysis_data4 %>%
  mutate(across(all_of(aware_vars_asked), ~ dummy_funct2(.x))) -> analysis_data4
#
# regression for each technology, 
# each regression uses a subset on the sample that was asked about the technology. 
subset_1 <- subset(analysis_data4, AWARE_VAH_asked == 1)
subset_1$AWARE <- subset_1$AWARE_VAH_dummy
subset_1$EXP <- subset_1$EXP_VAH_dummy
m1 <- lm(net_ben_virt_assist_health ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_1, weights=PV25_Weight)

subset_2 <- subset(analysis_data4, AWARE_VASS_asked == 1)
subset_2$AWARE <- subset_2$AWARE_VASS_dummy
subset_2$EXP <- subset_2$EXP_VASS_dummy
m2 <- lm(net_ben_virt_assist_speaker ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_2, weights=PV25_Weight)

subset_3 <- subset(analysis_data4, AWARE_SMC_asked == 1)
subset_3$AWARE <- subset_3$AWARE_SMC_dummy
subset_3$EXP <- subset_3$EXP_SMC_dummy
m3 <- lm(net_ben_socmedia_consum ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_3, weights=PV25_Weight)

subset_4 <- subset(analysis_data4, AWARE_SMP_asked == 1)
subset_4$AWARE <- subset_4$AWARE_SMP_dummy
subset_4$EXP <- subset_4$EXP_SMP_dummy
m4 <- lm(net_ben_socmedia_politad ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_4, weights=PV25_Weight)

subset_5 <- subset(analysis_data4, AWARE_VR_asked == 1)
subset_5$AWARE <- subset_5$AWARE_VR_dummy
subset_5$EXP <- subset_5$EXP_VR_dummy
m5 <- lm(net_ben_educ ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_5, weights=PV25_Weight)

subset_6 <- subset(analysis_data4, AWARE_CCR_asked == 1)
subset_6$AWARE <- subset_6$AWARE_CCR_dummy
m6 <- lm(net_ben_climate_res ~ age65_and_over + Male + Degree + AWARE + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_6, weights=PV25_Weight)

subset_7 <- subset(analysis_data4, AWARE_RVC_asked == 1)
subset_7$AWARE <- subset_7$AWARE_RVC_dummy
subset_7$EXP <- subset_7$EXP_RVC_dummy
m7 <- lm(net_ben_rob_vacuum ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_7, weights=PV25_Weight)

subset_8 <- subset(analysis_data4, AWARE_RCA_asked == 1)
subset_8$AWARE <- subset_8$AWARE_RCA_dummy
m8 <- lm(net_ben_rob_care_assist ~ age65_and_over + Male + Degree + AWARE + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_8, weights=PV25_Weight)

subset_9 <- subset(analysis_data4, AWARE_CAN_asked == 1)
subset_9$AWARE <- subset_9$AWARE_CAN_dummy
subset_9$EXP <- subset_9$EXP_CAN_dummy
m9 <- lm(net_ben_cancer ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
         , data = subset_9, weights=PV25_Weight)

subset_10 <- subset(analysis_data4, AWARE_LOAN_asked == 1)
subset_10$AWARE <- subset_10$AWARE_LOAN_dummy
subset_10$EXP <- subset_10$EXP_LOAN_dummy
m10 <- lm(net_ben_loan ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_10, weights=PV25_Weight)

subset_11 <- subset(analysis_data4, AWARE_MP_asked == 1)
subset_11$AWARE <- subset_11$AWARE_MP_dummy
subset_11$EXP <- subset_11$EXP_MP_dummy
m11 <- lm(net_ben_face_unlock ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_11, weights=PV25_Weight)

subset_12 <- subset(analysis_data4, AWARE_PS_asked == 1)
subset_12$AWARE <- subset_12$AWARE_PS_dummy
subset_12$EXP <- subset_12$EXP_PS_dummy
m12 <- lm(net_ben_face_police ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_12, weights=PV25_Weight)

subset_13 <- subset(analysis_data4, AWARE_BC_asked == 1)
subset_13$AWARE <- subset_13$AWARE_BC_dummy
subset_13$EXP <- subset_13$EXP_BC_dummy
m13 <- lm(net_ben_face_border ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_13, weights=PV25_Weight)

subset_14 <- subset(analysis_data4, AWARE_WEL_asked == 1)
subset_14$AWARE <- subset_14$AWARE_WEL_dummy
subset_14$EXP <- subset_14$EXP_WEL_dummy
m14 <- lm(net_ben_welfare ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_14, weights=PV25_Weight)

subset_15 <- subset(analysis_data4, AWARE_JOB_asked == 1)
subset_15$AWARE <- subset_15$AWARE_JOB_dummy
subset_15$EXP <- subset_15$EXP_JOB_dummy
m15 <- lm(net_ben_job ~ age65_and_over + Male + Degree + AWARE + EXP + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_15, weights=PV25_Weight)

subset_16 <- subset(analysis_data4, AWARE_AW_asked == 1)
subset_16$AWARE <- subset_16$AWARE_AW_dummy
m16 <- lm(net_ben_auto_weapons ~ age65_and_over + Male + Degree + AWARE + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_16, weights=PV25_Weight)

subset_17 <- subset(analysis_data4, AWARE_DC_asked == 1)
subset_17$AWARE <- subset_17$AWARE_DC_dummy
m17 <- lm(net_ben_driverless_cars ~ age65_and_over + Male + Degree + AWARE + TECHINFO + TECHINTE + digital_comp1 + techcomf7_comp1 + NSSEC_1_3
          , data = subset_17, weights=PV25_Weight)
#
# switch off scientific notations
options(scipen=999) 
obj <- dwplot(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17))
obj
obj2 <- as.data.frame(obj$data)
obj2

obj2$model <- fct_recode(obj2$model, "Virtual assistant: health" = "Model 1", "Virtual assistant: speaker" = "Model 2", "Social media ads: product" = "Model 3"
                         ,"Social media ads: political" = "Model 4","Simulation: VR education" = "Model 5", "Simulation: climate change" = "Model 6"
                         ,"Robotics: vacuum" = "Model 7", "Robotics: care assistant" = "Model 8","Predict: risk cancer" = "Model 9"
                         ,"Predict: loan" = "Model 10", "Face recognition: unlock" = "Model 11", "Face recognition: policing" = "Model 12"
                         ,"Face recognition: border" = "Model 13","Eligibility: welfare" = "Model 14", "Eligibility: jobs" = "Model 15"
                         ,"Autonomous: weapons" = "Model 16", "Autonomous: cars" = "Model 17")
#
unique(obj2$model)
#
# install.packages("scales")
library(scales)
#install.packages("ggpmisc")
library(ggpmisc)
ggplot(data = obj2, aes(x = estimate, y = model)) + 
  geom_point(color=ifelse(obj2$p.value<0.05, 'red', 'black')) +
  labs(caption = "*Statistically significant coefficients (p<0.05) are marked in red\n**The question on experience was not asked for all technologies") +
  theme(plot.caption = element_text(hjust=0)) +
  geom_vline(xintercept=0,linetype=3.5) +
  xlim(limits = c(-0.8, 0.8)) +
  facet_wrap(~ term, nrow = 2, labeller = as_labeller(c(age65_and_over = "Aged 65+",
                 Male = "Male",
                 Degree = "Degree",
                 AWARE = "Awareness of technology",
                 EXP = "Experience with technology**",
                 TECHINFO = "Tech informed",
                 TECHINTE= "Tech interested",
                 digital_comp1= "Digital literacy",
                 techcomf7_comp1= "Tech comfortable",
                 NSSEC_1_3 = "Socio-economic class 1-3"))) +
  scale_x_continuous(labels = label_number(accuracy = 0.1),
  ) + 
  xlab("Coefficient Estimates (higher values = more net benefit)") + ylab("AI Technologies") +
  ggtitle("Predictors* of net benefit") +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = c(0.82, 0.00),
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank()
  )
#

