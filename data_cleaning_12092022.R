library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)

# load datasets
setwd("/xxx/1819-0119/Linked Data/Reference files/")
mother_child_link_data <- read.csv("Mother_Child_lookup_file.csv")
setwd("/xxx/1819-0119/Linked Data")
#chi_child_data <- read.csv("CHI_Children.csv.xz")
#gpoo_data <- read.csv("GPOOH_Children.csv")
smr02_data <- read.csv("SMR02_Mothers.csv.xz")
smr11_data <- read.csv("SMR11_Children.csv.xz")
smr01_data <- read.csv("SMR01_Children.csv.xz")
ssbid_data_1 <- read.csv("SSBID_Mothers.csv.xz")## old file
setwd("/xxx/1819-0119/Linked Data/")
ssbid_data_2 <- read.csv("SSBID_Mothers_202212.csv")
sbr_data <- read.csv("SBR_Children.csv.xz")
deaths_children <- read.csv("Deaths_Children.csv.xz")
deaths_mother <- read.csv("Deaths_Mothers.csv.xz")
stillbirths_data <- read.csv("Stillbirths_Mothers.csv.xz")

setwd("/xxx/1819-0119/Research/adeniyi")

##########
# data cleaning
smr11_data$DOB <- ymd(smr11_data$DOB)
range(smr11_data$DOB)
smr02_data$DATE_OF_DELIVERY <- ymd(smr02_data$DATE_OF_DELIVERY)
range(smr02_data$DATE_OF_DELIVERY, na.rm = TRUE)
sbr_data$BABY_DATE_OF_BIRTH <- ymd(sbr_data$BABY_DATE_OF_BIRTH)
range(sbr_data$BABY_DATE_OF_BIRTH, na.rm = TRUE)

smr02_data$DATE_OF_DELIVERY <- ymd(smr02_data$DATE_OF_DELIVERY)
smr02_data <-mutate(smr02_data, year_del = year(DATE_OF_DELIVERY))
year_del <- as.data.frame(table(smr02_data$year_del)) # request to be released

####


# analysis
analysis_data <- filter(smr02_data, DATE_OF_DELIVERY<="2019-12-31")
addmargins(table(analysis_data$year_del))
addmargins(table(analysis_data$ESTIMATED_GESTATION))
analysis_data <- filter(analysis_data, ESTIMATED_GESTATION>="24")
addmargins(table(analysis_data$ESTIMATED_GESTATION)) 
analysis_data <- filter(analysis_data, ESTIMATED_GESTATION<"37")
addmargins(table(analysis_data$ESTIMATED_GESTATION)) 
length(which(is.na(analysis_data$ESTIMATED_GESTATION)))
analysis_data <- filter(analysis_data, CONDITION_ON_DISCHARGE==3)
addmargins(table(analysis_data$CONDITION_ON_DISCHARGE))
analysis_data <- filter(analysis_data, NUM_OF_BIRTHS_THIS_PREGNANCY==1)
addmargins(table(analysis_data$NUM_OF_BIRTHS_THIS_PREGNANCY))
analysis_data <- filter(analysis_data, is.na(OUTCOME_OF_PREGNANCY_BABY_2))##### 
addmargins(table(analysis_data$OUTCOME_OF_PREGNANCY_BABY_1)) 
#analysis_data <- filter(analysis_data, OUTCOME_OF_PREGNANCY_BABY_1=="1" |
#                          OUTCOME_OF_PREGNANCY_BABY_1=="3" | OUTCOME_OF_PREGNANCY_BABY_1=="4" |
#                          OUTCOME_OF_PREGNANCY_BABY_1=="5") 
##skipping the above lines
analysis_data <- filter(analysis_data, OUTCOME_OF_PREGNANCY_BABY_1=="1" |OUTCOME_OF_PREGNANCY_BABY_1=="2" |
                        OUTCOME_OF_PREGNANCY_BABY_1=="3" | OUTCOME_OF_PREGNANCY_BABY_1=="4" |
                         OUTCOME_OF_PREGNANCY_BABY_1=="5") 


addmargins(table(analysis_data$PRESENTATION_AT_DELIVERY_B1)) # request to be released
addmargins(table(analysis_data$MODE_OF_DELIVERY_BABY_1))
addmargins(table(analysis_data$PRESENTATION_AT_DELIVERY_B1,analysis_data$MODE_OF_DELIVERY_BABY_1))
# 
analysis_data1<-filter(analysis_data, MODE_OF_DELIVERY_BABY_1=="5" | MODE_OF_DELIVERY_BABY_1=="6")
analysis_data1<-filter(analysis_data1, PRESENTATION_AT_DELIVERY_B1!="4")
analysis_data <-filter(analysis_data, PRESENTATION_AT_DELIVERY_B1=="4")
analysis_data1 <-bind_rows(analysis_data, analysis_data1)
rm(analysis_data1)
addmargins(table(analysis_data$ADMISSION_REASON)) # request to be released
analysis_data <- mutate(analysis_data, OPERATIVE_DEL_3chars = str_extract(INDICATION_FOR_OPERATIVE_DEL, "[0-9a-zA-Z]{3}"))
addmargins(table(analysis_data$OPERATIVE_DEL_3chars))
addmargins(table(analysis_data$PRESENTATION_AT_DELIVERY_B1,analysis_data$MODE_OF_DELIVERY_BABY_1))

addmargins(table(analysis_data$OUTCOME_OF_PREGNANCY_BABY_1,analysis_data$MODE_OF_DELIVERY_BABY_1))

# 
# ##create separate data frame to indicate livebirths and stillbirths
# analysis_data_livebirth <- filter(analysis_data, OUTCOME_OF_PREGNANCY_BABY_1=="1" |
#                                     OUTCOME_OF_PREGNANCY_BABY_1=="3" | OUTCOME_OF_PREGNANCY_BABY_1=="4" |
#                                     OUTCOME_OF_PREGNANCY_BABY_1=="5") 
# analysis_data_livebirth <- mutate(analysis_data_livebirth,livebirth_smr02=1) 
# analysis_data_livebirth <- select(analysis_data_livebirth,MotherID,livebirth_smr02)
# analysis_data_still <- filter(analysis_data, OUTCOME_OF_PREGNANCY_BABY_1=="2" )# only included stillbirth here
# # 785
# analysis_data_still <- mutate(analysis_data_still,stillbirth_smr02=1) 
# analysis_data_still <- select(analysis_data_still,MotherID,stillbirth_smr02) 
# 
# ##add the data frame to the main data
# analysis_data<-left_join(analysis_data,analysis_data_livebirth)
# analysis_data<-left_join(analysis_data,analysis_data_still)


####
# adding prom variables
addmargins(table(analysis_data$INDUCTION_OF_LABOUR))
addmargins(table(analysis_data$INDICATION_FOR_OPERATIVE_DEL))
# induciton of labour 0,9: no and 1-8:yes
analysis_data <- mutate(analysis_data,  INDUCTION_OF_LABOUR_b = 
                          if_else(INDUCTION_OF_LABOUR=="0" | INDUCTION_OF_LABOUR=="9" | is.na(INDUCTION_OF_LABOUR),
                                  "No", "Yes"))
addmargins(table(analysis_data$INDUCTION_OF_LABOUR_b))
analysis_data <- mutate(analysis_data, MAIN_CONDITION_3chars = str_extract(MAIN_CONDITION, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data, OTHER_CONDITION_1_3chars = str_extract(OTHER_CONDITION_1, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data, OTHER_CONDITION_2_3chars = str_extract(OTHER_CONDITION_2, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data, OTHER_CONDITION_3_3chars = str_extract(OTHER_CONDITION_3, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data, OTHER_CONDITION_4_3chars = str_extract(OTHER_CONDITION_4, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data, OTHER_CONDITION_5_3chars = str_extract(OTHER_CONDITION_5, "[0-9a-zA-Z]{3}"))
analysis_data <- mutate(analysis_data,  PROM = 
                          if_else(OPERATIVE_DEL_3chars=="O42" | MAIN_CONDITION_3chars=="O42" | OTHER_CONDITION_1_3chars=="O42" |
                                    OTHER_CONDITION_2_3chars=="O42" |OTHER_CONDITION_3_3chars=="O42" |
                                    OTHER_CONDITION_4_3chars=="O42" |OTHER_CONDITION_5_3chars=="O42",
                                  "Yes", "No"))
analysis_data <- mutate(analysis_data,  PROM = if_else(is.na(PROM) | PROM == "No","No", "Yes"))
addmargins(table(analysis_data$PROM))
addmargins(table(analysis_data$DURATION_OF_LABOUR))
analysis_data <- mutate(analysis_data,  DURATION_OF_LABOUR_c = if_else(DURATION_OF_LABOUR=="0", "0", 
                                                                       if_else(DURATION_OF_LABOUR=="99", "unknown", ">0")))
addmargins(table(analysis_data$DURATION_OF_LABOUR_c))
addmargins(table(analysis_data$MODE_OF_DELIVERY_BABY_1))
analysis_data <- mutate(analysis_data,  MODE_OF_DELIVERY_b = if_else(
  MODE_OF_DELIVERY_BABY_1 == "0" | MODE_OF_DELIVERY_BABY_1 == "1" | MODE_OF_DELIVERY_BABY_1 == "5" | 
    MODE_OF_DELIVERY_BABY_1 == "6", "vaginal", if_else(MODE_OF_DELIVERY_BABY_1 == "7", "Elective c-section", "Emergency c- section")))
addmargins(table(analysis_data$MODE_OF_DELIVERY_b))

analysis_data <- mutate(analysis_data, Group1 = if_else(PROM=="Yes", 1, 0))
addmargins(table(analysis_data$Group1))
analysis_data <- mutate(analysis_data, Group2 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="Yes", 1, 0))
addmargins(table(analysis_data$Group2))
analysis_data <- mutate(analysis_data, Group3 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="No" & 
                                                          MODE_OF_DELIVERY_b == "vaginal", 1, 0))
addmargins(table(analysis_data$Group3))
analysis_data <- mutate(analysis_data, Group4 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="No" & 
                                                          DURATION_OF_LABOUR_c == "0" & MODE_OF_DELIVERY_b != "vaginal", 1, 0))
addmargins(table(analysis_data$Group4))
analysis_data <- mutate(analysis_data, Group5 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="No" & 
                                                          DURATION_OF_LABOUR_c == ">0" & MODE_OF_DELIVERY_b != "vaginal", 1, 0))
addmargins(table(analysis_data$Group5))
analysis_data <- mutate(analysis_data, Group6 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="No" & 
                                                          DURATION_OF_LABOUR_c == "unknown" & 
                                                          MODE_OF_DELIVERY_b == "Elective c-section", 1, 0))
addmargins(table(analysis_data$Group6))
analysis_data <- mutate(analysis_data, Group7 = if_else(PROM=="No" & INDUCTION_OF_LABOUR_b =="No" & 
                                                          DURATION_OF_LABOUR_c == "unknown" & 
                                                          MODE_OF_DELIVERY_b == "Emergency c- section", 1, 0))
addmargins(table(analysis_data$Group7))

analysis_data$delivery_classification[analysis_data$Group1==1]<-"Spontaneous with PROM"
analysis_data$delivery_classification[analysis_data$Group2==1]<-"Medically indicated induction"
analysis_data$delivery_classification[analysis_data$Group4==1 | analysis_data$Group6==1]<-"Medically indicated c-section"
analysis_data$delivery_classification[analysis_data$Group3==1 | analysis_data$Group5==1 | 
                                        analysis_data$Group7==1]<-"Spontaneous without PROM"
addmargins(table(analysis_data$delivery_classification))

####not that useful
# analysis with amaya
analysis_data1<-filter(analysis_data, MODE_OF_DELIVERY_BABY_1=="5" | MODE_OF_DELIVERY_BABY_1=="6" |
                         MODE_OF_DELIVERY_BABY_1=="8" | MODE_OF_DELIVERY_BABY_1=="9")
addmargins(table(analysis_data1$MODE_OF_DELIVERY_BABY_1))
addmargins(table(analysis_data1$OPERATIVE_DEL_3chars))
analysis_data1<-filter(analysis_data1, OPERATIVE_DEL_3chars=="O42")
addmargins(table(analysis_data1$MODE_OF_DELIVERY_BABY_1))

analysis_data2<-filter(analysis_data, ADMISSION_REASON == "23")
analysis_data2<-filter(analysis_data2, MODE_OF_DELIVERY_BABY_1=="5" | MODE_OF_DELIVERY_BABY_1=="6" |
                         MODE_OF_DELIVERY_BABY_1=="8" | MODE_OF_DELIVERY_BABY_1=="9")
addmargins(table(analysis_data2$MODE_OF_DELIVERY_BABY_1))

analysis_data3<-filter(analysis_data, OPERATIVE_DEL_3chars=="O42")
addmargins(table(analysis_data3$MODE_OF_DELIVERY_BABY_1))
 


####
deaths_children <- select(deaths_children, ChildID, DATE_OF_DEATH, YEAR_AND_MONTH_OF_DELIVERY)
deaths_children$DATE_OF_DEATH <- ymd(deaths_children$DATE_OF_DEATH)
deaths_children$YEAR_AND_MONTH_OF_DELIVERY <- ym(deaths_children$YEAR_AND_MONTH_OF_DELIVERY)
deaths_children <- mutate(deaths_children, year_del = year(YEAR_AND_MONTH_OF_DELIVERY))
mother_child_link_data <- distinct(mother_child_link_data, ChildID, .keep_all = TRUE)
deaths_breech <- left_join(deaths_children, mother_child_link_data)
deaths_breech <- left_join(analysis_data, deaths_breech)
deaths_breech <- mutate(deaths_breech, days_since_delivery = DATE_OF_DEATH-DATE_OF_DELIVERY)
deaths_breech$days_since_delivery<-as.numeric(deaths_breech$days_since_delivery)
deaths_breech$days_since_delivery <- as.character(cut(deaths_breech$days_since_delivery,c(-Inf,-1,6,28, Inf),
                                                      labels=c("<0", "0-6", "7-27", ">28")))
addmargins(table(deaths_breech$days_since_delivery))
#addmargins(table(deaths_breech$days_since_delivery,deaths_breech$ESTIMATED_GESTATION))
#addmargins(table(deaths_breech$days_since_delivery,deaths_breech$delivery_classification))
addmargins(table(deaths_breech$ESTIMATED_GESTATION,deaths_breech$days_since_delivery))
addmargins(table(deaths_breech$delivery_classification,deaths_breech$days_since_delivery))
temp<-select(deaths_breech, MotherID,ChildID,DATE_OF_DEATH, DATE_OF_DELIVERY,days_since_delivery )

neonatal_stay <- select(smr11_data, ChildID, INTENSIVE_TOTAL, YEAR_OF_ADMISSION)
neonatal_stay$YEAR_OF_ADMISSION <- as.numeric(neonatal_stay$YEAR_OF_ADMISSION)
colnames(neonatal_stay)[3] <- "year_del"
neonatal_stay <- left_join(deaths_breech, neonatal_stay)
neonatal_stay$INTENSIVE_TOTAL <- as.character(cut(neonatal_stay$INTENSIVE_TOTAL,c(-Inf,-1,6,28, Inf),
                                                      labels=c("<0", "0-6", "7-27", ">28")))
addmargins(table(neonatal_stay$INTENSIVE_TOTAL,neonatal_stay$delivery_classification))
colnames(neonatal_stay)
# stillbirths summary
stillbirths_data <- mutate(stillbirths_data, PRIMARY_CAUSE_OF_DEATH_3chars = str_extract(PRIMARY_CAUSE_OF_DEATH, "[0-9a-zA-Z]{3}"))
summ_stillbirths <- as.data.frame(table(stillbirths_data$PRIMARY_CAUSE_OF_DEATH_3chars))
##########################################################

temp<-select(sbr_data, ChildID,BABY_DATE_OF_BIRTH)
temp<-distinct(temp,ChildID,.keep_all = TRUE)
mother_child_link_data<-left_join(mother_child_link_data,temp)
temp<-select(smr11_data,ChildID,DOB)
temp<-distinct(temp,ChildID,.keep_all = TRUE)
mother_child_link_data<-left_join(mother_child_link_data,temp)
mother_child_link_data$BABY_DATE_OF_BIRTH<-
  coalesce(mother_child_link_data$BABY_DATE_OF_BIRTH,
           mother_child_link_data$DOB)

temp<-select(smr02_data, MotherID,DATE_OF_DELIVERY)
temp<-distinct(temp,MotherID,.keep_all = TRUE)
mother_child_link_data<-left_join(mother_child_link_data,temp)
length(which(!is.na(mother_child_link_data$BABY_DATE_OF_BIRTH)))
length(which(!is.na(mother_child_link_data$DATE_OF_DELIVERY)))
mother_child_link_data <- mutate(mother_child_link_data, year_del=year(BABY_DATE_OF_BIRTH))
mother_child_link_data <- distinct(mother_child_link_data, MotherID,
                                   ChildID,BABY_DATE_OF_BIRTH,
                                   DATE_OF_DELIVERY,.keep_all = TRUE)

##Adeniyi
##length of stay at ICU
neonatal_smr11 <- mutate(smr11_data, smr11_presence=1)

neonatal_smr11 <- select(neonatal_smr11, ChildID, DOB, YEAR_OF_ADMISSION, INTENSIVE_TOTAL,NEO_NATAL_STAY, smr11_presence)
colnames(neonatal_smr11)[c(2:5)] <- c("dob_smr11", "adm_year_smr11", "intensive_tot_smr11", "neonatal_stay_smr11")
length(which(neonatal_smr11$smr11_presence==1)) ## 
neonatal_smr11 <- distinct(neonatal_smr11,ChildID, .keep_all = TRUE)
length(which(neonatal_smr11$smr11_presence==1)) ##


neonatal_sbr<-select(sbr_data, ChildID, BABY_DATE_OF_BIRTH,IC_ICU1_DAYS, IC_ICU2_DAYS, IC_SCBU_DAYS )
neonatal_sbr<-filter(neonatal_sbr, !is.na(IC_ICU1_DAYS) | !is.na(IC_ICU2_DAYS) | !is.na(IC_SCBU_DAYS) )##filter out those without icu
colnames(neonatal_sbr)[c(2)] <- c("dob_sbr")
#neonatal_sbr <- mutate(neonatal_sbr, year_del= year(dob_sbr))
neonatal_sbr <- mutate(neonatal_sbr, sbr_presence=1)
length(which(neonatal_sbr$sbr_presence==1)) ##
neonatal_sbr <- distinct(neonatal_sbr,ChildID, .keep_all = TRUE)
length(which(neonatal_sbr$sbr_presence==1)) ## 




##from UA

##Linked papers

# table(linked_data$linked)
neonatal_days <- select(deaths_breech,-ChildID)##drop childID attached to death records
#neonatal_days<-mutate(neonatal_days,id=seq(1:xxx)) #
neonatal_days<-mutate(neonatal_days,id=seq(1:xxx) #or 
#temp<-select(analysis_data,MotherID,year_del,id)
temp<-select(neonatal_days,MotherID,year_del,id)
temp <- left_join(temp,mother_child_link_data)
temp<-distinct(temp,MotherID,ChildID,.keep_all = TRUE)
length(which(!is.na(temp$ChildID)))
temp<-filter(temp,!is.na(ChildID))
temp<-mutate(temp,linked=1)
linked_data<-temp
addmargins(table(temp$year_del))
# analysis_data_linked <- left_join(analysis_data,linked_data)
# analysis_data_linked <- mutate(analysis_data_linked,link12=if_else(linked=="1" , "linked", "not_linked", "nas"))
# table(analysis_data_linked$link12)
# table(analysis_data_linked$linked)

table(linked_data$linked)
linked_data<-select(linked_data, MotherID,id,linked,ChildID)
#linked_data<-distinct(linked_data,id,.keep_all = TRUE)
neonatal_days <-left_join(neonatal_days,linked_data,by = c("MotherID","id"))
#analysis_data_linked <-distinct(analysis_data_linked ,id,.keep_all = TRUE)
neonatal_days <-mutate(neonatal_days,link12=if_else(linked=="1" , "linked", "not_linked", "nas"))
table(neonatal_days$link12)
table(neonatal_days$linked)


#neonatal_days <- select(deaths_breech,-ChildID) ##remove childID attached to death records
#neonatal_days <- distinct(neonatal_days, MotherID,DATE_OF_DELIVERY,.keep_all = TRUE )
#neonatal_days <- left_join(neonatal_days, mother_child_link_data,by = c("MotherID")) ####
#neonatal_days <- left_join(neonatal_days, mother_child_link_data) 
length(which(!is.na(neonatal_days$ChildID))) ###TO BE RESOLVED FIRST
length(unique(neonatal_days$ChildID)) ##
length(unique(neonatal_days$MotherID)) ##

neonatal_days <- left_join(neonatal_days,neonatal_smr11)
#neonatal_days <- select(neonatal_days, -intensive_tot_smr11, -neonatal_stay_smr11)
length(which(neonatal_days$smr11_presence==1)) ##
#neonatal_days <- distinct(neonatal_days, ChildID, dob_smr11, .keep_all = TRUE)
length(which(neonatal_days$smr11_presence==1)) ## 
length(unique(neonatal_days$ChildID)) ## 
length(unique(neonatal_days$MotherID)) ## 
##check percent linked
addmargins(table(neonatal_days$smr11_presence,neonatal_days$year_del))
round(100*prop.table(table(neonatal_days$smr11_presence,neonatal_days$year_del),1),1)

neonatal_days <- left_join(neonatal_days,neonatal_sbr)
length(which(neonatal_days$sbr_presence==1))
#neonatal_days <- distinct(neonatal_days, ChildID, dob_smr11, .keep_all = TRUE)
length(which(neonatal_days$sbr_presence==1)) 
length(unique(neonatal_days$ChildID)) ##
length(unique(neonatal_days$MotherID)) ## 

##check percent linked
addmargins(table(neonatal_days$sbr_presence,neonatal_days$year_del))
round(100*prop.table(table(neonatal_days$sbr_presence,neonatal_days$year_del),1),1)


#neonatal_days <- mutate(neonatal_days, year_del = year(dob_smr11))
neonatal_days1 <- left_join(neonatal_days,neonatal_sbr)


neonatal_days$smr11_presence[is.na(neonatal_days$smr11_presence)] <- 0



##ICU Data
#neonatal_days <- mutate(neonatal_days,  neonate_smr11=if_else(smr11_presence!=1|is.na(smr11_presence) , 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_smr11=if_else(neonatal_stay_smr11==0|is.na(neonatal_stay_smr11) , 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_sbu=if_else(IC_SCBU_DAYS==0  |is.na(IC_SCBU_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_icu1=if_else(IC_ICU1_DAYS==0 |is.na(IC_ICU1_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_icu2=if_else(IC_ICU2_DAYS==0 |is.na(IC_ICU2_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_smr11_28=if_else((neonatal_stay_smr11==0 | neonatal_stay_smr11>27) |is.na(neonatal_stay_smr11), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_sbu28=if_else((IC_SCBU_DAYS==0 | IC_SCBU_DAYS>27) |is.na(IC_SCBU_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_icu128=if_else((IC_ICU1_DAYS==0 | IC_ICU1_DAYS>27) |is.na(IC_ICU1_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_icu228=if_else((IC_ICU2_DAYS==0 | IC_ICU2_DAYS>27) |is.na(IC_ICU2_DAYS), 0 , 1))
neonatal_days <- mutate(neonatal_days,  neonate_all=if_else(neonate_smr11==1|neonate_sbu==1|neonate_icu1==1|neonate_icu2==1, 1 , 0))
#neonatal_days <- mutate(neonatal_days,  neonate_28=if_else(neonate_smr11==1|neonate_sbu28==1|neonate_icu128==1|neonate_icu228==1, 1 , 0))
neonatal_days <- mutate(neonatal_days,  neonate_28=if_else(neonate_smr11_28==1|neonate_sbu28==1|neonate_icu128==1|neonate_icu228==1, 1 , 0))
addmargins(table(neonatal_days$delivery_classification,neonatal_days$neonate_all))
addmargins(table(neonatal_days$delivery_classification,neonatal_days$neonate_28))
round(100*prop.table(table(neonatal_days$delivery_classification,neonatal_days$neonate_28),1),1)

table(neonatal_days$NEONATAL_INDICATOR_BABY_1)
table(neonatal_days$NEONATAL_INDICATOR_BABY_1, neonatal_days$neonate_28)

# icu_smr01<-select(smr01_data, ChildID, SPECIALTY, ADMISSION_DATE,LENGTH_OF_STAY )
# icu_smr01<-filter(icu_smr01, SPECIALTY=="CC" )## select only those on intensive admission
# icu_smr01$ADMISSION_DATE<-ymd(icu_smr01$ADMISSION_DATE)
# colnames(icu_smr01)[2] <- "specialty_smr01"
# colnames(icu_smr01)[3] <- "adm_date_smr01"
# colnames(icu_smr01)[4] <- "los_smr01"

# neonatal_days <- left_join(neonatal_days,icu_smr01)
# neonatal_days<-mutate (neonatal_days, days_b4_adm=adm_date_smr01 - dob_smr11)
# table(neonatal_days$neonatal_days$adm_date_smr01)
# table(neonatal_days$days_b4_adm, neonatal_days$adm_date_smr01)
# table(neonatal_days$days_b4_adm)

# neonatal_days <- mutate(neonatal_days,  neonate_smr11=if_else(smr11_presence=="1"|!is.na , "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_sbu=if_else(IC_SCBU_DAYS>0 & IC_SCBU_DAYS<100, "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_icu1=if_else(IC_ICU1_DAYS>"0" & IC_ICU1_DAYS<"100", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_icu2=if_else(IC_ICU2_DAYS>"0" & IC_ICU2_DAYS<"100", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_sbu28=if_else(IC_SCBU_DAYS>"0" & IC_SCBU_DAYS<"28", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_icu128=if_else(IC_ICU1_DAYS>"0" & IC_ICU1_DAYS<"28", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_icu228=if_else(IC_ICU2_DAYS>"0" & IC_ICU2_DAYS<"28", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_all=if_else(neonate_smr11=="1"|neonate_sbu=="1"|neonate_icu1=="1"|neonate_icu2=="1", "1", "0"))
# neonatal_days <- mutate(neonatal_days,  neonate_28=if_else(neonate_smr11=="1"|neonate_sbu28=="1"|neonate_icu128=="1"|neonate_icu228=="1", "1", "0"))

##merging files and inserting "presence" for relevant files
# temp2<-mutate(neonatal_stay, neonatal_stay_data=1)
# length(unique(neonatal_stay$MotherID))
# length(unique(neonatal_stay$ChildID))#
# temp3<-mutate(deaths_breech, deaths_breech_data=1)
# length(unique(deaths_breech$MotherID))#
# length(unique(deaths_breech$ChildID))##
# temp2<-select(temp2,ChildID, MotherID, neonatal_stay_data )
# temp3<-select(temp3,ChildID, MotherID, deaths_breech_data )
# neonatal_days <- left_join(neonatal_days,temp2)
# neonatal_days <- left_join(neonatal_days,temp3)
# length(unique(neonatal_days$MotherID))#
# length(unique(neonatal_days$ChildID))##


neonatal_days_y <- distinct(neonatal_days, id,.keep_all = TRUE)
fwrite(neonatal_days_y, file="Breech_data_130922_still.csv", sep=",")

neonatal_days1 <- filter(neonatal_days,ADMISSION_DATE>="2003-01-01")
table(neonatal_days1)
analysis_data_u <- select(neonatal_days1,ChildID)
analysis_data_u <- mutate(analysis_data_u, presence=1)
analysis_data_u<-left_join(smr01_data,analysis_data_u)
analysis_data_u<-drop_na(analysis_data_u,presence)
analysis_data_u <- distinct(analysis_data_u, ChildID,.keep_all = TRUE)

analysis_data_u

##all children from SMR01
adm_smr01<-select(smr01_data, ChildID, SPECIALTY, ADMISSION_DATE,LENGTH_OF_STAY )
#adm_smr01<-filter(adm_smr01, SPECIALTY=="CC" )## select only those on intensive admission
adm_smr01 <- filter(adm_smr01,  ADMISSION_DATE<="2019-12-31")
##ADMISSION_DATE>="2000-01-01" &
adm_smr01$ADMISSION_DATE<-ymd(adm_smr01$ADMISSION_DATE)
colnames(adm_smr01)[2] <- "specialty_smr01"
colnames(adm_smr01)[3] <- "adm_date_smr01"
colnames(adm_smr01)[4] <- "los_smr01"

# obtain dob from SBR and SMR11
adm_smr11<-select(smr11_data, ChildID, DOB)
colnames(adm_smr11)[2] <- "dob_smr11"
adm_sbr<-select(sbr_data, ChildID, BABY_DATE_OF_BIRTH)
colnames(adm_sbr)[2] <- "dob_sbr"
adm_smr01<-left_join(adm_smr01,adm_smr11)
adm_smr01<-left_join(adm_smr01,adm_sbr)
length(which(!is.na(adm_smr01$adm_date_smr01)))
#length(which(is.na(adm_smr01$DOB)))
length(which(is.na(adm_smr01$dob_sbr)))
length(which(is.na(adm_smr01$dob_smr11)))#

length(which(!is.na(neonatal_days$DATE_OF_DELIVERY)))#

##merge the dob in SBR and SMR11 and the nd admission data in SMR01 to the neonate data
adm_smr01 <- distinct(adm_smr01, ChildID, adm_date_smr01, .keep_all = TRUE)
adm_smr01 <- arrange(adm_smr01, ChildID, adm_date_smr01)
adm_smr01 <- distinct(adm_smr01, ChildID, .keep_all = TRUE)
neonatal_days<-left_join(neonatal_days,adm_smr01, by = "ChildID")
length(which(!is.na(neonatal_days$DATE_OF_DELIVERY)))
length(which(!is.na(neonatal_days$adm_date_smr01)))
neonatal_days<-mutate (neonatal_days, adm_minus_dob=adm_date_smr01 -DATE_OF_DELIVERY)
neonatal_days$neo_days[neonatal_days$adm_minus_dob<28]=1
neonatal_days$neo_days[neonatal_days$adm_minus_dob>=28]=0
neonatal_days$neo_days[is.na(neonatal_days$adm_minus_dob)]=2

table(neonatal_days$adm_minus_dob)
table(neonatal_days$neo_days)
##obtaining DOB from SMRO" using date of delivery
#smr02_data_dod <- select(smr02_data, MotherID, DATE_OF_DELIVERY)
#neonatal_days <-left_join(neonatal_days, smr02_data_dod)

#Neonatal admission into SCBU using SBR & SMR01
addmargins(table(neonatal_days$neonate_all))
round(100*prop.table(table(neonatal_days$neonate_all)),1)

addmargins(table(neonatal_days$delivery_classification,neonatal_days$neonate_all))
round(100*prop.table(table(neonatal_days$delivery_classification,neonatal_days$neonate_all),1),1)
addmargins(table(neonatal_days$delivery_classification,neonatal_days$neonate_28))
round(100*prop.table(table(neonatal_days$delivery_classification,neonatal_days$neonate_28),1),1)

##Neonate SMR01 admission using date of delivery in SMR02 as DOB
addmargins(table(neonatal_days$neo_days))
round(100*prop.table(table(neonatal_days$neonate_28)),1)

##Groups versus Neonate SMR01 admission using date of delivery in SMR02 as DOB
addmargins(table(neonatal_days$delivery_classification,neonatal_days$neo_days))
round(100*prop.table(table(neonatal_days$delivery_classification,neonatal_days$neo_days),1),1)



###Linked versus unlinked
analysis_data<-mutate(analysis_data,id=seq(1:7583))
temp<-select(analysis_data,MotherID,year_del,id)
temp <- left_join(temp,mother_child_link_data)
temp<-distinct(temp,MotherID,ChildID,.keep_all = TRUE)
length(which(!is.na(temp$ChildID)))
temp<-filter(temp,!is.na(ChildID))
temp<-mutate(temp,linked=1)
linked_data<-temp
addmargins(table(temp$year_del))

length(which(temp$year_del<2003))
tt<-select(smr11_data,ChildID,DOB)
tt<-mutate(tt,year_del=year(DOB))
temp<-left_join(temp,tt)

tt<-as.data.frame(table(analysis_data$year_del))
colnames(tt)[2]<-"cohort_data"
tt<-mutate(tt,table(temp$year_del))
colnames(tt)[3]<-"linked_mothers"
tt<-mutate(tt,table(mother_child_link_data$year_del))
colnames(tt)[4]<-"original_data_provided"
tt<-mutate(tt,prop_linked_data=round(100*linked_mothers/cohort_data,1))

addmargins(table(analysis_data$year_del))
write.csv(tt, "summary_linked_data.csv")


##from UA
# table(linked_data$linked)
analysis_data<-mutate(analysis_data,id=seq(1:7583))
temp<-select(analysis_data,MotherID,year_del,id)
temp <- left_join(temp,mother_child_link_data)
temp<-distinct(temp,MotherID,ChildID,.keep_all = TRUE)
length(which(!is.na(temp$ChildID)))
temp<-filter(temp,!is.na(ChildID))
temp<-mutate(temp,linked=1)
linked_data<-temp
addmargins(table(temp$year_del))
# analysis_data_linked <- left_join(analysis_data,linked_data)
# analysis_data_linked <- mutate(analysis_data_linked,link12=if_else(linked=="1" , "linked", "not_linked", "nas"))
# table(analysis_data_linked$link12)
# table(analysis_data_linked$linked)

table(linked_data$linked)
linked_data<-select(linked_data, MotherID,id,linked)
#linked_data<-distinct(linked_data,id,.keep_all = TRUE)
analysis_data_linked <-left_join(analysis_data,linked_data,by = c("MotherID","id"))
#analysis_data_linked <-distinct(analysis_data_linked ,id,.keep_all = TRUE)
analysis_data_linked <-mutate(analysis_data_linked,link12=if_else(linked=="1" , "linked", "not_linked", "nas"))
table(analysis_data_linked$link12)
table(analysis_data_linked$linked)
##write.csv(analysis_data_linked, "analysis_data_linked.csv")


addmargins(table(analysis_data$OUTCOME_OF_PREGNANCY_BABY_1,analysis_data$PRESENTATION_AT_DELIVERY_B1))
addmargins(table(analysis_data$OUTCOME_OF_PREGNANCY_BABY_1,analysis_data$ADMISSION_REASON))

addmargins(table(analysis_data$OUTCOME_OF_PREGNANCY_BABY_1,analysis_data$MODE_OF_DELIVERY_BABY_1))

 setwd("/xxx/1819-0119/Linked Data/")
 ssbid_data_1 <- read.csv("SSBID_Mothers.csv.xz")## old file
 setwd("/xxx/1819-0119/Linked Data/")
 ssbid_data_2 <- read.csv("SSBID_Mothers_202212.csv")

#format the first SSIBD file
ssbid_data_1<-mutate(ssbid_data_1, ssbid_still_1=1)
ssbid_data_1<-select(ssbid_data_1,MotherID,STILLBIRTH,  AGE_AT_DEATH, ssbid_still_1)
ssbid_data_1$stillbirth_1<-factor(ssbid_data_1$STILLBIRTH, levels = c(0,1,2),labels = c("0","before_labour","after_labour"))
ssbid_data_1$AGE_AT_DEATH<-factor(ssbid_data_1$AGE_AT_DEATH, levels = c(1,2,3,4,5,6,7,8),labels = c("antepartum","intrapatum","<1h","1h<death<1day","1_6_days", "7_27_days",">weeks","late_foetal_death"))
#format the second SSIBD file
ssbid_data_2<-mutate(ssbid_data_2, ssbid_still_2=2)
ssbid_data_2<-select(ssbid_data_2,MotherID,STILLBIRTH, DATE_OF_DEATH, ssbid_still_2)
ssbid_data_2<-mutate(ssbid_data_2,still_date_of_death= DATE_OF_DEATH)
ssbid_data_2$stillbirth_2<-factor(ssbid_data_2$STILLBIRTH, levels = c(0,1,2),labels = c("0","before_labour","after_labour"))

##merging SSIBD data with the analysis data
ssbid_new_2<-left_join(ssbid_data_1,ssbid_data_2,by = "MotherID" )
ssbid_new_2<-left_join(neonatal_days,ssbid_new_2,by = "MotherID")
ssbid_new_2<-mutate(ssbid_new_2,diff_delivery_death=diff.Date(DATE_OF_DELIVERY-still_date_of_death))
table(ssbid_new_2$still_date_of_death)

setwd("/xxx/1819-0119/Research/adeniyi")





table(ssbid_new$AGE_AT_DEATH)
table(ssbid_new$STILLBIRTH)


##to see births that has smro2 link

smr02_present<-mutate(smr02_data, smr02_present=1)
smr02_present<-select(smr02_present, MotherID,smr02_present)
smr02_present <- distinct(smr02_present, MotherID,.keep_all = TRUE)
ssbid_new<-left_join(ssbid_new, smr02_present)
#save the distinct cohort
ssbid_new <- distinct(ssbid_new, id,.keep_all = TRUE)
fwrite(ssbid_new, file="Breech_data_130922_still_full.csv", sep=",")

ssbid_data$death_date<-date(ssbid_data$q8_5_death_date)
range(!is.na(ssbid_data$q8_5_death_date))
