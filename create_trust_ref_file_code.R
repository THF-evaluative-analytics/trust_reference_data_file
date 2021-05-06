# CREATE A TRUST REFERENCE FILE USING PUBLICLY AVAILABLE DATA

# Load required libraries
library(janitor)
library(tidyverse)
library(data.table)

#Define your path (full path of the folder where all the different publicly available datasets are saved)
path= ""

##############################################
##1. READ IN DATA FROM VARIETY OF PUBLIC SOURCES
##############################################

#############
##PEER data - Includes ACUTE and MH TRUSTS
#############

peers = read.csv(paste0(path, "/appendix_a_data.csv")) %>%
  clean_names() %>%
  mutate(year=2018) %>%
  as.data.frame()
names(peers)[-1] = paste0(names(peers)[-1], "_peers")

#############
##CATCHMENT data - Includes ACUTE trusts only
#############

catchment = read.csv(paste0(path, "/2020 Trust Catchment Populations Worksheet.csv")) %>%
  clean_names() %>%
  as.data.frame()

#############
##NHS ODS CODES 
#############

ods_2014 = read.csv(paste0(path, "/nhs-non-nhs-ods-codes.csv")) %>%
   clean_names() %>%
  dplyr::select(procode, name, postcode, open_date, close_date) %>%
  mutate(year=2014) %>%
  filter(procode!="04X") %>%
  filter(!grepl("AMBULANCE", name)) %>%
  as.data.frame()

etr_2021 = read.csv(paste0(path, "/etr_2021.csv")) %>%
  clean_names() %>%
  dplyr::select(organisation_code, name, postcode, open_date, close_date) %>%
  rename(procode=organisation_code) %>%
  mutate(year=2021) %>%
  filter(!grepl("AMBULANCE", name)) %>%
  as.data.frame()

etr_2016 = read.csv(paste0(path, "/etr_2016.csv")) %>%
  clean_names() %>%
  dplyr::select(organisation_code, name, postcode, open_date, close_date) %>%
  rename(procode=organisation_code) %>%
  mutate(year=2016) %>%
  filter(!grepl("AMBULANCE", name)) %>%
  as.data.frame()

ods_data = bind_rows(ods_2014, etr_2021, etr_2016)

#############
##Names of Mental Health Trusts
#############

mh_trusts = read.csv(paste0(path, "/mental_health_trusts.csv")) %>%
   clean_names() %>%
  as.data.frame()

##Check that all MH trusts can be found in collected ods data: 
i1_MH = match(mh_trusts$name_caps, ods_data$name)
table(is.na(i1_MH))


##############################################
##2.CREATE SKELETON TRUST REFERENCE FILE USING ALL AVAILABLE DATA
##############################################

##(i) Create a base file with all relevant rows and years:
all_procodes = unique(sort(c(peers$procode, ods_data$procode)))
trust_ref_00 = expand.grid(year=c(2011:2021), procode=all_procodes)

##Add data to base file:
trust_ref_01 = trust_ref_00 %>%
  left_join(peers, by=c("procode" = "procode", "year"= "year_peers")) %>%
  left_join(ods_data, by=c("procode" ="procode", "year"="year")) %>%
  as.data.frame()

trust_ref_02 = trust_ref_01 %>%
  mutate(MH_trust_flag = ifelse(name %in% mh_trusts$name_caps, 1, 0)) %>%
  group_by(procode) %>%
  mutate(MH_trust_flag = ifelse(sum(MH_trust_flag)>=1, 1, 0)) %>%
  as.data.frame()

trust_ref_03 <- trust_ref_02 %>% 
  as.data.frame()


##############################################
##3.ADD IN CATCHMENT SIZES
##############################################

##(i). Use catchment data to add in catchment sizes for acute trusts

##Selecting data needed
catchment_01 <- catchment %>%
  filter(admission_type=="All Admissions") %>% 
  select(year=catchment_year, procode=trust_code, trust_type, sex, age, patients_admitted, catchment) %>%
  as.data.frame()

##Calculating percentages for age groupings (redefined)
catchment_01a <- catchment_01 %>%
  group_by(procode, year, age) %>%
  summarise_at(.vars=c("patients_admitted", "catchment"), .funs=list(sum)) %>%
  as.data.frame()

catchment_01a$age <- gsub("-", "_", catchment_01a$age)
catchment_01a$age <- ifelse(catchment_01a$age=="90+", "90_plus", catchment_01a$age)

catchment_02a <- catchment_01a %>%
  select(-patients_admitted) %>% 
  spread(age, catchment, sep="_") %>% 
  mutate(age_05_14 = age_05_09 + age_10_14, 
         age_15_24 = age_15_19 + age_20_24,
         age_25_64 = age_25_29 + age_30_34 + age_35_39 + age_40_44 + age_45_49 + age_50_54 + age_55_59 + age_60_64,
         age_65_74 = age_65_69 + age_70_74, 
         age_75_plus = age_75_79 + age_80_84 + age_85_89 + age_90_plus) %>% 
  select(procode, year, age_00_04, age_05_14, age_15_24, age_25_64, age_65_74, age_75_plus) %>% 
  mutate( catchment = rowSums(select(., starts_with("age_"))) ) %>% 
  group_by(procode, year, catchment) %>% 
  summarise_at(c("age_00_04", "age_05_14", "age_15_24", "age_25_64", "age_65_74", "age_75_plus"), ~(./catchment)) %>% 
  as.data.frame()

##Calculating percentages for sex groupings (male only)
catchment_01b <- catchment_01 %>%
  group_by(procode, year, sex) %>%
  summarise_at(.vars=c("patients_admitted", "catchment"), .funs=list(sum)) %>%
  as.data.frame()

catchment_01b$sex <- ifelse(catchment_01b$sex=="1", "m", "f")

catchment_02b <- catchment_01b %>%
  select(-patients_admitted) %>% 
  spread(sex, catchment, sep="_") %>% 
  select(procode, year, sex_m, sex_f) %>% 
  mutate( catchment = rowSums(select(., starts_with("sex_"))) ) %>% 
  group_by(procode, year, catchment) %>% 
  summarise_at(c("sex_m"), ~(./catchment)) %>% 
  as.data.frame()

##Retrieving patient numbers to add to final file
patients_adm <- catchment_01 %>%
  group_by(procode, year) %>%
  summarise_at(.vars=c("patients_admitted"), .funs=list(sum)) %>%
  as.data.frame()

##Creating final catchment file
catchment_final <- catchment_02a %>% 
  left_join(catchment_02b) %>% 
  left_join(patients_adm) %>% 
  as.data.frame()

colnames(catchment_final) <- c("procode", "year",paste(colnames(catchment_final[,3:11]), "cat", sep = "_"))

##Joining catchment file to full trust reference file
trust_ref_04 <- trust_ref_03 %>% 
  full_join(catchment_final) %>% 
  as.data.frame() 

##(ii). Add in catchment sizes for mental health trusts (optional)
  
#Ratio of admissions data to catchment size in the acute trust data:
c1 = catchment %>%
  group_by(trust_code, catchment_year) %>%
  summarise_at(.vars=c("catchment", "patients_admitted"), .funs=list(sum)) %>%
  left_join(etr_2016, by=c("trust_code" ="procode")) %>%
  as.data.frame()
lm1 = lm(catchment ~ patients_admitted, data=c1)
summary(lm1)$coef

##We can see by looking at acute trust data that in the acute trusts, population size is roughly 4 x admission size.


##############################################
##4.FILL DATA UP AND DOWN AS APPROPRIATE
##############################################

##Filling in data from both peers (for 2011-2017 & 2019-2021) and catchment (for 2019-2021)

###Fill up 
trust_ref_05a <- trust_ref_04 %>% 
  group_by(procode) %>% 
  arrange(procode, year) %>% 
  fill(names(trust_ref_04)[-c(1,2)], .direction="up") %>% 
  as.data.frame()

###Fill down
trust_ref_05b <- trust_ref_05a %>% 
  group_by(procode) %>% 
  arrange(procode, year) %>% 
  fill(names(trust_ref_04)[-c(1,2)], .direction="down") %>% 
  as.data.frame()


##############################################
##5. Save final trust reference file
##############################################

trust_ref_final <- trust_ref_05b %>% 
  #filter(!procode=="04X") %>% 
  as.data.frame() 

write_csv(trust_ref_final, paste0(path, "/trust_ref_file.csv"))


  
