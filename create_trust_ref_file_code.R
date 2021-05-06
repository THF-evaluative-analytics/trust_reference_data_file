#CREATE A TRUST REFERENCE FILE USING PUBLICLY AVAILABLE DATA

library(janitor)
library(tidyverse)
library(data.table)

path= "/Users/gclarke/My Work/Projects/Consult/Mental Health Decision Units/data/trust reference"
path = "/Users/parispariza/Documents/NHSEI/Projects/DECISION/trust reference"
##############################################
##1. READ IN DATA FROM VARIETY OF PUBLIC SOURCES
##############################################

#############
##PEER data - Includes ACUTE and MH TRUSTS
#############

#Ref: https://www.england.nhs.uk/insights-platform/model-hospital/
#https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/innovative-uses-of-data/multi-dataset-analysis/nhs-trust-peer-finder-tool
#Data tab from Appendix A from NHS Peer Finder Tool has been saved as a csv file"
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

##16/4/21 - Have noticed that some trusts are AMBULANCE trusts. Have removed these. 
##Some of the trusts are in Wales. Can you work out how to remove these - perhaps using: ?
##I think some https://gov.wales/nhs-wales-health-boards-and-trusts
## "RYT", "RQF", "RT4"

##nhs Code Organisation from:
##https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiLrvvQlervAhU-gP0HHUf2DfcQFjADegQIBhAD&url=https%3A%2F%2Fwww.england.nhs.uk%2Fwp-content%2Fuploads%2F2014%2F11%2Fnhs-non-nhs-ods-codes.xlsx&usg=AOvVaw1hduuzFTZeTUFD1QYmk7u2
ods_2014 = read.csv(paste0(path, "/nhs-non-nhs-ods-codes.csv")) %>%
   clean_names() %>%
  dplyr::select(procode, name, postcode, open_date, close_date) %>%
  mutate(year=2014) %>%
  filter(procode!="04X") %>%
  filter(!grepl("AMBULANCE", name)) %>%
  as.data.frame()
#NHS TRUST NAMES (etr) downloaded from: 
##2021:https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
##2016:https://data.england.nhs.uk/dataset/ods-nhs-trusts-and-sites
##Some trusts have merged between 2016 and 2021 e.g. RLN (City Hospitals Sunderland) and RE9 (South Tyneside NHS FT) have merged into 
##R0B (South Tyneside and Sunderland NHS Foundation Trust). We use etr_2016 to determine trust names for 2011-2018 catchment data:
##Here we find all ODS codes and record year:
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
##Could not find list of Mental Health Trusts from NHS Sources - instead have downloaded from wikipedia
##https://en.wikipedia.org/wiki/Mental_health_trust#List_of_MHTs 
##and created a csv file"
mh_trusts = read.csv(paste0(path, "/mental_health_trusts.csv")) %>%
   clean_names() %>%
  as.data.frame()

##Check that all MH trusts can be found in collected ods data: 
i1_MH = match(mh_trusts$name_caps, ods_data$name)
table(is.na(i1_MH))
# FALSE 
# 53 

##############################################
##2.CREATE SKELETON TRUST REFERENCE FILE USING ALL AVAILABLE DATA
##############################################

##(i) Create a base file with all relevant rows and years:
#*#all_procodes = unique(c(peers$procode, ods_data$procode))
all_procodes = unique(sort(c(peers$procode, ods_data$procode)))
trust_ref_00 = expand.grid(year=c(2011:2021), procode=all_procodes)

##Add data to base file:
trust_ref_01 = trust_ref_00 %>%
  left_join(peers, by=c("procode" = "procode", "year"= "year_peers")) %>%
  left_join(ods_data, by=c("procode" ="procode", "year"="year")) %>%
  as.data.frame()

##GMC 21/4/21 Have added >= to condition (rather than >) to pick up a trust like WEST LONDON NHS TRUST
##which is recorded as a mh trust but earlier had a different name, so only one value of MH_trust_flag
##of possible 11 is captured in first mutate. Should have 53 trusts each calendar year. 

trust_ref_02 = trust_ref_01 %>%
  mutate(MH_trust_flag = ifelse(name %in% mh_trusts$name_caps, 1, 0)) %>%
  group_by(procode) %>%
  mutate(MH_trust_flag = ifelse(sum(MH_trust_flag)>=1, 1, 0)) %>%
  as.data.frame()

trust_ref_03 <- trust_ref_02 %>%
  # select(year, procode, patients, attendances, age_0_15, age_60_74, deprivation, urban, fte_staff, diagnosis,
  #        night_beds, day_beds, sites, non_type_1_ae_department, MH_trust_flag) %>% 
  as.data.frame()

##############################################
##3.ADD IN CATCHMENT SIZES
##############################################

##(i). Use catchment data to add in catchment sizes for acute trusts
#- Total catchment size each year (2011-2018 and fill in 2018 data after that)
#- % of individuals in selected age groups each year e.g. [0-4), [5-14), [15-24), [25-44), [45-64), [65-74), 75+?

##Selecting data needed
catchment_01 <- catchment %>%
  filter(admission_type=="All Admissions") %>% 
  #select(year=catchment_year, procode=trust_code, sex, age, patients_admitted, catchment) %>%
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
  #summarise_at(c("age_00_04", "age_05_14", "age_15_24", "age_25_64", "age_65_74", "age_75_plus"), ~round(./catchment,2)) %>% 
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
  #summarise_at(c("sex_m"), ~round(./catchment,2)) %>% 
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

#colnames(catchment_final) <- c("procode", "year", "catchment" ,paste(colnames(catchment_final[,4:11]), "cat", sep = "_"))
colnames(catchment_final) <- c("procode", "year",paste(colnames(catchment_final[,3:11]), "cat", sep = "_"))

##Joining catchment file to full trust reference file
trust_ref_04 <- trust_ref_03 %>% 
  full_join(catchment_final) %>% 
  as.data.frame() 

##(ii). Add in catchment sizes for mental health trusts
#Perhaps we should use no. of attendances data from peer data? Match on procode and add in (this is data for 2018).

##We can see by looking at acute trust data that in the acute trusts, population size is roughly 4 x admission size.  
#Ratio of admissions data to catchment size in the acute trust data:
c1 = catchment %>%
  group_by(trust_code, catchment_year) %>%
  summarise_at(.vars=c("catchment", "patients_admitted"), .funs=list(sum)) %>%
  left_join(etr_2016, by=c("trust_code" ="procode")) %>%
  as.data.frame()
lm1 = lm(catchment ~ patients_admitted, data=c1)
summary(lm1)$coef
# Estimate   Std. Error   t value     Pr(>|t|)
# (Intercept)       -36911.146786 9.745697e+03  -3.78743 0.0001598754
# patients_admitted      3.918727 2.924792e-02 133.98310 0.0000000000

##############################################
##4.FILL DATA UP AND DOWN AS APPROPRIATE
##############################################

##Filling in data from both peers (for 2011-2017 & 2019-2021??) and catchment (for 2019-2021)
###Fill up 
# trust_ref_05a <- trust_ref_04 %>% 
#   group_by(procode, MH_trust_flag) %>% 
#   arrange(procode, year) %>% 
#   fill(names(trust_ref_04)[-c(1,2,14)], .direction="up") %>% 
#   as.data.frame()

trust_ref_05a <- trust_ref_04 %>% 
  group_by(procode) %>% 
  arrange(procode, year) %>% 
  fill(names(trust_ref_04)[-c(1,2)], .direction="up") %>% 
  as.data.frame()

###Fill down
# trust_ref_05b <- trust_ref_05a %>% 
#   group_by(procode, MH_trust_flag) %>% 
#   arrange(procode, year) %>% 
#   fill(names(trust_ref_04)[-c(1,2,14)], .direction="down") %>% 
#   as.data.frame()

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

write_csv(trust_ref_final, paste0(path, "/trust_ref_file_V4.csv"))

  
##GMC 16/4/12
##FILE CHECKS

##Which trusts do not appear in peer data? Some of these are MH trusts 
##There are 40. None of these have catchment data either
a=trust_ref_final
tmp = a[which(a$year==2016),]
i1.peer.error = which(is.na(tmp$deprivation))  
length(i1.peer.error)
# [1] 40
tmp[i1.peer.error, c(1, 2, 24, 25, 26, 28, 29)]

##Which trusts do not appear in catchment data? These will include all 53 MH trusts. 
##Looks like they also include children's hospitals e.g. Alder Hey .
##We will not want to compare to children's hospitals so can remove these also (as well as Wales)
##What about other specialist?

i1.cat.error = which(is.na(tmp$catchment_cat) & !tmp$MH_trust_flag==1)  
length(i1.cat.error)
# [1] 55
tmp[i1.cat.error, c(1, 2, 24, 25, 26, 28, 29)]


  