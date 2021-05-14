library(data.table)
library(magrittr)
library(fuzzyjoin)
library(stringdist)
rm(list=ls())

#set up directory
input_dir_2016 <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/"
input_dir_2017 <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2017/"

#load WHO data
df_who <- fread(paste0(input_dir_2016, "Data_request_20171201_raw.csv"))
df_who <- fread(paste0(input_dir_2017, "Subnational_data_africa.csv"))
df_who2 <-fread(paste0(input_dir_2017, "Subnational_data_world.csv"))
#-------------------------------------------------------Country list cleaning------------------------------------------------------------------------#
#List of countries in WHO
who_countries <- unique(df_who, by = "Country Name")
who_afr_countries <- unique(df_who, by = "iso") %>% subset(., select = c(2,3))
who_world_countries <- unique(df_who2, by = "iso") %>% subset(., select = c(2,3))
who_countries <- rbind(who_afr_countries,who_world_countries)
who_countries <- subset(who_countries, select = 2)
setnames(mbg_countries,"ihme_lc_id", "iso")
mbg_afr_countries <- fread(paste0(input_dir_2017, "grant_afr_countries.csv"), header =T) %>% subset(., select = c(1,3))  %>% setnames(., "iso3", "iso")
# compare 2017 mbg countries and jrf countries
countries_combined <- merge(mbg_countries, who_countries, all.x = T, all.y = T)
afr_combined <- merge(mbg_afr_countries, who_afr_countries, all.x = T, all.y = T )
write.csv(countries_combined, paste0(input_dir_2017, "countries_comparison.csv"))
write.csv(afr_combined, paste0(input_dir_2017, "africa_comparison.csv"))

who_countries <- who_countries[,c("Iso Code", "Country Name", "WHO Region")]
who_afr_countries <- who_countries[`WHO Region` == "AFRO",c(1,2)]
#List of countries in MBG
mbg_countries <- fread(paste0(input_dir_2017, "vcm_grant_country_list_for_mapping.csv"))
mbg_countries <- unique(mbg_countries[, c(1,8)])

setnames(who_countries, c("Country Name", "Iso Code"), c("Country_who",  "iso3"))
setnames(who_afr_countries, c("Country Name", "Iso Code"), c("Country_who",  "iso3"))
#check which countries is missing in WHO
setnames(mbg_countries, c("Country","ihme_lc_id"), c("Country_mbg", "iso3"))
countries_combined <- merge(who_countries, mbg_countries, by = "iso3", all.x = T, all.y = T)
#whoMissingCountries <- countries_combined[is.na(Country_who), c(1,3)]
mbg_afr <- merge(who_afr_countries, mbg_countries, by = "iso3", all.x =T, all.y= T)

#write.csv(countries_combined, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/WHO_missing_countries.csv")
write.csv(mbg_afr, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/WHO_missing_afr_countries.csv")

#removed countries in WHO but not in MBG using Excel (african countries only)
mbg_countries <- fread(paste0(input_dir, "grant_afr_countries.csv"), header =T)
mbg_afr_countries <- mbg_countries[,"Country_mbg"][[1]]
#-----------------------------------------------------------------------------------------------------------------------------------------------------#

afr2017 <- fread(paste0(input_dir_2017,"unitmatch2017_AFR.csv"))
afr2017 <- afr2017[,"countries"][[1]]

unitmatch2017 <- fread(paste0(input_dir_2017, "unitmatch2017.csv"))
unitmatch2017[countries %in% afr2017, continent:= "Africa"]
write.csv(unitmatch2017, paste0(input_dir2017,"unitmatch2017.csv"))

# read the WHO admin 2 shapefile
WHO_adm1 <- fread(paste0(input_dir, "who_adm1.csv"))
WHO_adm2 <- fread(paste0(input_dir, "who_adm2.csv"))
WHO_adm3 <- fread(paste0(input_dir, "who_adm3.csv"))
WHO_adm4 <- fread(paste0(input_dir, "who_adm4.csv"))
iso3admin1 <- WHO_adm1$ISO_3_CODE
iso3admin2 <- WHO_adm2$ISO_3_CODE
iso3admin3 <- WHO_adm3$ISO_3_CODE
iso3admin4 <- WHO_adm4$ISO_3_CODE
unit_count1 <- as.data.frame(table(iso3admin1))
unit_count2 <- table(iso3admin2)
unit_count3 <- table(iso3admin3)
unit_count4 <- table(iso3admin4)


write.csv(unit_count1, paste0(input_dir2017,"unitcount_whoadm1.csv"))
