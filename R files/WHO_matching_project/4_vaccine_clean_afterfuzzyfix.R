library(data.table)
library(magrittr)
library(fuzzyjoin)
library(stringdist)

rm(list=ls())
input_dir <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/"

#load gadm and gaul matching shapefile
df_gaul <- fread(paste0(input_dir, "matching_adm2_gaulupdate_Africa.csv"))
df_gadm <- fread(paste0(input_dir, "matching_adm2_gadmupdate_Africa.csv"))

#remove duplicate in fuzzy need fix
df_gaul <- df_gaul[Note != "r", ]
df_gadm <- df_gadm[Note != "r", ]
# for wrong fuzzy match, change merge status to need change
df_gadm <- df_gadm[Note == "change", merge_status := "need change"]
df_gaul <- df_gaul[Note == "change", merge_status := "need change"]

# for manual add location, change merge status to fuzzy success
df_gadm <- df_gadm[Note %in% c("manual add","match"), merge_status := "manually match"]
df_gaul <- df_gaul[Note %in% c("manual add", "match"), merge_status := "manually match"]


#recalculate the not-matching percentage
gaul_afr_countries <- unique(df_gaul$ADM0_NAME.x)
gadm_afr_countries <- unique(df_gadm$ADM0_NAME.x)


percent_match_gaul <- c()
for (i in 1:length(gaul_afr_countries)){
percent_match_gaul[i] <- nrow(df_gaul[!(merge_status %in% c("in WHO but not shpfile", "need change")) & ADM0_NAME.x == gaul_afr_countries[i]])*100/nrow(df_gaul[ADM0_NAME.x == gaul_afr_countries[i]])
}
percent_match_gaul <- as.data.table(cbind(gaul_afr_countries, percent_match_gaul))

percent_match_gadm <- c()
for (i in 1:length(gadm_afr_countries)){
  percent_match_gadm[i] <- nrow(df_gadm[!(merge_status %in% c("in WHO but not shpfile", "need change")) & ADM0_NAME.x == gadm_afr_countries[i]])*100/nrow(df_gadm[ADM0_NAME.x == gadm_afr_countries[i]])
}

percent_match_gadm <- as.data.table(cbind(gadm_afr_countries, percent_match_gadm))

#remove duplicate
df_gaul <- unique(df_gaul, by= c("ADM2_NAME.who", "vaccine", "who_coverage", "ADM2_NAME.shpfile" ))
df_gadm <- unique(df_gadm, by= c("ADM2_NAME.who", "vaccine", "who_coverage", "ADM2_NAME.shpfile" ))

#write to csv
write.csv(df_gaul[,-1], "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/remaining_adm2gaul_matching2_Africa.csv")
write.csv(df_gadm[,-1], "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/remaining_adm2gadm_matching2_Africa.csv")





