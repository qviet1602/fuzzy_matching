
#set up directory
input_dir <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2016/"
input_dir2017 <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2017/"
df_who_afr <- fread(paste0(input_dir2017, "Subnational_data_africa.csv"), header = T)
df_who_world <- fread(paste0(input_dir2017, "Subnational_data_world.csv"), header = T)
df_who <- rbind(df_who_afr, df_who_world) %>% setnames(., "Country.who", "ADM0_NAME")
df_who <- df_who[Vaccode == "DTP3"]
#-----------------------------------------Function for unit matching---------------------------------------------------------------#
# create a function to calculate the percent unit matching with adm2 shapefiles
unit_matching_adm <- function(countries, shapefile){
	unit_match <- c()
	count_difference <- c() 
  # start the for loop matching for all african countries using gadm shapefile
  for (i in 1 : length(countries)){
  	unit_match[i]<- nrow(df_who[ADM0_NAME == countries[i]])*100/ nrow(shapefile[ADM0_NAME == countries[i]])
  	count_difference[i] <- nrow(df_who[ADM0_NAME == countries[i]]) - nrow(shapefile[ADM0_NAME == countries[i]])
  }
  unit_match <- data.table(cbind(countries, unit_match))
  return(unit_match)
}
mbg_afr_countries <- mbg_afr_countries[[2]]

gaul_unit_match_adm2 <- unit_matching_adm(mbg_afr_countries, df_gl) 
gadm_unit_match_adm2 <- unit_matching_adm(mbg_afr_countries,df_gadm36)
gaul_unit_match_adm1 <- unit_matching_adm(mbg_afr_countries, df_gl1)
gadm_unit_match_adm1 <- unit_matching_adm(mbg_afr_countries, df_gadm36_1)
badMatch_countries <- fread(paste0(input_dir,"unitmatch_africa.csv"))
badMatch_countries <- badMatch_countries[is.na(`Name match`),c("countries")][[1]]
badMatch_countries <- badMatch_countries[1:20]
gadm36_unit_match_adm2 <- unit_matching_adm(badMatch_countries, df_gadm36)
gadm36_unit_match_adm1[unit_match == 100]

unit_match <- cbind(gaul_unit_match_adm2, gadm_unit_match_adm2[,2], gaul_unit_match_adm1[,2], gadm_unit_match_adm1[,2])
setnames(unit_match, c("countries", "gaul_adm2","gadm_adm2","gaul_adm1","gadm_adm1" ))
#write csv to fixing outliers in Excel
write.csv(unit_match, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2017/unitmatch2017_AFR.csv")

rm(gaul_unit_match_adm2, gadm_unit_match_adm2, gaul_unit_match_adm1, gadm_unit_match_adm1)
#------------------------------------------Separate into different adm unit----------------------------------------------#
unit_match_update <- fread(paste0(input_dir,"unitmatch_africa.csv"))
adm1_countries <- unit_match_update[`adm level`== "adm1"][[1]]
adm1_gaul_countries <- unit_match_update[`adm level` == "adm1" & Shapefile == "gaul"][[2]]
adm1_gadm_countries <- unit_match_update[`adm level` == "adm1" & Shapefile == "gadm"][[2]]
adm2_countries <- unit_match_update[`adm level` == "adm2"][[2]]
adm2_gaul_countries <- unit_match_update[`adm level` == "adm2" & Shapefile == "gaul"][[2]]
adm2_gadm_countries <- unit_match_update[`adm level` == "adm2" & Shapefile == "gadm"][[2]]

#--------------------------------------JRF2017 unit matching-----------------------------------------------------------#

# countries in MBG but no data in JRF2017

mbg2017 <- fread(paste0(input_dir2017, "allCountries_whovsmbg.csv"), header = T)
mbg2017_nodat <- mbg2017[is.na(Country_who2017), "Country_mbg"]


# remaining countries (in MBG and JRF) need to unit and name match
mbg2017 <-   as.vector(mbg2017[!is.na(Country_who2017), "Country_mbg"])
mbg2017 <- mbg2017[[1]]

# run unit matching with mbg2017 countries using adm2 shapefile
gaul_unit_match_adm2 <- unit_matching_adm(mbg2017, df_gl) 
gadm_unit_match_adm2 <- unit_matching_adm(mbg2017,df_gadm36)

# run unit matach with mbg2017 countries using adm1 shapefile
gaul_unit_match_adm1 <- unit_matching_adm(mbg2017, df_gl1)
gadm_unit_match_adm1 <- unit_matching_adm(mbg2017, df_gadm36_1)

# combine all of the matching above together
unit_match2017 <- cbind(gaul_unit_match_adm2, gadm_unit_match_adm2[,2], gaul_unit_match_adm1[,2], gadm_unit_match_adm1[,2])
setnames(unit_match2017, c("countries", "gaul_adm2","gadm_adm2","gaul_adm1","gadm_adm1" ))

# combine the nodata countries in
unit_match2017 <- rbind(unit_match2017,mbg2017_nodat, fill = T)

# output as csv 
write.csv(unit_match2017, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_Subnational_Immunization_Coverage_data/2017/unitmatch2017.csv")

