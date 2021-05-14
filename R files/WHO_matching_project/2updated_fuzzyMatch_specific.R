library(data.table)
library(magrittr)
library(fuzzyjoin)
library(stringdist)
library(dplyr)
rm(list=ls())
### Functions
fix_diacritics <- function(x) {
  
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')    
  
  return(chartr(replace_me, replace_with, x))
  
}
get_gaul_codes <- function(gauls) {
  # Inputs: gauls = vector of continents, regions, stages, or countries
  #                 **now also accepting combinations**
  # Outputs: vector of gaul codes
  gaul_list <- NULL
  # Set up list of gaul_codes
  gaul_ref <- list()
  gaul_ref[['all']] <- c(163,2059,115,188,170,160,152,205,226,270,133,150,74,220,257, # all gauls in the master shape
                         253,271,79,77,70,58,43,89,76,50,214,59,68,45,49,8,169,145,6,
                         248,40765,4,172,235,142,227,35,243,221,217,182,181,159,155,
                         144,105,90,106,94,47,66,42,29,71,72,11,63,99,123,246,20,209,
                         211,24,108,200,258,191,75,180,162,111,103,61,28,259,98,30,57,
                         107,73,194,233,260,263,195,51,37,33,12,250,261,239,138,132,167,
                         147296,202,67,126,147295,171,44,264,139,153,240,40,196,116,222,
                         242,154,175,117,231,31,23,1,187,137,267,141,201,215,249,269,118,
                         130,238,121,92,64,21,19,13,255,165,113,198,203,254,26,65,204,223,
                         41,186,236,147,140,119,114,256,84,78,69,241,2647,62,156,224,97,
                         122,199,2648,229,34,7,3,177,148,85,93,237,27,18,179,17,192,262,
                         225,83,101,185,157,135,212,245,5,46,77310,2707,2708,77311,77313,
                         77314,77315,77312,2714,2028,2029,2030,2031,2032,2033,2034,2035,
                         671,2036,2037,2038,2039,2040,2041,2042,2043,2044,2045,2046,2047,
                         2048,2049,2050,2051,2052,2053,2054,2055,2056,2057,2058,3214,3215,
                         3216,3217,3218,3219,3220,3221,3222,3223,3224,3225,3226,3227,3228,
                         3229,3230,3231,3232,3233,3234,3235,3236,3237,3238,3239,3240,3241,
                         3242,3243,3244,3245,3246,3247,3248,3249,3250,3251,3252,3253,3254,
                         3255,3256,3257,3258,3259,3260,3261,3262,3263,3264,3182,665,668,
                         666,667,669,670,672,673,674,675,676,677,678,679,680,681,682,683,
                         684,685,686,687,688,689,690,691,1672,1652,1653,1654,1655,1656,1657,
                         1658,1659,1660,1661,1677,1663,1664,1665,1666,1667,1668,1669,1670,1682,
                         1685,1673,1674,1675,1676,1694,1678,1679,1680,1681,1662,1683,1684,
                         1671,1687,1688,1689,1690,1686,1692,1693,1691,1695,1696,1697,898,899,
                         900,901,902,903,904,905,906,907,908,909,911,912,913,914,915,916,917,
                         918,919,920,921,922,923,924,926,927,928,929,930,2622,2623,2624,2625,
                         2626,2627,2628,2629,2630,2631,2632,2633,2634,2786,2800,3183,3184,3186,149,33364)
  gaul_ref[["eastern_europe"]] <- c(165,254,26,147,140,78)
  gaul_ref[["middle_east"]] <- c(117,1,187,137,267,141,201,215,249,118,130,238,21,255)
  gaul_ref[["latin_america"]] <- c(71,72,11,63,99,123,246,20,209,211,24,108,200,258,191,75,
                                   180,162,111,103,61,28,30,57,107,73,194,233,263,195,37,33)
  gaul_ref[["se_asia"]] <- c(147296,67,147295,171,44,264,139,153,240,196,116,242)
  gaul_ref[["south_asia"]] <- c(23,31,115,175,188)
  gaul_ref[["africa"]] <- c(4,6,8,29,35,42,43,45,47,49,50,58,59,66,68,70,
                            74,76,77,79,89,90,94,95,105,106,142,144,145,150,
                            152,155,159,169,170,172,181,182,205,214,217,221,
                            226,235,243,248,253,268,270,271,40762,40765,
                            227,257,133,269)
  gaul_ref[["central_america"]] <- c(11,20,28,30,24,61,63,71,72,99,103,111,108,
                                     123,209,162,180,191,200,75,246,211,258)
  gaul_ref[["south_america"]] <- c(33,37,57,73,107,195,194,233,263)
  gaul_ref[["stage1"]] <- c(170,152,205,226,270,133,150,74,257,253,271,79,77,70,
                            58,43,89,76,50,214,59,68,45,49,8,169,6,40765,4,172,235,
                            142,227,35,243,221,217,182,181,159,155,144,105,90,106,94,
                            47,66,42,29,72,108,180,111,103,195,33,239,132,167,67,171,
                            44,264,139,153,240,196,116,242,175,188,115,117,231,31,23,
                            1,267,269,91,118,130,238,3,192,262,225,157,135)
  gaul_ref[["stage2"]] <- c(145,75,162,28,57,107,73,261,138,147295,154,187,215,249,19,
                            255,254,34,170,152,205,226,270,133,150,74,257,253,271,79,77,
                            70,58,43,89,76,50,214,59,68,45,49,8,169,6,40765,4,172,235,
                            142,227,35,243,221,217,182,181,159,155,144,105,90,106,94,
                            47,66,42,29,72,108,180,111,103,195,33,239,132,167,67,171,
                            44,264,139,153,240,196,116,242,175,188,115,117,231,31,23,
                            1,267,269,91,118,130,238,3,192,262,225,157,135)
  gaul_ref[["cssa"]]   <- c(8,49,59,68,76,89)
  gaul_ref[["cssa_diarrhea"]]   <- c(49,59,68,76,89) # Removed Angola
  gaul_ref[["sssa_diarrhea"]]   <- c(8,35,142,172,227,271) # Added Angola, removed Swaziland
  gaul_ref[["essa_diarrhea"]]   <- c(43,58,70,77,79,133,150,152,170,205,226,74,235,257,253,270) # Added Swaziland
  gaul_ref[["cssa_diarrhea2"]]   <- c(49,45,50,59,89,76)
  gaul_ref[["essa_diarrhea2"]]   <- c(43,58,70,77,79,133,150,152,170,205,226,74,235,257,253,270,6) # Added Swaziland
  gaul_ref[["name_diarrhea2"]]   <- c(4,40762,145,169,248) # no yemen for now,269)
  gaul_ref[["sssa_diarrhea2"]]   <- c(8,35,142,172,227,271) # Added Angola, removed Swaziland
  gaul_ref[["wssa_diarrhea2"]]   <- c(29,42,47,66,90,94,106,105,144,155,159,181,182,214,217,221,243)
  gaul_ref[["essa_edu"]]   <- c(226,79,74,6,77,70)
  gaul_ref[["cssa_edu"]]   <- c(182,43,58,133,150,152,170,205,235,257,253,270,49,45,59,89,76,68,8) # Added Swaziland
  gaul_ref[["name_edu"]]   <- c(4,40762,145,169,248) # no yemen for now,269)
  gaul_ref[["sssa_edu"]]   <- c(35,142,172,227,271) # Added Angola, removed Swaziland
  gaul_ref[["wssa_edu"]]   <- c(29,42,47,66,90,94,106,105,144,155,159,181,214,217,221,243,50)
  gaul_ref[["essa_edu2"]]   <- c(43,58,70,77,79,133,150,152,170,205,226,74,257,253,270)
  gaul_ref[["name_edu2"]]   <- c(4,40762,145,169,6,248) # no yemen for now,269)
  gaul_ref[["sssa_edu2"]]   <- c(35,142,172,227,235,271)
  gaul_ref[["wssa_edu2"]]   <- c(29,42,45,47,50,66,90,94,106,105,144,155,159,181,182,214,217,221,243)
  gaul_ref[["cssa_edu2"]]   <- c(8,49,59,68,76,89)
  gaul_ref[["essa"]]   <- c(43,58,70,77,79,133,150,152,170,205,226,74,257,253,270)
  gaul_ref[["name"]]   <- c(4,40762,40765,145,169,6,248) # no yemen for now,269)
  gaul_ref[["namelite"]] <- c(169,6,40765) # remove tunisia, algeria and libya from mesh
  gaul_ref[["sssa"]]   <- c(35,142,172,227,235,271)
  gaul_ref[["wssa"]]   <- c(29,42,45,47,50,66,90,94,106,105,144,155,159,181,182,214,217,221,243)
  gaul_ref[["cessa"]]  <- c(8,49,59,68,76,89,43,58,70,77,79,133,150,152,170,205,226,74,257,253,270)
  gaul_ref[["cwssa"]]  <- c(8,49,59,68,76,89,29,42,45,47,50,66,90,94,106,105,144,155,159,181,182,214,217,221,243)
  gaul_ref[["cessa2"]]  <- c(49,59,68,76,89,43,58,70,77,79,133,150,205,226,74,257,253,5) # remove AGO, ZMB, MWI, MOZ, add cameroon
  gaul_ref[["sssa2"]]    <- c(35,142,172,227,235,271,8,270,152,170) # add AGO, ZMB, MWI, MOZ
  gaul_ref[["cssa_cam"]]   <- c(8,49,59,68,76,89,45) # added cameroon
  gaul_ref[["wssa_nocam"]]   <- c(29,42,47,50,66,90,94,106,105,144,155,159,181,182,214,217,221,243) # removed cameroon
  gaul_ref[["name_hi"]] <- c(4,40765,145,169,248,40762) # No Sudan
  gaul_ref[["essa_hi"]] <- c(133,270) # Zambia and Kenya
  gaul_ref[["essa_lo"]] <- c(43,58,70,77,79,150,152,170,205,226,74,257,253,6) # Contains Sudan
  gaul_ref[["cssa_hi"]] <- c(59,76,89) # GNQ, COG, and Gabon
  gaul_ref[["cssa_lo"]] <- c(8,49,68)
  gaul_ref[["wssa_hi"]] <- c(94) #Ghana
  gaul_ref[["wssa_lo"]] <- c(29,42,45,47,50,66,90,106,105,144,155,181,182,214,217,221,243) # no Ghana
  gaul_ref[["sssa_hi"]] <- c(35,172,227) # Botswana, namibia, south africa
  gaul_ref[["sssa_lo"]] <- c(142,235,271) # LSO, Zimbabwe, Swaziland
  gaul_ref[["mwi"]] <- 152
  gaul_ref[["nga"]] <- 182
  gaul_ref[["egy"]] <- 40765
  gaul_ref[["gha"]] <- 94
  gaul_ref[["cod"]] <- 68
  gaul_ref[["zaf"]] <- 227
  gaul_ref[["ago"]] <- 8
  gaul_ref[["caf"]] <- 49
  gaul_ref[["cog"]] <- 59
  gaul_ref[["cod"]] <- 68
  gaul_ref[["gnq"]] <- 76
  gaul_ref[["gab"]] <- 89
  gaul_ref[["bwa"]] <- 35
  gaul_ref[["lso"]] <- 142
  gaul_ref[["nam"]] <- 172
  gaul_ref[["zaf"]] <- 227
  gaul_ref[["swz"]] <- 235
  gaul_ref[["zwe"]] <- 271
  gaul_ref[["ben"]] <- 29
  gaul_ref[["bfa"]] <- 42
  gaul_ref[["cmr"]] <- 45
  gaul_ref[["cpv"]] <- 47
  gaul_ref[["tcd"]] <- 50
  gaul_ref[["civ"]] <- 66
  gaul_ref[["gmb"]] <- 90
  gaul_ref[["gha"]] <- 94
  gaul_ref[["gin"]] <- 106
  gaul_ref[["gnb"]] <- 105
  gaul_ref[["lbr"]] <- 144
  gaul_ref[["mli"]] <- 155
  gaul_ref[["mrt"]] <- 159
  gaul_ref[["ner"]] <- 181
  gaul_ref[["nga"]] <- 182
  gaul_ref[["stp"]] <- 214
  gaul_ref[["sen"]] <- 217
  gaul_ref[["sle"]] <- 221
  gaul_ref[["tgo"]] <- 243
  gaul_ref[["bdi"]] <- 43
  gaul_ref[["com"]] <- 58
  gaul_ref[["dji"]] <- 70
  gaul_ref[["eri"]] <- 77
  gaul_ref[["eth"]] <- 79
  gaul_ref[["ken"]] <- 133
  gaul_ref[["mdg"]] <- 150
  gaul_ref[["mwi"]] <- 152
  gaul_ref[["moz"]] <- 170
  gaul_ref[["rwa"]] <- 205
  gaul_ref[["som"]] <- 226
  gaul_ref[["ssd"]] <- 74
  gaul_ref[["tza"]] <- 257
  gaul_ref[["uga"]] <- 253
  gaul_ref[["zmb"]] <- 270
  gaul_ref[["dza"]] <- 4
  gaul_ref[["egy"]] <- 40765
  gaul_ref[["lby"]] <- 145
  gaul_ref[["mar"]] <- 169
  gaul_ref[["sdn"]] <- 6
  gaul_ref[["tun"]] <- 248
  gaul_ref[["essa_hilo"]] <- c(133,270,43,58,70,77,79,150,152,170,205,226,74,257,253,6,142,235,271) # Added LSO, ZWE, swaziland, and SDN
  # Start to build gaul list by matching reference list above
  gaul_list <- lapply(gauls, function(x) {gaul_ref[[x]]})
  names(gaul_list) <- gauls
  # Try to bring in additional gaul codes with gaul_convert
  gaul_list <- lapply(names(gaul_list), function(x) {
    gl <- c()
    if (is.null(gaul_list[[x]]) == T) {
      try(gl <- gaul_convert(x, from = "iso3"))
    } else {
      gl <- gaul_list[[x]]
    }
    # Message if this didn't work
    if (is.null(gl) == T) {
      message(paste0("/nUnable to find a match for the following: ", x))
      message("Check your input vector to ensure valid region names / country codes")
    }
    return(gl)
  })
  # Rearrange gaul_list to be a single vector
  gaul_list <- unlist(gaul_list)
  # Check for duplicates & drop if needed
  if (length(gaul_list) != length(unique(gaul_list))) {
    message("Duplicate gaul codes found - your input regions may overlap.")
    message("Dropping duplicates...")
  }
  gaul_list <- unique(gaul_list)
  return(gaul_list)
}
gaul_convert <- function(countries, from = "iso3", verbose = F) {
  
  # Purpose: Convert a vector of countries (ihme_loc_id format) to vector of GAUL codes
  # Inputs:
  #         countries: vector of countries in ihme_loc_id format
  #         from: format of input
  #               options: "iso3" = "ihme_loc_id" = "ihme_lc_id"
  #                        "name" (the loc_name or loc_nm_short)
  #               ("iso3" is treated as "ihme_loc_id" for backwards compatability)
  #
  # Outputs: a vector of gaul codes
  
  # load reference table
  
  if (Sys.info()["sysname"] == "Linux") {
    j_root <- "/home/j/"
  } else {
    j_root <- "J:/"
  }
  
  str_match <- stringr::str_match
  
  # Catch if already passed gaul codes
  if(class(countries) =="numeric") return (countries)
  if(all(grepl("^[[:digit:]]+$", countries))) return(countries)
  
  table_file <- paste0(j_root, "WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv")
  gaul_table <- read.csv(table_file) %>% data.table
  
  # convert input & output to lower case for easier matching
  #lowercase_cols <- c("short_name", "official_name", "iso3", "iso2", "uni", "undp")
  #gaul_table[, (lowercase_cols) := lapply(.SD, tolower), .SDcols = lowercase_cols,]
  
  ## ## convert input & output to lower case for easier matching
  if (verbose == T) message("\nlowercasing columns. the columns that get lowered are:")
  for(i in 1:ncol(gaul_table)){
    if(class(gaul_table[[i]]) == "factor"){
      if (verbose == T) message(sprintf("On column: %s", colnames(gaul_table)[i]))
      gaul_table[[i]] <- tolower(gaul_table[[i]])
    }
  }
  
  # Lowercase & ensure character for input
  countries <- tolower(countries)
  countries <- as.character(countries)
  
  ## Catch if a subnational in XXX_##### IHME syntax
  ## This returns national-level gaul codes only
  if(any(grepl("_", countries))) {
    countries[grepl("_", countries)] <- str_match(countries[grepl("_", countries)], "(.*)_.*")[,2]
  }
  
  if (from == "iso3" | from == "ihme_loc_id" | from == "ihme_lc_id") {
    
    gaul_code <- sapply(countries, function(x) gaul_table[ihme_lc_id == x, GAUL_CODE]) %>% as.numeric
    
  } else if(from == "name") {
    
    # Matching only national-level for now
    # Drop undefined & subnational rows
    gaul_table_nat <- subset(gaul_table, GAUL_CODE != -1)
    gaul_table_nat <- subset(gaul_table_nat, level == 3)
    
    gaul_code <- sapply(countries, function(x) gaul_table_nat[loc_nm_sh == x, GAUL_CODE]) %>% as.numeric
    
    #check to see if this matched all of the provided items in the vector; use partial / fuzzy matching if not
    if(length(gaul_code[is.na(gaul_code)]) > 0) {
      
      # Create a table to fill in
      table_matching <- cbind(countries, gaul_code) %>% as.data.table
      names(table_matching) <- c("country", "gaul_code")
      table_matching$gaul_code <- as.numeric(table_matching$gaul_code)
      
      approx_matched <- table_matching[is.na(gaul_code), country]
      
      # Indicate that approximate matching took place
      
      message("\nNot all country names provided were found in the lookup table.")
      message("Attempting to match names provided with those in lookup table.")
      message(paste0("Approximate matching attempted for: ", paste(approx_matched, collapse = ', '), "\n"))
      
      approx_match <- function(country) {
        # First, try matching to long form of name
        gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name),]$GAUL_CODE
        
        # If that doesn't work, grep within the short name
        if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_nm_sh),]$GAUL_CODE
        
        # If that doesn't work, grep within the long name
        if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name),]$GAUL_CODE
        
        # Could fill in other matching here if desired
        
        # Warn if nonspecific
        if (length(gaul_code) > 1) warning(paste0("\"", country, "\" matches multiple country names in the lookup table. Please be more specific."))
        
        # Finally, if no matches, return NA
        if (length(gaul_code) != 1) gaul_code <- NA
        
        return(as.numeric(gaul_code))
      }
      
      # Try approximate matching
      table_matching[is.na(gaul_code)]$gaul_code <- sapply(table_matching[is.na(gaul_code)]$country,approx_match)
      
      not_matched <- table_matching[is.na(gaul_code)]$country
      
      # Error checking
      if(length(not_matched) > 0) {
        warning(paste0("Some countries could not be matched:\n", paste(not_matched, collapse=', ')))
      }
      gaul_code <- table_matching$gaul_code %>% as.numeric
    }
    
  } else {
    # Error catching for non-supported country type
    stop("\nPlease enter a valid country code type")
  }
  
  if(length(gaul_code[is.na(gaul_code)]) > 0){
    # Error catching for failure to match all country codes
    message(paste0("CAUTION! Returning NA values.\nMatches not found for all country codes in input list.\n",
                   "Please check your input values"))
  }
  return(gaul_code)
}

#set up directory
input_dir <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_JRF_matching/2016/"
input_dir2017 <- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_JRF_matching/2017/"
input_dir2018<- "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_JRF_matching/2018/"

#load in JRF data 
# 2016
df_who <- fread(paste0(input_dir, "Data_request_20171201_raw.csv"))
#2018
df_who18 <- fread(paste0(input_dir2018, "JRF2018_rawfile.csv"))
# JRF data set: choose only one vaccine to remove redundant. Choose only name column and coverage
setnames(df_who, c("Iso Code", "Country Name", "Admin1", "Admin2", "Coverage", "Vaccine Type"),
         c("iso3", "ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "who_coverage", "vaccine"))
setnames(df_who18, c("iso", "CountryName", "Admin1", "Admin2", "Coverage"),
         c("iso3", "ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "who_coverage"))
# 2017
df_who_afr <- fread(paste0(input_dir2017, "JRF2017raw_Subnational_data_africa.csv"))
df_who_world <- fread(paste0(input_dir2017, "JRF2017raw_Subnational_data_world.csv"))
df_who <- rbind(df_who_afr, df_who_world)

setnames(df_who, c("iso", "Country.who", "Admin1", "Admin2", "Valor", "Vaccode"),
         c("iso3", "ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "who_coverage", "vaccine"))



# Select useful columns 
df_who <- subset(df_who, select = c("iso3", "ADM0_NAME","Year", "vaccine","ADM1_NAME", "ADM2_NAME", "who_coverage", "Denominator"))
df_who18 <- subset(df_who18, select = c("iso3", "ADM0_NAME","Year", "Vaccine","ADM1_NAME", "ADM2_NAME", "who_coverage", "Numerator","Denominator"))
#load shapefile files to match with JRF data
# gadm: ad1 and ad2 are new gadm shapefile; ad3 is old gadm shapefile
df_gadm3 <- fread(paste0(input_dir, "lf_gadm28_adm3.csv"))
df_gadm2 <- fread(paste0(input_dir, "gadm_36_ad2.csv"))
df_gadm1 <- fread(paste0(input_dir, "gadm_36_ad1.csv"))

# gaul shapefile
df_gl1 <- fread(paste0(input_dir, "lf_g2015_2014_1_addiso3.csv"))
df_gl2 <- fread(paste0(input_dir, "lf_g2015_2014_2_addiso3.csv"))

#specific shapefile
df_COD <- fread(paste0(input_dir, "COD_health2_repaired.csv"))
df_ZMB <- fread(paste0(input_dir,"Zambia_ConstituenciesAndWards_2006to2010_UTM35S.csv"))
df_ETH <- fread(paste0(input_dir_2017, "EthiopiaWoredaLsib2016Nov.csv"))
df_CMR <- fread(paste0(input_dir, "Cameroon_HealthDistricts12012016.csv"))
df_BDI <- fread(paste0(input_dir, "district_sanitaire_burundi_mai2016v3.csv"))
df_BWA <- fread(paste0(input_dir,"BotswanaHealthDistrictsLsib2016July.csv"))
df_PAHO <- fread(paste0(input_dir,"AMERICAS_ADM3.csv"))
df_PAHO[df_PAHO=="Brasil"]<- "Brazil"

df_polio_who <- fread( "who_adm2.csv")
df_polio_who <- subset(df_polio_who, select = c("iso3", "ADM0_NAME", "ADM1_NAME", "NAME", "GAUL_CODE"))

# SET NAME to either ADM0/1/2/3_NAME or NAME so that function can work (DO AS NECESSARY)
setnames(df_gadm3, c("NAME_0","NAME_1","NAME_2", "NAME_3","ISO"), c("ADM0_NAME","ADM1_NAME","ADM2_NAME","NAME","iso3"))
setnames(df_gadm2, c("NAME_0","NAME_1","NAME_2","ISO"), c("ADM0_NAME","ADM1_NAME","NAME","iso3"))
setnames(df_gadm1, c("ADM1_NAME","ISO"), c("NAME","iso3"))
setnames(df_COD, c("Name", "PROVNAME"), c("NAME", "ADM1_NAME"))
setnames(df_PAHO, c("CTRY_ISO_N","ADM1_ISO_N","ADM3_ISO_N", "GIS_CODE"), c("ADM0_NAME", "ADM1_NAME","NAME", "GAUL_CODE"))
# change who/jrf most granular adm name to the same as shapefile most granular column name (DO AS NECESSARY)
setnames(df_who, "ADM2_NAME", "NAME")
setnames(df_who18, "ADM2_NAME", "NAME")

#temporary change
setnames(df_who, c("NAME","ADM2_NAME"),c("ADM1_NAME","NAME"))


# Clean up names (fix diacritics then  #change to lower case then #remove special symbol, character)
fix_adm_name <- function(shpfile){

  #fix diacritics then  #change to lower case then #remove special symbol, character
  if ("ADM0_NAME" %in% colnames(shpfile)){
  shpfile[, ADM0_NAME := fix_diacritics(ADM0_NAME)] 
  }
  if ("ADM1_NAME" %in% colnames(shpfile)){
    shpfile[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
    shpfile[, ADM1_NAME := tolower(ADM1_NAME)]
    shpfile[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
    shpfile[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]
  }
  if ("ADM2_NAME" %in% colnames(shpfile)){
  shpfile[, ADM2_NAME := fix_diacritics(ADM2_NAME)]
  shpfile[, ADM2_NAME := tolower(ADM2_NAME)]  
  shpfile[, ADM2_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
  shpfile[, ADM2_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
  }
  if ("ADM3_NAME" %in% colnames(shpfile)){
    shpfile[, ADM3_NAME := fix_diacritics(ADM2_NAME)]
    shpfile[, ADM3_NAME := tolower(ADM2_NAME)]  
    shpfile[, ADM3_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
    shpfile[, ADM3_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
  }
  if ("NAME" %in% colnames(shpfile)){
  shpfile[, NAME := fix_diacritics(NAME)]
  shpfile[, NAME := tolower(NAME)]
  shpfile[, NAME := gsub("[[:punct:]]", "", NAME)]
  shpfile[, NAME := gsub("[[:space:]]", "", NAME)]
  }
 
}
fix_adm_name(df_who18)

fix_adm_name(df_gadm3)
fix_adm_name(df_gadm2)
fix_adm_name(df_gadm1)
fix_adm_name(df_gl2)
fix_adm_name(df_gl1)

fix_adm_name(df_polio_who)
fix_adm_name(df_PAHO)
#--------------------------------------------------------------Name matching adm1---------------------------------------------------------------------------#
# admname can be adm1,2, or 3
matching_name <- function(countries, shapefile, who_year, fuzMethod, admname){
  df_combined <- list()
  df_who_only <- list()
  df_shpfile_only <- list()
  df_success <- list()
  df_fuzzy <- list()
  df_fuzzy_fix <- list()
  df_fuzzy_exact <- list()
  df_who_miss <- list()
  df_shp_miss <- list()
  df_combined_final <- list()
  df_combined_final1 <- list()
  df_combined_final2 <- list()
  df_combined_final3 <- list()
  df_combined_final4 <- list()
  df_combined_final5 <- list()
  df_combined_final6 <- list()
  df_combined_final7 <- list()
  df_combined_final8 <- list()
  
  
  # start the for loop matching for all african countries using gadm shapefile
  for (i in 1 : length(countries)){
    df_combined[[i]]<- merge(who_year[ADM0_NAME == countries[i]], shapefile[ADM0_NAME == countries[i]], by = c(admname), all.x = T, all.y = T)
    #regular match status
    df_combined[[i]][!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
    df_combined[[i]][!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not shpfile"]
    df_combined[[i]][is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in shpfile but not WHO"]
    #subset each status to different list for fuzzy match
    df_who_only[[i]] <- subset(df_combined[[i]], merge_status == "in WHO but not shpfile")
    df_shpfile_only[[i]] <- subset(df_combined[[i]], merge_status == "in shpfile but not WHO")
    df_success[[i]] <- subset(df_combined[[i]], merge_status == "success")
    
    #fuzzy match
    df_fuzzy[[i]] <- stringdist_join(df_who_only[[i]], df_shpfile_only[[i]], by = c(admname), method = fuzMethod)
    df_fuzzy[[i]]$merge_status <- NA

    #identify fuzzy match with more than 1 match
    for (j in 1:(nrow(df_fuzzy[[i]])-1)){
    # first if statement make sure we have at least 2 fuzzy match so that the second if statement makes sense
    if  (nrow(df_fuzzy[[i]]) <= 1){
      next
    } else if (df_fuzzy[[i]]$NAME.x[j] == df_fuzzy[[i]]$NAME.x[j+1]) {
      df_fuzzy[[i]]$merge_status[j] <- "fuzzy need fix"
      df_fuzzy[[i]]$merge_status[j+1] <- "fuzzy need fix"
    }
  }


    df_fuzzy_fix[[i]] <- df_fuzzy[[i]][merge_status == "fuzzy need fix"]
    setnames(df_fuzzy_fix[[i]], c("NAME.x", "NAME.y"), c("NAME.who", "NAME.shpfile"))
    # fuzzy match with only one shapefile location will have NA on merge status
    df_fuzzy_exact[[i]] <- df_fuzzy[[i]][is.na(merge_status)]
    setnames(df_fuzzy_exact[[i]], c("NAME.x", "NAME.y"), c("NAME.who", "NAME.shpfile"))
    
    df_combined[[i]] <- rbind(df_success[[i]], df_fuzzy_fix[[i]], df_fuzzy_exact[[i]], df_who_only[[i]], fill = T)
    df_combined[[i]][is.na(merge_status), merge_status := "fuzzy success"]
    df_combined[[i]][is.na(NAME.who), NAME.who := NAME]
    # for location in shapefile but do not get match in who location
    df_combined[[i]] <- rbind(df_combined[[i]], df_shpfile_only[[i]], fill =T)
    ##df_combined[[i]][merge_status == "in shpfile but not WHO", NAME.shpfile := NAME]
    #sort out location in WHO which never get match (regular and fuzzy match)
    df_who_miss[[i]] <- unique(df_combined[[i]], by= c("who_coverage", "NAME.who"))
    #sort out location in shapefile which not get mactch with WHO location
    df_shp_miss[[i]] <- unique(df_combined[[i]], by = c("NAME.shpfile", "GAUL_CODE"))
    
    #final data set with matching
    df_combined_final[[i]] <- rbind(df_combined[[i]][!(merge_status %in% c("in WHO but not shpfile", "in shpfile but not WHO"))], df_who_miss[[i]][merge_status == "in WHO but not shpfile",], df_shp_miss[[i]][merge_status == "in shpfile but not WHO"])
    
    # select approriate column only 
    df_combined_final1[[i]] <- subset(df_combined_final[[i]], select = grepl("^NAME", names(df_combined_final[[i]])))
    df_combined_final2[[i]] <- subset(df_combined_final[[i]], select = grepl("iso3", names(df_combined_final[[i]])))
    df_combined_final3[[i]] <- subset(df_combined_final[[i]], select = grepl("who_coverage", names(df_combined_final[[i]])))
    df_combined_final4[[i]] <- subset(df_combined_final[[i]], select = grepl("merge_status", names(df_combined_final[[i]])))
    df_combined_final5[[i]] <- subset(df_combined_final[[i]], select = grepl("GAUL_CODE", names(df_combined_final[[i]])))
    df_combined_final6[[i]] <- subset(df_combined_final[[i]], select = grepl("vaccine", names(df_combined_final[[i]])))
    df_combined_final7[[i]] <- subset(df_combined_final[[i]], select = grepl("ADM1_NAME", names(df_combined_final[[i]])))
    df_combined_final8[[i]] <- subset(df_combined_final[[i]], select = grepl("Denominator", names(df_combined_final[[i]])))
    df_combined_final[[i]] <- cbind(df_combined_final1[[i]], df_combined_final2[[i]], df_combined_final3[[i]], df_combined_final4[[i]], df_combined_final5[[i]], df_combined_final6[[i]], df_combined_final7[[i]], df_combined_final8[[i]])
    
    #reformat into nicer table
    #iso3
    if ("iso3.x.x" %in% colnames(df_combined_final[[i]])){
      df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), iso3.x := iso3.x.x]
    }
    df_combined_final[[i]][merge_status == "in shpfile but not WHO", iso3.x := iso3.y]
    #ADM1_NAME
    df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), ADM1_NAME := ADM1_NAME.x]
    df_combined_final[[i]][merge_status == "in shpfile but not WHO", ADM1_NAME := ADM1_NAME.y]
    #vaccine
    df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), vaccine := vaccine.x]
    
    #name
    df_combined_final[[i]] [merge_status == "success", NAME.shpfile := NAME.who]
    df_combined_final[[i]] [merge_status == "in shpfile but not WHO", NAME.shpfile := NAME]
    #who coverage
    df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), who_coverage := who_coverage.x]
    #denominator
    df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), Denominator := Denominator.x]
    #GAUL CODE
    df_combined_final[[i]][merge_status %in% c("fuzzy need fix", "fuzzy success"), GAUL_CODE := GAUL_CODE.y]
    #remove duplicate
    fuzzySuccess.who <- df_combined_final[[i]][merge_status == "fuzzy success", "NAME.who"][[1]]
    fuzzySuccess.shp <- df_combined_final[[i]][merge_status == "fuzzy success", "NAME.shpfile"][[1]]
    df_combined_final[[i]][merge_status == "in WHO but not shpfile" & NAME.who %in% fuzzySuccess.who, merge_status:="duplicate"]
    df_combined_final[[i]][merge_status == "in shpfile but not WHO" & NAME.shpfile %in% fuzzySuccess.shp, merge_status := "duplicate"]
    df_combined_final[[i]] <- df_combined_final[[i]][merge_status != "duplicate"]
    # take out necessary columns only
    df_combined_final[[i]] <- subset(df_combined_final[[i]], select= c("NAME.who", "NAME.shpfile","ADM1_NAME","vaccine", "iso3.x", "who_coverage", "merge_status", "GAUL_CODE", "Denominator"))
    setnames(df_combined_final[[i]], c("iso3.x","ADM1_NAME"), c("iso3","NAME.who_adm1"))

    }
  
  return(df_combined_final)
}

# run the matching function on countries and shpfile of interest
paho <- c('Panama')
paho_match <- matching_name(paho, df_PAHO, df_who,"osa", c("NAME")) %>% rbindlist(.)

write.csv(Afr_match, paste0(input_dir2017,"Afr.csv")) 

#-------------------------------------------------------binding 2new matching into current matching sheet-----------------------------------------------------------#
#read unit match csv to determine which shapefile to use for matching
unit_match <- fread(paste0(input_dir2017, "JRF2017_unitmatch.csv"))
gaul2 <- unit_match[`Unit matchStatus` == "ok match" &  shapefile == "gaul", "countries"][[1]]

#matching
gaul2_match <- matching_name(gaul2, df_gl2, "osa", c("ADM1_NAME","NAME")) %>% rbindlist(.)

#remove those current not match to add new match
df_match <- fread(paste0(input_dir2017,"JRF2017_matching.csv"))
df_match <- df_match[!(ADM0_NAME %in% "Botswana" & is.na(shapefile))]

BWA_match <- fread(paste0(input_dir2017,"BWA.csv"))
df_match <- rbind(df_match, BWA_match, fill = T)
write.csv(df_match, paste0(input_dir2017,"JRF2017_matching.csv")) 

#--------------------------------------Name matching JRF 2017------------------------------------------#
allCountry <- fread(paste0(input_dir2017,"unitmatch2017.csv")) 

gadm2 <- allCountry[Status == "total match" & gadm_adm2 == 100 , "countries"][[1]]
gaul2 <- allCountry[gadm_adm2 !=100 & Status == "total match" & gaul_adm2 == 100 , "countries"][[1]]
gadm1 <- allCountry[gadm_adm2 !=100 & gaul_adm2 !=100 & Status == "total match" & gadm_adm1 == 100 , "countries"][[1]]
gaul1 <- allCountry[gadm_adm2 !=100 & gaul_adm2 !=100 & gadm_adm1 != 100 & Status == "total match" & gaul_adm1 == 100 , "countries"][[1]]


gadm2_2017 <- matching_name(gadm2, df_gadm2, "osa", "NAME") %>% rbindlist(.)
gaul2_2017 <- matching_name(gaul2, df_gl2, "osa", "NAME") %>% rbindlist(.)
gadm1_2017 <- matching_name(gadm1, df_gadm1, "osa", "NAME") %>% rbindlist(.)
gaul1_2017 <- matching_name(gaul1, df_gl1, "osa", "NAME") %>% rbindlist(.)

nameMatch2017 <- rbind(gadm1_2017,gaul1_2017,gadm2_2017,gaul2_2017)
write.csv(nameMatch2017, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/WHO_JRF_matching/2016/namematch2017.csv")

BRA <- fread(paste0(input_dir2017, "country_fuzzy_fix"))
fuzzy_BRA <- stringdist_join(df_who_only[[i]], df_shpfile_only[[i]], method = soundex)

#------------------------------------------------------fix the duplicates in location code-------------------------------------------------------------------------------------------------------#
df_jrf_all <- fread(paste0(input_dir2017, "JRF2017_matching.csv")) %>% .[ADM0_NAME == "India"]
dup_loc_code <- df_jrf_all[duplicated(df_jrf_all$location_code), location_code] %>% .[!is.na(.)] %>% unique
IND_dup <- df_jrf_all[location_code %in% dup_loc_code,]



library(plyr)
df_unit <- fread(paste0(input_dir2017,"whocombined_namefixdiacritics.csv"))
ter_countries <- fread(paste0(input_dir2017,"JRF2017_unitmatch.csv"))

unit_count <- count(df_unit, "ADM0_NAME") %>% setnames(., "ADM0_NAME", "countries")
unit_count<- unit_count[unit_count$countries %in% ter_countries$countries,]

df_unit2 <- merge(unit_count, ter_countries, all.x =T, all.y =T)

write.csv(df_unit2, paste0(input_dir2017, "unit_match2.csv"))
write.csv(df_who18, paste0(input_dir2018, "JRF2018_raw_fixdiacritics.csv"))

write.csv(df_polio_who, "~/IHME/Work/Vaccine (local)/WHO_JRF_matching/2016/who_adm2_fixed.csv")
