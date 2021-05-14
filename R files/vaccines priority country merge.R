library(data.table)
library(magrittr)
library(fuzzyjoin)
library(stringr)

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

### Active code to merge

input_dir <- "J:/temp/nqviet94/Vaccines/"
df_who <- fread(paste0(input_dir, "Data_request_20171201.csv"))
df_eth <- fread("J:/temp/nqviet94/Vaccines/eth_dpt3_cov_merged_adm_summary.csv")
df_eth1 <- read.csv(file="J:/temp/nqviet94/Vaccines/eth_dpt3_cov_merged_adm_summary.csv",header=TRUE,sep=",")
df_ssd <- fread(paste0(input_dir, "ssd_dpt3_cov_merged_adm_summary.csv"))
df_ner <- fread(paste0(input_dir, "ner_adm2_dpt3_cov_merged_adm_summary.csv"))
df_tcd <- fread(paste0(input_dir, "tcd_adm3_dpt3_cov_merged_adm_summary.csv"))

df_eth <- subset(df_eth, year == 2016)
df_ssd <- subset(df_ssd, year == 2016)
df_ner <- subset(df_ner, year == 2016)
df_tcd <- subset(df_tcd, year == 2016)

df_who <- subset(df_who, select = c("Iso Code", "Country Name", "Year", "Admin1", "Admin2", "Denominator", "Numerator", "Coverage", "Vaccine Type"))
setnames(df_who, c("Iso Code", "Country Name", "Year", "Admin1", "Admin2", "Denominator", "Numerator", "Coverage", "Vaccine Type"),
         c("iso3", "country", "year", "ADM1_NAME", "ADM2_NAME", "who_denom", "who_numer", "who_coverage", "vaccine"))
df_who <- subset(df_who, vaccine == "DTP3")


df_eth <- subset(df_eth, select = c("ISO", "NAME_0", "year", "NAME_1", "NAME_3", "GAUL_CODE", "mean", "upper", "lower", "cirange"))
setnames(df_eth, c("ISO", "NAME_0", "year", "NAME_1", "NAME_3", "GAUL_CODE", "mean", "upper", "lower", "cirange"), 
         c("iso3", "country", "year", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "mean", "upper", "lower", "cirange"))

df_ssd <- subset(df_ssd, select = c("admin0Name", "year", "admin1Name", "admin2Name", "GAUL_CODE", "mean", "upper", "lower", "cirange"))
setnames(df_ssd, c("admin0Name", "year", "admin1Name", "admin2Name", "GAUL_CODE", "mean", "upper", "lower", "cirange"),
         c("country", "year", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "mean", "upper", "lower", "cirange"))

df_ner <- subset(df_ner, select = c("NAME_0", "year", "NAME_1", "NAME_2", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"))
setnames(df_ner, c("NAME_0", "year", "NAME_1", "NAME_2", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"),
         c("country", "year", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"))

df_tcd <- subset(df_tcd, select = c("NAME_0", "year", "NAME_1", "NAME_3", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"))
setnames(df_tcd, c("NAME_0", "year", "NAME_1", "NAME_3", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"),
         c("country", "year", "ADM1_NAME", "ADM3_NAME", "GAUL_CODE", "mean", "upper", "lower", "cirange", "pop"))

# Clean up names
# fix accent (above or bwlow letters)
df_who[, country := fix_diacritics(country)]
df_eth[, country := fix_diacritics(country)]
df_ssd[, country := fix_diacritics(country)]
df_ner[, country := fix_diacritics(country)]
df_tcd[, country := fix_diacritics(country)]
# change adm name to lowercase letter
df_who[, ADM2_NAME := tolower(ADM2_NAME)]
df_eth[, ADM2_NAME := tolower(ADM2_NAME)]
df_ssd[, ADM2_NAME := tolower(ADM2_NAME)]
df_ner[, ADM2_NAME := tolower(ADM2_NAME)]
df_tcd[, ADM3_NAME := tolower(ADM3_NAME)]

df_who[, ADM1_NAME := tolower(ADM1_NAME)]
df_eth[, ADM1_NAME := tolower(ADM1_NAME)]
df_ssd[, ADM1_NAME := tolower(ADM1_NAME)]
df_ner[, ADM1_NAME := tolower(ADM1_NAME)]
df_tcd[, ADM1_NAME := tolower(ADM1_NAME)]
#fix accent for adm name
df_who[, ADM2_NAME := fix_diacritics(ADM2_NAME)]
df_eth[, ADM2_NAME := fix_diacritics(ADM2_NAME)]
df_ssd[, ADM2_NAME := fix_diacritics(ADM2_NAME)]
df_ner[, ADM2_NAME := fix_diacritics(ADM2_NAME)]
df_tcd[, ADM3_NAME := fix_diacritics(ADM3_NAME)]

df_who[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
df_eth[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
df_ssd[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
df_ner[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
df_tcd[, ADM1_NAME := fix_diacritics(ADM1_NAME)]
# Replace punctuation character in adm name with space
df_who[, ADM2_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
df_eth[, ADM2_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
df_ssd[, ADM2_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
df_ner[, ADM2_NAME := gsub("[[:punct:]]", "", ADM2_NAME)]
df_tcd[, ADM3_NAME := gsub("[[:punct:]]", "", ADM3_NAME)]

df_who[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
df_eth[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
df_ssd[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
df_ner[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
df_tcd[, ADM1_NAME := gsub("[[:punct:]]", "", ADM1_NAME)]
# Replace space character in adm name with space
df_who[, ADM2_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
df_eth[, ADM2_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
df_ssd[, ADM2_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
df_ner[, ADM2_NAME := gsub("[[:space:]]", "", ADM2_NAME)]
df_tcd[, ADM3_NAME := gsub("[[:space:]]", "", ADM3_NAME)]

df_who[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]
df_eth[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]
df_ssd[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]
df_ner[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]
df_tcd[, ADM1_NAME := gsub("[[:space:]]", "", ADM1_NAME)]


## Subset WHO data to only the countries of interest
who_code_lookup = data.table(iso3 = unique(df_who$iso3))
#assign admin0 gaul code to the countries of interest
who_code_lookup[, ADM0_CODE := gaul_convert(iso3)]
#add admin0 column to df_who datatable
df_who <- merge(df_who, who_code_lookup)

##Ethiopia/SSD/NER match and merges
df_eth_who <- subset(df_who, ADM0_CODE == 79)
df_ssd_who <- subset(df_who, ADM0_CODE == 74)
df_ner_who <- subset(df_who, ADM0_CODE == 181)
df_tcd_who <- subset(df_who, ADM0_CODE == 50)
#change column name adm2name to adm3 name in df_tcd_who
setnames(df_tcd_who, c("ADM2_NAME"), c("ADM3_NAME"))

df_eth_combined <- merge(subset(df_eth_who, select = c("ADM2_NAME", "ADM0_CODE", "who_denom", "who_numer", "who_coverage", "country")), df_eth, by = c("country", "ADM2_NAME"), all.x = T, all.y = T)
                                                                                                                                

df_eth_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_eth_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_eth_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_ssd_combined <- merge(subset(df_ssd_who, select = c("ADM2_NAME", "ADM0_CODE", "who_denom", "who_numer", "who_coverage", "country")), df_ssd, by = c("country", "ADM2_NAME"), all.x = T, all.y = T)

df_ssd_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_ssd_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_ssd_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_ner_combined <- merge(subset(df_ner_who, select = c("ADM2_NAME", "ADM0_CODE", "who_denom", "who_numer", "who_coverage", "country")), df_ner, by = c("country", "ADM2_NAME"), all.x = T, all.y = T)

df_ner_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_ner_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_ner_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_tcd_combined <- merge(subset(df_tcd_who, select = c("ADM3_NAME", "ADM0_CODE", "who_denom", "who_numer", "who_coverage", "country")), df_tcd, by = c("country", "ADM3_NAME"), all.x = T, all.y = T)

df_tcd_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_tcd_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_tcd_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]
#playing
#df_eth_combined[GAUL_CODE>200, gaul_code:="gaul code is large"]
#df_eth_combined <- df_eth_combined[,1:15]
##Ethiopia Fuzzy match
df_eth_whofuzz <- subset(df_eth_combined, merge_status == "in WHO but not MBG")
df_eth_mbgfuzz <- subset(df_eth_combined, merge_status == "in MBG but not WHO")


df_eth_fuzzy <- stringdist_inner_join(df_eth_whofuzz, df_eth_mbgfuzz, by = c("ADM2_NAME"), method = 'soundex')
df_eth_fuzzy_adm2 <- subset(df_eth_fuzzy,select=c("ADM2_NAME.x","ADM2_NAME.y"))
df_eth_fuzzy <- df_eth_fuzzy[-c(1,3,7,12,15,19,21,26,28,31,34,37,42:44,46,47,49,51:54,59,62,67,68,77,84,89,90,97:99,101:103,107,113,114,118,121,123:125,128,131,133,137,153,163,165,166,171,172,
                                174,177,180,182,187,190,191,193,195,199,200,201,205:210,212,222,224,230,237:239,241,242,247,258:260,264,265,268,269,271,272,275,276,280)]

df_eth_fuzzy <- subset(df_eth_fuzzy, select = c("ADM0_CODE.x", "iso3.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "merge_status.y"))
setnames(df_eth_fuzzy, c("ADM0_CODE.x", "iso3.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "merge_status.y"), c("ADM0_CODE", "iso3", "country", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "who_denom", "who_numer", "who_coverage", "year", "mean", "lower", "upper", "cirange", "merge_status"))
df_eth_combined <- df_eth_combined[-c(6,7,15,16,18,25,26,29,42,43,46,47,49,50,54,55,59,63,64,66,68,69,85,86,89:94,83,82,96:99,104,105,112,113,117,122,125,126,135,136,141:144,151,152,149,162,
                                      163,173,174,181,186,210:213,228,232,236,237,246,256,248,249,255,268,271,273,332,333,275,276,278,279,281,282,284,286,287,289,290,346,293,294,304:308,
                                      310,312,315,316,360,341,344,351,373,349,295,357,359,361,363,364,338,366,367,370:372,334,374,376,318,387,391,393:397,401,402,405,406,413:418,425,428:430,
                                      432,433,439,440,445,446,461,462,464,483,480,534,487,489,490,491,482,475,506,520,522,523,525:528,530,532,535,539,543,544,513,548,557,558,568,569,581,582,
                                      586,590,587,588,593,598,600,601,608,609,618,619,632,633,644,645,650,651,653,654,660,661,664,665,668:673,678,676,686:688,691,694,696,701,702,709,711,720:723,
                                      736,737,740,741,725,728:731,742,744,756,760:763,766,768,770:772,797:799,832,781,831,793:795,767,769,801,809,810,814:816,775,833,834,843,844,846,
                                      847,860,861,870,871,873,874,876,877,881,882,889,890,895,896,918,919,924:926,945,929,954,930,955,935,1007,936,938,922,941,946,928,927,947,948,949,960,
                                      1001,939,967,940,968,971,972,979,980,982:985,990,991,956,961,999,1002,1010,1012:1014,1025:1030,1033,1034,1036,1046,1062,1063,1072,1073,1080:1082,1085,
                                      1088,1112,1074,1094,1099,1100,1107,1086,1121,1122,1118,1126,1128,1129,1134,1135,1137,1140)]

df_eth_combined <- rbind(df_eth_combined, df_eth_fuzzy)
df_eth_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_eth_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_eth_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_eth_success <- subset(df_eth_combined, merge_status == "success")

#SSD fuzzy match and bind
df_ssd_whofuzz <- subset(df_ssd_combined, merge_status == "in WHO but not MBG")
df_ssd_mbgfuzz <- subset(df_ssd_combined, merge_status == "in MBG but not WHO")


df_ssd_fuzzy <- stringdist_inner_join(df_ssd_whofuzz, df_ssd_mbgfuzz, by = c("ADM2_NAME"), method = 'soundex')
df_ssd_fuzzy <- subset(df_ssd_fuzzy, select = c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "merge_status.y"))
setnames(df_ssd_fuzzy, c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "merge_status.y"), c("ADM0_CODE", "country", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "who_denom", "who_numer", "who_coverage", "year", "mean", "lower", "upper", "cirange", "merge_status"))

df_ssd_combined <- df_ssd_combined[-c(5,6,37,38,64,65,70,71,73,74)]

df_ssd_combined <- rbind(df_ssd_combined, df_ssd_fuzzy)

df_ssd_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_ssd_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_ssd_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_ssd_success <- subset(df_ssd_combined, merge_status == "success")

##NER fuzzy match and bind
df_ner_whofuzz <- subset(df_ner_combined, merge_status == "in WHO but not MBG")
df_ner_mbgfuzz <- subset(df_ner_combined, merge_status == "in MBG but not WHO")


df_ner_fuzzy <- stringdist_inner_join(df_ner_whofuzz, df_ner_mbgfuzz, by = c("ADM2_NAME"), method = 'soundex')
df_ner_fuzzy <- df_ner_fuzzy[-c(5:9)]
df_ner_fuzzy <- subset(df_ner_fuzzy, select = c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "pop.y", "merge_status.y"))
setnames(df_ner_fuzzy, c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM2_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "pop.y", "merge_status.y"), c("ADM0_CODE", "country", "ADM1_NAME", "ADM2_NAME", "GAUL_CODE", "who_denom", "who_numer", "who_coverage", "year", "mean", "lower", "upper", "cirange", "pop", "merge_status"))

df_ner_combined <- df_ner_combined[-c(3,4,15,16,18:23,48,50,51,53,55,56)]

df_ner_combined <- rbind(df_ner_combined, df_ner_fuzzy)

df_ner_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_ner_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_ner_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_ner_success <- subset(df_ner_combined, merge_status == "success")

##TCD match and merge
df_tcd_whofuzz <- subset(df_tcd_combined, merge_status == "in WHO but not MBG")
df_tcd_mbgfuzz <- subset(df_tcd_combined, merge_status == "in MBG but not WHO")

df_tcd_fuzzy <- stringdist_inner_join(df_tcd_whofuzz, df_tcd_mbgfuzz, by = c("ADM3_NAME"), method = 'soundex')
df_tcd_fuzzy <- df_tcd_fuzzy[c(4,11,17,19,29)]

df_tcd_fuzzy <- subset(df_tcd_fuzzy, select = c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM3_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "pop.y", "merge_status.y"))
setnames(df_tcd_fuzzy, c("ADM0_CODE.x", "country.x", "ADM1_NAME.y", "ADM3_NAME.x", "GAUL_CODE.y", "who_denom.x", "who_numer.x", "who_coverage.x", "year.y", "mean.y", "lower.y", "upper.y", "cirange.y", "pop.y", "merge_status.y"), c("ADM0_CODE", "country", "ADM1_NAME", "ADM3_NAME", "GAUL_CODE", "who_denom", "who_numer", "who_coverage", "year", "mean", "lower", "upper", "cirange", "pop", "merge_status"))

df_tcd_combined <- df_tcd_combined[-c(36,37,133,134,240,241,262,302,390,391)]

df_tcd_combined <- rbind(df_tcd_combined, df_tcd_fuzzy)

df_tcd_combined[!is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "success"]
df_tcd_combined[!is.na(who_coverage) & is.na(GAUL_CODE), merge_status := "in WHO but not MBG"]
df_tcd_combined[is.na(who_coverage) & !is.na(GAUL_CODE), merge_status := "in MBG but not WHO"]

df_tcd_success <- subset(df_ner_combined, merge_status == "success")

##CSV export
write.csv(df_eth_success, "J:/temp/jbhall/vaccines/eth_success0427.csv")
write.csv(df_eth_combined, "J:/temp/jbhall/vaccines/eth_fullmerge0427.csv")
write.csv(df_ssd_success, "J:/temp/jbhall/vaccines/ssd_success0427.csv")
write.csv(df_ssd_combined, "J:/temp/jbhall/vaccines/ssd_fullmerge0427.csv")
write.csv(df_ner_success, "J:/temp/jbhall/vaccines/ner_success0430.csv")
write.csv(df_ner_combined,"J:/temp/jbhall/vaccines/ner_fullmerge0430.csv")
