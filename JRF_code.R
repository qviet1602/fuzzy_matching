#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#           COD MCV:  jrf reports, custom aggregated surveys, and MBG estimates       #
#                                  Alyssa Sbarra                                      #
#                                 12 February 2019                                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())

user               <- Sys.info()['user']
core_repo          <- sprintf('/share/code/geospatial/%s/lbd_core/',user)
indic_repo         <- sprintf('/share/code/geospatial/%s/vaccine/',user)
remote             <- 'origin'
branch             <- 'develop'
pullgit            <- FALSE

commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))

# Ensure that required packages are loaded
package_list <- unique(c(package_list,
                         "fasterize",
                         "sf", "stringr"))

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

source(paste0(indic_repo,'functions/misc_vaccine_functions.R'))
source('/share/scratch/users/asbarra/helper_functions.R')

# Options
country_to_compare <- "COD"
vaccine <- 'dpt'

# COD, ECU is a mess
# MDG
# SUR 


# General
if(vaccine== 'mcv'){
  indicator='mcv1_cov'
  ind_title <- "MCV1"
  indicator_group='vaccine'
  run_date = '2018_12_21_21_18_19'
  indicator_family = "binomial"
  svy_id = "svy_id"
  sample_column = "SSW"
  subnational_nids = NULL
  shapefile_version = 'current'
  raked <- TRUE
  most_recent_year <- 2017
  year_list <- c(2000:2017)
  out_dir <- "/share/scratch/users/asbarra/jrf_comparisons/mcv/"
  input_data <- fread(paste0('/share/geospatial/mbg/vaccine/mcv1_cov/output/',run_date,'/input_data.csv'))
  vacc_name <- "MCV1"
}


# General
if(vaccine== 'dpt'){
  indicator='dpt3_cov'
  ind_title <- "DPT3"
  indicator_group='vaccine'
  run_date = '2018_12_21_21_20_34'
  indicator_family = "binomial"
  svy_id = "svy_id"
  sample_column = "SSW"
  subnational_nids = NULL
  shapefile_version = 'current'
  raked <- TRUE
  most_recent_year <- 2017
  year_list <- c(2000:2017)
  out_dir <- "/share/scratch/users/asbarra/jrf_comparisons/dpt/"
  input_data <- fread(paste0('/share/geospatial/mbg/vaccine/dpt3_cov/output/',run_date,'/input_data.csv'))
  vacc_name <- "DTP3"
}



###########################################################################
# Country-specific
if (country_to_compare == "DZA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Algeria"
  regions = "name"
  country_prefix <- "DZA"
  ctry_nid <- 210614
  compare_svy_name <- "2012-13 UNICEF MICS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}

if (country_to_compare == "SWZ") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Swaziland"
  regions = "sssa"
  country_prefix <- "SWZ"
  ctry_nid <- 200707
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}



if (country_to_compare == "KGZ") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Kyrgyzstan"
  regions = "caeu"
  country_prefix <- "KGZ"
  ctry_nid <- 162283
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}


if (country_to_compare == "MNG") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Mongolia"
  regions = "eaas"
  country_prefix <- "MG"
  ctry_nid <- 335994
  compare_svy_name <- "2016 UNICEF MICS"
  compare_svy_year <- 2016
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}


if (country_to_compare == "TKM") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Turkmenistan"
  regions = "caeu"
  country_prefix <- "TKM"
  ctry_nid <- 264583
  compare_svy_name <- "2015-16 UNICEF MICS"
  compare_svy_year <- 2015
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}


if (country_to_compare == "UKR") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Ukraine"
  regions = "caeu"
  country_prefix <- "UKR"
  ctry_nid <- 132739
  compare_svy_name <- "2012 UNICEF MICS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}

if (country_to_compare == "GUY") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Guyana"
  regions = "trsa"
  country_prefix <- "GUY"
  ctry_nid <- 200598
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}

if (country_to_compare == "UZB") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Uzbekistan"
  regions = "caeu"
  country_prefix <- "UZB"
  ctry_nid <- 13445
  compare_svy_name <- "2006 UNICEF MICS"
  compare_svy_year <- 2006
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}



if (country_to_compare == "BLZ") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Belize"
  regions = "ctam"
  country_prefix <- "BLZ"
  ctry_nid <- 264910
  compare_svy_name <- "2015-16 UNICEF MICS"
  compare_svy_year <- 2015
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}


if (country_to_compare == "SUR") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Suriname"
  regions = "trsa"
  country_prefix <- "SUR"
  ctry_nid <- 81203
  compare_svy_name <- "2010 UNICEF MICS"
  compare_svy_year <- 2010
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- country_name
}



if (country_to_compare == "AGO") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Angola"
  regions = "cssa"
  country_prefix <- "AGO"
  ctry_nid <- 218555
  compare_svy_name <- "2015-16 MACRO DHS"
  compare_svy_year <- 2015
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Angola"
}



if (country_to_compare == "ECU") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Ecuador"
  regions = "ansa"
  country_prefix <- "ECU"
  ctry_nid <- 46924
  compare_svy_name <- "2005 MACRO DHS"
  compare_svy_year <- 2005
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Ecuador"
}


if (country_to_compare == "CRI") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Costa Rica"
  regions = "ctam"
  country_prefix <- "CRI"
  ctry_nid <- 125596
  compare_svy_name <- "2011 UNICEF MICS"
  compare_svy_year <- 2011
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Costa Rica"
}

if (country_to_compare == "CUB") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Cuba"
  regions = "crbn"
  country_prefix <- "CUB"
  ctry_nid <- 169975
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Cuba"
}


if (country_to_compare == "YEM") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Yemen"
  regions = "name"
  country_prefix <- "YEM"
  ctry_nid <- 112500
  compare_svy_name <- "2013 MACRO DHS"
  compare_svy_year <- 2013
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Yemen"
}



if (country_to_compare == "BGD") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Bangladesh"
  regions = "soas"
  country_prefix <- "BGD"
  ctry_nid <- 283269
  compare_svy_name <- "2015 BGD INTEGREATED HOUSEHOLD SURVEY"
  compare_svy_year <- 2015
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Bangladesh"
}


if (country_to_compare == "TZA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Tanzania"
  regions = "essa"
  country_prefix <- "TZA"
  ctry_nid <- 218593
  compare_svy_name <- "2014 MACRO DHS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Tanzania"
}


if (country_to_compare == "IDN") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Indonesia"
  regions = "seas"
  country_prefix <- "IDN"
  ctry_nid <- 264956
  compare_svy_name <- "2014 RAND FLS IDN"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Indonesia"
}





if (country_to_compare == "RWA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Rwanda"
  regions = "essa"
  country_prefix <- "RWA"
  ctry_nid <- 157063
  compare_svy_name <- "2014 MACRO DHS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Rwanda"
}




if (country_to_compare == "SLE") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Sierra Leone"
  regions = "wssa"
  country_prefix <- "SLE"
  ctry_nid <- 218619
  compare_svy_name <- "2017 UNICEF MCIS"
  compare_svy_year <- 2017
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Sierra Leone"
}




if (country_to_compare == "STP") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Sao Tome and Principe"
  regions = "wssa"
  country_prefix <- "STP"
  ctry_nid <- 214640
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "São Tomé and Príncipe"
}




if (country_to_compare == "PAN") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Panama"
  regions = "ctam"
  country_prefix <- "PAN"
  ctry_nid <- 161587
  compare_svy_name <- "2013 UNICEF MICS"
  compare_svy_year <- 2013
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Panama"
}

if (country_to_compare == "HND") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Honduras"
  regions = "ctam"
  country_prefix <- "HND"
  ctry_nid <- 95440
  compare_svy_name <- "2011-12 MACRO DHS"
  compare_svy_year <- 2011
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Honduras"
}



if (country_to_compare == "ARM") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Armenia"
  regions = "caeu"
  country_prefix <- "ARM"
  ctry_nid <- 31750
  compare_svy_name <- "2010 MACRO DHS"
  compare_svy_year <- 2010
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Armenia"
}


if (country_to_compare == "COD") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/COD_health2_repaired.shp'
  rasterize_field <- 'GAUL_CODE'
  country_name <- "Democratic Republic of the Congo"
  country_prefix <- "COD"
  regions = "cssa"
  ctry_nid <- 76878
  compare_svy_name <- "2013-14 MACRO DHS"
  compare_svy_year <- 2013
  loc_code_field <- "GAUL_CODE"
  loc_title_field <- "Name_API"
  crop_shapefile <- FALSE
  crop_shapefile_field <- NULL
  crop_shapefile_value <- NULL
}

if (country_to_compare == "NGA") {
  shapefile_path <- '/snfs1/DATA/Incoming Data/GEOGRAPHIC_POPULATION_DEMOGRAPHIC_DATA_GEOPODE/NGA_shapefiles/NGA_shapefile_adm2/Boundary_VaccLGAs_Export.shp'
  rasterize_field <- 'LGACode'
  country_name <- "Nigeria"
  country_prefix <- "NGA"
  regions = "wssa"
  ctry_nid <- 218613
  compare_svy_name <- "2017 UNICEF MICS"
  compare_svy_year <- 2017
  loc_code_field <- "LGACode"
  loc_title_field <- "LGAName"
  crop_shapefile <- FALSE
  crop_shapefile_field <- NULL
  crop_shapefile_value <- NULL
}

if (country_to_compare == "UGA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Uganda"
  regions = "essa"
  country_prefix <- "UGA"
  ctry_nid <- 286780
  compare_svy_name <- "2017 MACRO DHS"
  compare_svy_year <- 2017
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Uganda"
}


if (country_to_compare == "THA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Thailand"
  regions = "seas"
  country_prefix <- "THA"
  ctry_nid <- 331377
  compare_svy_name <- "2015-16 UNICEF MICS"
  compare_svy_year <- 2016
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Thailand"
}



if (country_to_compare == "TLS") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Timor-Leste"
  regions = "seas"
  country_prefix <- "TLS"
  ctry_nid <- 286785
  compare_svy_name <- "2016 MACRO DHS"
  compare_svy_year <- 2016
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Timor-Leste"
}



if (country_to_compare == "CMR") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/06_original shapefiles/PEPFAR/Cameroon_HealthDistricts12012016/Cameroon_HealthDistricts12012016.shp'
  rasterize_field <- 'organisa_2'
  country_name <- "Cameroon"
  regions = "wssa"
  country_prefix <- "CMR"
  ctry_nid <- 19274
  compare_svy_name <- "2011 MACRO DHS"
  compare_svy_year <- 2011
  loc_code_field <- "organisa_2"
  loc_title_field <- "name_1"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "level3na_1"
  crop_shapefile_value <- "Cameroon"
}



if (country_to_compare == "KEN") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Kenya"
  regions = "essa"
  country_prefix <- "KEN"
  ctry_nid <- 157057
  compare_svy_name <- "2014 MACRO DHS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Kenya"
}


if (country_to_compare == "LBR") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Liberia"
  regions = "wssa"
  country_prefix <- "LBR"
  ctry_nid <- 286768
  compare_svy_name <- "2016 MACRO MIS"
  compare_svy_year <- 2016
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Liberia"
}



if (country_to_compare == "GIN") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Guinea"
  regions = "wssa"
  country_prefix <- "GIN"
  ctry_nid <- 69761
  compare_svy_name <- "2012 MACRO DHS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Guinea"
}


if (country_to_compare == "NIC") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Nicaragua"
  regions = "ctam"
  country_prefix <- "NIC"
  ctry_nid <- 9270
  compare_svy_name <- "2006 CDC RHS"
  compare_svy_year <- 2006
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Nicaragua"
}





if (country_to_compare == "MDV") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Maldives"
  regions = "soas"
  country_prefix <- "MDV"
  ctry_nid <- 21311
  compare_svy_name <- "2009 MACRO DHS"
  compare_svy_year <- 2009
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Maldives"
}




if (country_to_compare == "ZWE") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Zimbabwe"
  regions = "sssa"
  country_prefix <- "ZWE"
  ctry_nid <- 157066
  compare_svy_name <- "2015 MACRO DHS"
  compare_svy_year <- 2015
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Zimbabwe"
}


if (country_to_compare == "JAM") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Jamaica"
  regions = "crbn"
  country_prefix <- "JAM"
  ctry_nid <- 141336
  compare_svy_name <- "2011 UNICEF MICS"
  compare_svy_year <- 2011
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Jamaica"
}

if (country_to_compare == "GTM") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Guatemala"
  regions = "ctam"
  country_prefix <- "GTM"
  ctry_nid <- 4779
  compare_svy_name <- "2008 CDC RHS"
  compare_svy_year <- 2008
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Guatemala"
}



if (country_to_compare == "COL") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Colombia"
  regions = "ctam"
  country_prefix <- "COL"
  ctry_nid <- 69761
  compare_svy_name <- "2012 MACRO DHS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Colombia"
}



if (country_to_compare == "MDG") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Madacascar"
  regions = "essa"
  country_prefix <- "MDG"
  ctry_nid <- 125594
  compare_svy_name <- "2012 UNICEF MICS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Madagascar"
}





if (country_to_compare == "BEN") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Benin"
  regions = "wssa"
  country_prefix <- "BEN"
  ctry_nid <- 206075
  compare_svy_name <- "2014 UNICEF MICS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Benin"
}




if (country_to_compare == "MWI") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Malawi"
  regions = "essa"
  country_prefix <- "MWI"
  ctry_nid <- 218581
  compare_svy_name <- "2015-2016 MACRO DHS"
  compare_svy_year <- 2016
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Malawi"
}



if (country_to_compare == "LSO") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/gadm_36_ad1.shp'
  rasterize_field <- 'ADM1_CODE'
  country_name <- "Lesotho"
  regions = "sssa"
  country_prefix <- "LSO"
  ctry_nid <- 157058
  compare_svy_name <- "2014 MACRO DHS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM1_CODE"
  loc_title_field <- "ADM1_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Lesotho"
}



if (country_to_compare == "ERI") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Eritrea"
  regions = "essa"
  country_prefix <- "ERI"
  ctry_nid <- 19539
  compare_svy_name <- "2002 MACRO DHS"
  compare_svy_year <- 2002
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Eritrea"
}






if (country_to_compare == "GAB") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Gabon"
  regions = "cssa"
  country_prefix <- "GAB"
  ctry_nid <- 19539
  compare_svy_name <- "2012 MACRO DHS"
  compare_svy_year <- 2012
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Gabon"
}




if (country_to_compare == "GHA") {
  shapefile_path <- '/snfs1/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/lf_g2015_2014_2.shp'
  rasterize_field <- 'ADM2_CODE'
  country_name <- "Ghana"
  regions = "wssa"
  country_prefix <- "GHA"
  ctry_nid <- 157027
  compare_svy_name <- "2014 MACRO DHS"
  compare_svy_year <- 2014
  loc_code_field <- "ADM2_CODE"
  loc_title_field <- "ADM2_NAME"
  crop_shapefile <- TRUE
  crop_shapefile_field <- "ADM0_NAME"
  crop_shapefile_value <- "Ghana"
}



# Reading in input data from specific model run, subsetting to specific nid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
input_data$SSW <- ifelse(is.na(input_data$SSW), input_data$N_obs, input_data$SSW)
input_data <- input_data[which(input_data$svy_id==ctry_nid),]
input_data <- input_data[which(input_data$year==(max(unique(input_data$year)))),]

# Loading in shapefile and making appropriate modifications 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
admin_shp <- sf::st_read(shapefile_path)
gaul_codes <- get_adm0_codes(regions, shapefile_version = shapefile_version) 

gaul_to_loc_id <- 
  get_location_code_mapping(shapefile_version = shapefile_version) %>% 
  dplyr::select(GAUL_CODE, loc_name, ihme_lc_id) %>% 
  dplyr::rename(location_name = loc_name) %>%
  filter(GAUL_CODE %in% gaul_codes)

admin_shp_data <- as.data.table(admin_shp)
admin_shp_data <- unique(subset(admin_shp_data, select = c(loc_code_field, loc_title_field)))
setnames(admin_shp_data, c(loc_code_field, loc_title_field), c("std_loc_code", "std_loc_title"))
gaul_to_loc_id <- admin_shp_data

input_data <-      input_data %>%
  rowwise() %>% 
  ungroup() %>% data.table() %>%
  setnames(c(svy_id, sample_column), c("svy_id", "sample_column"))

countries <- input_data$country %>% unique()

# Assign input data to correct admin0/admin1/dmin2 in one step
input_admin <-
  input_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(admin_shp)) %>%
  st_join(admin_shp) %>%
  st_set_geometry(NULL) %>%
  setnames(eval(indicator), "prev") %>%
  as.data.table

# Standardize names
setnames(input_admin, c(loc_code_field, loc_title_field), c("std_loc_code", "std_loc_title"))

# Ensure correct formats
input_admin[, std_loc_code := as.numeric(as.character(std_loc_code))]

missing <-
  input_admin %>%
  filter(is.na(std_loc_code)) %>% nrow()

input_admin <- as.data.table(input_admin)

# If binomial make sure it is prevalence space
if (indicator_family == "binomial") input_admin[, prev := prev / N]


# Collapse to admin 0 level
input_admin0 <-
  input_admin %>%
  group_by(svy_id, source, point, std_loc_code, std_loc_title) %>%
  dplyr::summarise(
    year = floor(median(year, na.rm = T)),
    outcome = weighted.mean(prev, sample_column),
    N = sum(N * weight)
  ) %>%
  ungroup() %>%
  data.table()

# Change source/polygon to factor and shorten survey names to fit on plot legend
input_clean <- function(df) {
  df %>%
    rowwise() %>% 
    mutate(source = ifelse(source == "MACRO_DHS", "DHS", source)) %>%
    mutate(source = ifelse(source == "MACRO_AIS", "AIS", source)) %>%
    mutate(source = ifelse(source == "UNICEF_MICS", "MICS", source)) %>%
    mutate(source = ifelse(source == "COUNTRY_SPECIFIC", "CS", source)) %>%
    mutate(source = ifelse(source == "WB_CWIQ", "CWIQ", source)) %>%
    mutate(source = ifelse(source == "WB_CWIQ", "CWIQ", source)) %>%
    mutate(source = ifelse(source == "WB_LSMS", "LSMS", source)) %>%
    mutate(source = ifelse(source == "WB_LSMS_ISA", "ISA", source)) %>%
    mutate(source = ifelse(source == "WB_PRIORITY_SURVEY", "PRI_S", source)) %>%
    mutate(source = ifelse(source == "ARAB_LEAGUE_PAPFAM", "PAPFAM", source)) %>%
    mutate(source = ifelse(source == "JHSPH_PERFORMANCE_MONITORING_ACCOUNTABILITY_SURVEY_PMA2020", "PMA", source)) %>% 
    mutate(source = ifelse(nchar(source) > 6, str_trunc(source, 6, ellipsis = ""), source)) %>% #truncate source if it is too long and not specified above
    ungroup() %>% 
    mutate(source = as.factor(source)) %>%
    mutate(point = as.factor(point)) %>%
    data.table()
}

input_admin0 <- input_clean(input_admin0)

subnational_nid_subset <- function(df) {
  df %>% 
    mutate(point = as.numeric(levels(point))[point]) %>% 
    rowwise() %>% 
    mutate(point = ifelse(svy_id %in% subnational_nids, point + 2, point)) %>% 
    ungroup() %>% 
    mutate(point = as.factor(point)) %>% 
    data.table()
}

if (!is.null(subnational_nids)){
  input_admin0 <- subnational_nid_subset(input_admin0)
}


ago_admin <- input_admin0

# Reading in shapefile, rasters, and population weights
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in shapefile
ashp <- as(sf::st_read(shapefile_path), 'Spatial') #This trick uses the faster sf package drivers

# Crop shapefile if needed
if (crop_shapefile) {
  ashp <- subset(ashp, get(crop_shapefile_field) == crop_shapefile_value)
}

if(country_to_compare=="NGA"){
  ashp$std_loc_code <- as.numeric(as.character(ashp$std_loc_code))
  
}

# Standardize names
names(ashp)[names(ashp) == loc_code_field]  <- "std_loc_code"
names(ashp)[names(ashp) == loc_title_field] <- "std_loc_title"

# Create an sf version
# TODO: convert this to use either all 'spatial' objects or all 'sf' (way faster)
asf <- st_as_sf(ashp)

# Read in mean coverage raster and pop raster
mean_ras <- brick(paste0('/share/geospatial/mbg/vaccine/', indicator, '/output/', run_date,'/', indicator, '_vax_',regions,'_raked_mean_raster.tif'))
pop <- raster('/home/j/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/a0004t/1y/worldpop_a0004t_1y_2017_00_00.tif')

# Making sure adm2 codes are numeric and make a admin raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
asf$std_loc_code <- as.numeric(asf$std_loc_code)
a2_raster <- fasterize(asf, mean_ras[[1]], field = "std_loc_code")

# Cropping population to the spatial extent of the raster you are aggregating
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop2 <- crop(pop, mean_ras[[1]])
pop2 <- mask(pop2, mean_ras[[1]])

# pixel_spatial is a data.table where pixel_id is the cell numbers where its not-NA in the admin raster
a <- as.numeric(t(as.matrix(a2_raster)))
pixel_id <- which(!is.na(a))


weighted_mean <- function(x, w, ..., na.rm = TRUE){
  if(na.rm){
    df_omit <- na.omit(data.frame(x, w))
    return(weighted.mean(df_omit$x, df_omit$w, ...))
  } 
  weighted.mean(x, w, ...)
}

# Function to aggregate a given year
custom_agg <- function(mean_raster, 
                       pixel_id_table,
                       pop_raster,
                       admin2_raster,
                       agg_year, 
                       year_list,
                       the_shp) {
  
  # Grab index from the year
  year_idx <- which(year_list == agg_year)
  
  # Create a sf version of the shapefile
  asf <- st_as_sf(the_shp)
  
  # Extract values from raster  
  values <- data.table(mean = extract(mean_raster[[year_idx]], pixel_id_table), pop = extract(pop_raster, pixel_id_table))
  values[,pixel_id:= pixel_id_table]
  values[,a2:=extract(admin2_raster, pixel_id_table)]
  
  # Do aggregations
  agg <- values[,.(mean=weighted.mean(x = mean, w = pop, na.rm = TRUE), pop = sum(pop, na.rm = TRUE)), by = a2]
  agg <- agg[,std_loc_code:=a2][,.(mean, pop, std_loc_code)]
  
  ashp_dt <- as.data.table(ashp@data)
  ashp_dt[,std_loc_code:=as.numeric(std_loc_code)]
  agg <- merge(agg, ashp_dt[,.(std_loc_code, std_loc_title)], by = 'std_loc_code', all.x = TRUE)
  agg <- agg[,.(mean, pop, std_loc_code, std_loc_title)]
  agg[,year := agg_year]
  
  # Find missing admins
  # (miss_a2 is the admin2s that are not in the admin raster so we take their centroids since they are too small to be rasterized)
  miss_a2 <- setdiff(unique(as.numeric(the_shp$std_loc_code)), unique(admin2_raster))
  if (length(miss_a2 != 0)) {
    centroids <- gCentroid(the_shp[which(the_shp$std_loc_code %in% miss_a2),], byid = TRUE)
    miss_a2_val <- data.table(mean = extract(mean_raster[[year_idx]], centroids),pop = extract(pop_raster, centroids), year = agg_year, 
                              the_shp[which(the_shp$std_loc_code %in% miss_a2),]@data[,c('std_loc_code', 'std_loc_title')])
    
    agg <- rbind(agg, miss_a2_val)
  }
  
  # Prepare and format results
  results <- agg
  results$std_loc_code <- as.numeric(results$std_loc_code)
  asf$std_loc_code <- as.numeric(asf$std_loc_code)
  
  asf2 <- left_join(asf, results, by = c('std_loc_code', 'std_loc_title')) 
  
  simple_df <- data.frame(as.numeric(asf2$std_loc_code), asf2$std_loc_title, as.numeric(asf2$year), as.numeric(asf2$mean))
  colnames(simple_df) <- c("std_loc_code", "std_loc_title", "year", "mean")
  
  return(as.data.table(simple_df))
  
}

agg_by_year <- lapply(c(compare_svy_year, most_recent_year), 
                      function(yr) {
                        custom_agg(mean_raster = mean_ras, 
                                   pixel_id_table = pixel_id,
                                   pop_raster = pop2,
                                   admin2_raster = a2_raster,
                                   agg_year = yr,
                                   year_list = year_list,
                                   the_shp = ashp)
                      })

agg_by_year <- rbindlist(agg_by_year) %>% unique # this unique call takes care of the situation where the last year = svy year

# Get the year data and convert to spatial format - aggregates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
most_recent_agg_pred <- subset(agg_by_year, year == most_recent_year, select = c("std_loc_code", "mean"))
most_recent_total_df <- merge(ago_admin, most_recent_agg_pred, by="std_loc_code", all.y=T)

compare_svy_agg_pred <- subset(agg_by_year, year == compare_svy_year, select = c("std_loc_code", "mean"))
compare_svy_total_df <- merge(ago_admin, compare_svy_agg_pred, by="std_loc_code", all.y=T)

# Merging on the data from the JRF/WHO reports
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jrf <- fread('/snfs1/WORK/01_covariates/02_inputs/vaccines/geospatial/JRF matching/JRF2017_matching_081319.csv')
jrf <- jrf[which(jrf$ADM0_NAME==country_name),]
jrf <- jrf[which(jrf$vaccine==vacc_name),]
jrf$std_loc_code <- jrf$location_code

jrf <- subset(jrf, select = c("std_loc_code", "who_coverage"))
jrf[, std_loc_code := as.numeric(as.character(std_loc_code))]
jrf[, who_coverage := who_coverage / 100]

most_recent_df <- join(most_recent_total_df, jrf, by = "std_loc_code", type = "left", match = "first")
compare_svy_df <- join(compare_svy_total_df, jrf, by = "std_loc_code", type = "left", match = "first")

setnames(most_recent_df, c("outcome", "mean", "who_coverage"), c("survey", "ihme", "who"))
setnames(compare_svy_df, c("outcome", "mean", "who_coverage"), c("survey", "ihme", "who"))

most_recent_df[, ihme_year := most_recent_year]
compare_svy_df[, ihme_year := compare_svy_year]

# Prepare spdfs for plotting

# Ensure loc code ready for merging
ashp$std_loc_code <- as.numeric(as.character(ashp$std_loc_code))

most_recent_spdf <- merge(ashp, most_recent_df, by = "std_loc_code")
most_recent_spdf@data$id <- rownames(most_recent_spdf@data)
most_recent_spdf.points <- fortify(most_recent_spdf, region = "id")
most_recent_spdf.df <- join(most_recent_spdf.points, most_recent_spdf@data, by = "id") %>% as.data.table

compare_svy_spdf <- merge(ashp, compare_svy_df, by = "std_loc_code")
compare_svy_spdf@data$id <- rownames(compare_svy_spdf@data)
compare_svy_spdf.points <- fortify(compare_svy_spdf, region = "id")
compare_svy_spdf.df <- join(compare_svy_spdf.points, compare_svy_spdf@data, by = "id") %>% as.data.table

# Setting custom vaccine color scale 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vals <- c( 1,       0.8,        0.6,      0.496094,   0.4,       0.2,       0.164063,  0.000000)
cols <- c("#3D649D", "#91BEDC",  "#DEF3F8",  "#FEE191", "#FC8D58",  "#B03027", "#8F48AF","#44125B")

geom_polygon_quiet <- function(...) {suppressMessages(ggplot2::geom_polygon(...))}
geom_path_quiet     <- function(...) {suppressMessages(ggplot2::geom_path(...))}


# Plotting and mapping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map of most recent JRF estimates -----------------

gg_jrf <- ggplot() +
  geom_polygon_quiet(data = most_recent_spdf.df[who <= 1 | is.na(who)],
                     aes(x=long, y=lat, group=group, fill=who),
                     size = 0.2) +
  geom_polygon_quiet(data = most_recent_spdf.df[who > 1],
                     aes(x=long, y=lat, group=group),
                     fill = "#8436A8",
                     size = 0.2) +
  geom_path_quiet(data = most_recent_spdf.df,
                  aes(x=long, y=lat, group=group),
                  size = 0.2,
                  color = "black") +
  scale_fill_gradientn(colors = cols, values = vals, 
                       breaks = c(0,0.2,0.4,0.6,0.8,0.9,1),
                       limits = c(0,1),
                       labels = scales::percent) +
  theme_empty() +
  coord_equal() +
  labs(title = paste0(most_recent_year, " JRF-reported estimates"), fill = paste0(ind_title, " Coverage"))

# Map of survey data -----------------------------
# (youngest age cohort only) as points at centroids

# Get centroids for admins and merge to data
centroids <- gCentroid(most_recent_spdf, byid = T, id = most_recent_spdf$std_loc_code) %>%
  as.data.frame(centroids) %>%
  cbind(rownames(.), .) %>%
  as.data.table(.) %>%
  setnames(., names(.), c("std_loc_code", "long", "lat"))

centroids$std_loc_code <- as.numeric(levels(centroids$std_loc_code))[centroids$std_loc_code]

centroid_df <- merge(subset(as.data.table(most_recent_spdf), select = c("std_loc_code", "survey", "N")),
                     centroids, 
                     by = "std_loc_code")

gg_survey_point <- ggplot() +
  geom_path_quiet(data = compare_svy_spdf.df,
                  aes(x=long, y=lat, group=group),
                  size = 0.2,
                  color = "black") +
  geom_point(data = centroid_df, 
             aes(x = long, y = lat, size = N, fill = survey),
             alpha = 0.9, 
             shape = 21) +
  scale_fill_gradientn(colors = cols, values = vals, 
                       breaks = c(0,0.2,0.4,0.6,0.8,0.9,1),
                       limits = c(0,1),
                       labels = scales::percent) +
  theme_empty() +
  coord_equal() +
  labs(title = compare_svy_name, fill = paste0(ind_title, " Coverage"), size = "N (survey)")

# Map of survey data --------------------------
# (choropleth map)

gg_survey <- ggplot() +
  geom_polygon_quiet(data = compare_svy_spdf.df,
                     aes(x=long, y=lat, group=group, fill=survey),
                     size = 0.2) +
  geom_path_quiet(data = compare_svy_spdf.df,
                  aes(x=long, y=lat, group=group),
                  size = 0.2,
                  color = "black") +
  scale_fill_gradientn(colors = cols, values = vals, 
                       breaks = c(0,0.2,0.4,0.6,0.8,0.9,1),
                       limits = c(0,1),
                       labels = scales::percent) +
  theme_empty() +
  coord_equal() +
  labs(title = compare_svy_name, fill = paste0(ind_title, " Coverage"))

# Map of IHME aggregates (most recent year)
gg_agg_most_recent <- ggplot() +
  geom_polygon_quiet(data = most_recent_spdf.df,
                     aes(x=long, y=lat, group=group, fill=as.numeric(as.character(ihme))),
                     size = 0.2) +
  geom_path_quiet(data = most_recent_spdf.df,
                  aes(x=long, y=lat, group=group),
                  size = 0.2,
                  color = "black") +
  scale_fill_gradientn(colors = cols, values = vals, 
                       breaks = c(0,0.2,0.4,0.6,0.8,0.9,1),
                       limits = c(0,1),
                       labels = scales::percent) +
  theme_empty() +
  coord_equal() +
  labs(title = paste0(most_recent_year, " IHME aggregated estimates"), 
       fill = paste0(ind_title, " Coverage"))

# Map of IHME aggregates (survey comparison year)
gg_agg_compare_svy <- ggplot() +
  geom_polygon_quiet(data = compare_svy_spdf.df,
                     aes(x=long, y=lat, group=group, fill=as.numeric(as.character(ihme))),
                     size = 0.2) +
  geom_path_quiet(data = compare_svy_spdf.df,
                  aes(x=long, y=lat, group=group),
                  size = 0.2,
                  color = "black") +
  scale_fill_gradientn(colors = cols, values = vals, 
                       breaks = c(0,0.2,0.4,0.6,0.8,0.9,1),
                       limits = c(0,1),
                       labels = scales::percent) +
  theme_empty() +
  coord_equal() +
  labs(title = paste0(compare_svy_year, " IHME aggregated estimates"), 
       fill = paste0(ind_title, " Coverage"))

# Draw maps

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

the_legend <- g_legend(gg_survey_point)
title_grob <- textGrob("MCV1 Coverage: 2017", gp = gpar(fontsize = 24, fontface = "bold"))

gg_agg_most_recent <- gg_agg_most_recent + theme(legend.position = "none")
gg_agg_compare_svy <- gg_agg_compare_svy + theme(legend.position = "none")
gg_jrf <- gg_jrf + theme(legend.position = "none")
gg_survey <- gg_survey + theme(legend.position = "none")
gg_survey_point <- gg_survey_point + theme(legend.position = "none")

if (most_recent_year != compare_svy_year) {
  
  lay <- rbind(c(1,1,1,2,2,2,5),
               c(1,1,1,2,2,2,5),
               c(1,1,1,2,2,2,5),
               c(3,3,3,4,4,4,5),
               c(3,3,3,4,4,4,5),
               c(3,3,3,4,4,4,5))
  
  plot_all <- arrangeGrob(gg_survey_point, gg_jrf, gg_agg_compare_svy, gg_agg_most_recent, the_legend,
                          layout_matrix = lay,
                          heights = c(1,1,1,1,1,1))
  
  png(file = paste0(out_dir, paste0(country_prefix, "_maps.png")),
      width = 10,
      height = 9,
      units = "in", 
      res = 400, 
      type = "cairo")
  grid.draw(plot_all)
  dev.off()
  
} else {
  
  lay <- rbind(c(1,1,1,2,2,2,3,3,3,4),
               c(1,1,1,2,2,2,3,3,3,4),
               c(1,1,1,2,2,2,3,3,3,4))
  
  plot_all <- arrangeGrob(gg_jrf, gg_survey_point, gg_agg_most_recent, the_legend,
                          layout_matrix = lay,
                          heights = c(1,1,1))
  
  png(file = paste0(out_dir, paste0(country_prefix, "_maps.png")),
      width = 14,
      height = 8,
      units = "in", 
      res = 400, 
      type = "cairo")
  grid.draw(plot_all)
  dev.off()
}


# Function to make scatter plots
make_scatter <- function(df, 
                         x_col,
                         y_col,
                         x_col_title,
                         y_col_title,
                         ind_t = ind_title,
                         scale_by_size = FALSE,
                         max_x = 1,
                         max_y = 1) {
  
  if (scale_by_size == TRUE) {
    
    gg_scatter <-  ggplot() +
      geom_abline(slope = 0, intercept = 1, color = "darkred", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, color = "darkgrey") +
      geom_point(data = df,
                 aes(x = get(x_col), 
                     y = get(y_col), 
                     size = N), 
                 alpha = 0.2,  color="dodgerblue4") +   
      theme_classic() + 
      coord_equal() +
      scale_size_area() +
      scale_x_continuous(labels = scales::percent, limits = c(0,max_x), expand = expand_scale(mult = c(0, .1))) +
      scale_y_continuous(labels = scales::percent, limits = c(0,max_y), expand = expand_scale(mult = c(0, .1))) +
      labs(x = x_col_title, 
           y = y_col_title,
           size = "N (survey)",
           title = paste0(ind_t, " Coverage"))
    
  } else {
    
    gg_scatter <-  ggplot() +
      geom_abline(slope = 0, intercept = 1, color = "darkred", linetype = "dashed") +
      geom_abline(slope = 1, intercept = 0, color = "darkgrey") +
      geom_point(data = df,
                 aes(x = get(x_col), 
                     y = get(y_col)), 
                 alpha = 0.5,  color="dodgerblue4",
                 size = 2.5) +   
      theme_classic() + 
      coord_equal() +
      scale_x_continuous(labels = scales::percent, limits = c(0,max_x), expand = expand_scale(mult = c(0, .1))) +
      scale_y_continuous(labels = scales::percent, limits = c(0,max_y), expand = expand_scale(mult = c(0, .1))) +
      labs(x = x_col_title, 
           y = y_col_title,
           title = paste0(ind_t, " Coverage"))
    
  }
  
  return(gg_scatter)
  
}

# Find maximum y value
maximum_y = max(most_recent_df$who, na.rm=T)

png(file = paste0(out_dir, paste0(country_prefix, "_scatter_jrf_vs_svy.png")),
    width = 5,
    height = 8,
    units = "in", 
    res = 400, 
    type = "cairo")

make_scatter(df = most_recent_df,
             y_col = "who",
             y_col_title = paste0(most_recent_year, " JRF (country-reported)"),
             x_col = "survey",
             x_col_title = compare_svy_name, 
             scale_by_size = TRUE,
             max_y = maximum_y)

dev.off()

png(file = paste0(out_dir, paste0(country_prefix, "_scatter_ihme_recent_vs_svy.png")),
    width = 5,
    height = 8,
    units = "in", 
    res = 400, 
    type = "cairo")

make_scatter(df = most_recent_df,
             y_col = "ihme",
             y_col_title = paste0(most_recent_year, " IHME (aggregated estimates)"),
             x_col = "survey",
             x_col_title = compare_svy_name, 
             scale_by_size = TRUE,
             max_y = maximum_y)

dev.off()

png(file = paste0(out_dir, paste0(country_prefix, "_scatter_ihme_correct_year_vs_svy.png")),
    width = 5,
    height = 8,
    units = "in", 
    res = 400, 
    type = "cairo")

make_scatter(df = compare_svy_df,
             y_col = "ihme",
             y_col_title = paste0(compare_svy_year, " IHME (aggregated estimates)"),
             x_col = "survey",
             x_col_title = compare_svy_name, 
             scale_by_size = TRUE,
             max_y = maximum_y)

dev.off()

png(file = paste0(out_dir, paste0(country_prefix, "_scatter_ihme_recent_vs_jrf.png")),
    width = 5,
    height = 8,
    units = "in", 
    res = 400, 
    type = "cairo")


make_scatter(df = most_recent_df,
             y_col = "who",
             y_col_title = paste0(most_recent_year, " JRF (country-reported)"),
             x_col = "ihme",
             x_col_title = paste0(most_recent_year, " IHME (aggregated estimates)"),
             scale_by_size = FALSE,
             max_y = maximum_y)

dev.off()

