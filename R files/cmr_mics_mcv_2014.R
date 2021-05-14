rm(list=ls())
library(readstata13)
#TZA_DHS_2015-2016

df_stata <- read.dta13("J:/DATA/UNICEF_MICS/PAK/PUNJAB_2014/PAK_PUNJAB_MICS5_2014_CH5_Y2016M03D29.DTA")

#subset age and dtp dose status in stata data set
df_stata <- subset(df_stata,select=c("cage","IM16"))
#colnames(df_stata) <- c("mcv_dose","age in months")
#subset age of 36-48 months old in stata data set
df_stata <- subset(df_stata, df_stata$cage>36)

# exclude all DPT1 NA value
df_stata <- df_stata[!is.na(df_stata$IM16),]
# exclude all age NA values
df_stata <- df_stata[!is.na(df_stata$hw1),]




df_ubcov<- read.csv("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/vaccination/CMR_UNICEF_MICS_2014_2014_244455.csv", header= T)
df_ubcov <- subset(df_ubcov,select=c(1,2,3,4,5,6,16,24))
#subset age of 36-47 months old in ubcov data set
df_ubcov <- subset(df_ubcov, df_ubcov$age_month>=36 & df_ubcov$age_month<=47)
# exclude all NA values
df_ubcov <- df_ubcov[!is.na(df_ubcov$mcv_dose),]
table(df_ubcov$mcv_dose)

# MAR 2010-2011
dfdta_mar <- read.dta13("J:/DATA/ARAB_LEAGUE_PAPFAM/MAR/2010_2011/MAR_PAPFAM_2010_2011_BH_EN_Y2017M03D24.DTA")
dfub_mar <- read.csv("J:/LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/vaccination/MAR_ARAB_LEAGUE_PAPFAM_2010_2011_126909.csv",header=T)

dfdta_mar <- subset(dfdta_mar, select= c("q6516","agemonth"))
