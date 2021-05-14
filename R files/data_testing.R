
library(data.table)
#assign/modify using :=
# Operation with by
# Modify mutilple columns at once with .()

#subset column by name, should include with = FALSE
#subsetting rows: no need comma
#.() is data table short hand for list()
#setkey help merge 2 dt faster
#use copy function to duplicate data table (it does not automatically do that by <-)
#setnames function
#seq_len function
#lag and lead in time series
rm(list=ls())
#NGA_MICS_2016
#cleaning data, keep only necessary column
#for the one extracted all
data <- fread("CIV_UNICEF_MICS_2016_2016_218611.csv",header = T, stringsAsFactors = F)
data_pcv <- subset(data, select= grep("pcv*",names(data)))

missing1 <- data_pcv[is.na(data_pcv$pcv_dose),]
#for the one only extracted recall
data2 <- fread("C:/Users/nqviet94/Desktop/CIV_UNICEF_MICS_2016_2016_218611_no date.csv", header = T, stringsAsFactors = F)
data2_pcv <- subset(data2, select = grep("pcv*", names(data2)))
missing2 <- data2_pcv[is.na(data2_pcv$pcv_dose),]

#  missing 1 and missing 2 has diffrent number of rows => pcv dose depend on both recall and card date indicator
#in pcv1 but not in pcv1 age month
data_pcv1 <- data_pcv[!is.na(data$pcv1) & is.na(data$pcv1_age_month),]

data_pcv1age <- data_pcv[is.na(data$pcv1) & !is.na(data$pcv1_age_month),]








#missing in card but recall
data_dpt1_recall <- data_dpt1[!is.na(data_dpt1$dpt_dose) & is.na(data_dpt1$dpt1_age_month),]
#missing in recall but in card
data_dpt1_card <- data_dpt1[is.na(data_dpt1$dpt_dose) & !is.na(data_dpt1$dpt1_age_month),]
#have both
data_dpt1_both <- data_dpt1[!is.na(data_dpt1$dpt_dose) & !is.na(data_dpt1$dpt1_age_month),]
#missing both
data_dpt1_miss <- data_dpt1[is.na(data_dpt1$dpt_dose) & is.na(data_dpt1$dpt1_age_month),]





#MLI_MICS_2015
data2 <- fread("MLI_UNICEF_MICS_2015_2015_248224.csv",header = T, stringsAsFactors = F)
data2_dpt <- subset(data2, select =grep("dpt*", names(data2)))
#missing in card but recall
data2_dpt1_recall <- data2_dpt[!is.na(data2_dpt$dpt1) & is.na(data2_dpt$dpt1_age_month),]
#missing in recall but in card
data2_dpt1_card <- data2_dpt[is.na(data2_dpt$dpt1) & !is.na(data2_dpt$dpt1_age_month),]
#have both
data2_dpt1_both <- data2_dpt[!is.na(data2_dpt$dpt1) & !is.na(data2_dpt$dpt1_age_month),]
#missing both
data2_dpt1_miss <- data2_dpt[is.na(data2_dpt$dpt1) & is.na(data2_dpt$dpt1_age_month),]


data3 <- fread("TTO_UNICEF_MICS_2011_2011_332558.csv",header = T, stringsAsFactors = F)


data3_ever <- subset(data3, select = grep("*ever*", names(data3)))

data3_dpt <- subset(data3, select =grep("dpt*", names(data3)))
#missing in card but recall
data3_dpt1_recall <- data3_dpt[!is.na(data3_dpt$dpt1) & is.na(data3_dpt$dpt1_age_month),]
#missing in recall but in card
data3_dpt1_card <- data3_dpt[is.na(data3_dpt$dpt1) & !is.na(data3_dpt$dpt1_age_month),]
#have both
data3_dpt1_both <- data3_dpt[!is.na(data3_dpt$dpt1) & !is.na(data3_dpt$dpt1_age_month),]
#missing both
data3_dpt1_miss <- data3_dpt[is.na(data3_dpt$dpt1) & is.na(data3_dpt$dpt1_age_month),]

