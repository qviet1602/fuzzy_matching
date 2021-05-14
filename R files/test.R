gadm_adm2 <- fread("C:/Users/nqviet94/Desktop/gadm_36_ad2.csv")
vaccine_countries <- fread("C:/Users/nqviet94/Desktop/VCM_country_list_2018-01-11.csv")
gadm_adm2 <- subset(gadm_adm2, select = c("NAME_0", "NAME_1", "NAME_2", "ISO", "ADM1_CODE", "ADM2_CODE"))
vaccine_ISO <- subset(vaccine_countries, select = "ISO")
vaccine_ISO <- unlist(vaccine_countries)
vaccines_adm <- gadm_adm2[ISO %in% vaccine_ISO]
write.csv(vaccines_adm, "C:/Users/nqviet94/Desktop/figure4table.csv")



lentubcov_files <- list.files("C:/Users/nqviet94/Desktop/raw")
nid <- c()
for (i in 1: length(ubcov_files)){
  nid[i] <- substr(ubcov_files[i], nchar(ubcov_files[i]) - 10 +1, nchar(ubcov_files[i])-4)
}

for (i in 1: length(nid)){
  nid[i] <- gsub("*_", "", nid[i])
}
iso3 <- c()
for (i in 1: length(ubcov_files)){
  iso3[i] <- substr(ubcov_files[i],1,3)
}
list <- data.frame(iso3,nid)
list <- data.table(list)
stage2_ctries <- c("ARM", "AZE", "GEO", "RKS", "MDA", "TUV", "UKR", "MDV", "SYC", "FSM", "MHL", "WSM", "SLB", "VUT")
stage2 <- list[iso3 %in% stage2_ctries,] 



files <- list.files("C:/Users/nqviet94/Desktop/DHS")
write.csv(files,"C:/Users/nqviet94/Desktop/DHS/test1.csv")

# read Cooper project
Cooper <- fread("C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/Cooper Project Export 286698_updated_8.8.2018.csv")
Ubcov <- fread("C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/vaccination_Ubcov.csv") 
Ubcov_excluded <- Ubcov[excluded == 'No Relevant Info']
nid_excluded <- subset(Ubcov_excluded, select = 'nid')[[1]]

Cooper_update <- Cooper[nid %in% nid_excluded, acceptance_status := "Excluded"]


#Cooper <- Cooper[microdata_status == "Have microdata"]
#Cooper <- Cooper[acceptance_status == "To be reviewed"]
for (i in 1:nrow(Cooper)){
  if(Cooper$microdata_status[i] == "Have microdata" && Cooper$acceptance_status[i] == "To be reviewed" && Cooper$nid[i] %in% nid){

    Cooper$add_comment[i] <- "extracted"
  }
  }

write.csv(Cooper_update, "C:/Users/nqviet94/Documents/IHME/Work/Vaccine (local)/Cooper Project Export 286698_updated_8.13.2018.csv")


cleaned_extractions <- list.files("/share/covariates/hsa/archive/gbd2017/extract_recall_v_card/cleaned", full.names=TRUE)
df <- lapply(cleaned_extractions, fread) %>% rbindlist(., fill=TRUE)
a <- c(2,3,4)

df$combine <- c()
df <- fread("C:/Users/nqviet94/Desktop/filtersearch.csv")
k = 1 
for (i in 1: nrow(df)){
  for (j in 1: nrow(df)){
    if (df$measles[i] == df$SIA[j]){
      df$combine[k] =  df$measles[i]
      k = k +1
    }
  }
}
write.csv(df,"C:/Users/nqviet94/Desktop/CDCcombine.csv" )

  
# use intersect() and union() to extract common values from both vectors

df <- fread("C:/Users/nqviet94/Desktop/filtersearch.csv")
measles <- unique(df$measles)
mmr <- unique(df$mmr)
mr <- unique(df$mr)
extracted <- unique(df$`measles and SIA`)
uni1 <- union(measles,mmr)
uni <- union(union(measles,mmr),mr)
new1 <- setdiff(uni1,extracted)
new <- setdiff(uni,extracted)
df$`new(womr)` <- new1
df$`new(withmr)` <- new
write.csv(df,"C:/Users/nqviet94/Desktop/filtersearch.csv" )



lbd <- list.files("C:/Users/nqviet94/Desktop/tabulated/lbd")

write.csv(lbd,"C:/Users/nqviet94/Desktop/lbd.csv" )

df <- fread()

fact <- factor(c("Rep", "Dem"))


df <- fread("C:/Users/nqviet94/Downloads/nba2017.csv", header = TRUE)

Model_1 <- lm(Salary ~ Ht + Exp, data = df)
Model_2 <- lm(log(Salary) ~ Ht + Exp, data = df)

install.packages("epitools")
library(epitools)
data(wcgs)
logit <- glm(chd69~ age0 + ncigs0, data = wcgs, family = "binomial")

lbd <- list.files("C:/Users/nqviet94/Desktop/Surveys_data")
ldf <- lapply(lbd, fread)
ldf <- ldf[1:200]
ldf2 <- rbindlist(ldf, use.names=TRUE, fill=TRUE)
nrow(ldf2[is.na(age_month)])*100/nrow(ldf2)

               