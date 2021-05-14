library(data.table)
library(fuzzyjoin)
assessment <- c("good","fair","bad","fair")
name <- c("John","Jack","Ricky","Henry")

age <- c(13,14,16,15)
length(assessment)<- 4
df1 <- data.table(name,age,assessment)

name <- c("John","Jackk","Tracy")
age2 <- c(13,12,11)
height <- c("tall","average","short")
df2 <- data.table(name,age2,height)
#setnames(df2,c("age2","height"),c("age","height"))

#df_combined <- rbind(df1,df2)
df_combined <- merge(df1,df2, by.x = c("name","age"), by.y=c("name","age2"),all=TRUE)

df_combined[!is.na(assessment)&!is.na(height),merge_status:="success"]
df_combined[is.na(assessment)&!is.na(height),merge_status:="in df2 not df1"]
df_combined[!is.na(assessment)&is.na(height),merge_status:="in df1 not df2"]

df1_fuzz <- subset(df_combined, merge_status=="in df1 not df2")
df2_fuzz <- subset(df_combined, merge_status=="in df2 not df1")
df_fuzzy <- stringdist_inner_join(df1_fuzz,df2_fuzz, by="name", method="soundex")
df_fuzzy <- subset(df_fuzzy,select=c(1,2,3,7,9))
setnames(df_fuzzy,c("name.x","height.y","assessment.x"),c("name","height","assessment"))
df_fuzzy[,age:=(age.x+age.y)/2]
df_complete<-rbind(df_combined,df_fuzzy,fill=TRUE)
df_complete[,c("age.x","age.y"):=NULL]

                     
df_complete[!is.na(assessment)&!is.na(height),merge_status:="success"]
df_complete[is.na(assessment)&!is.na(height),merge_status:="in df2 not df1"]
df_complete[!is.na(assessment)&is.na(height),merge_status:="in df1 not df2"]

df_success <- subset(df_complete,merge_status=="success")

df_complete[is.na(df_complete$assessment)]
df_complete <- cbind(df_complete,random <- rnorm(7))
for (row in 1:3){
  print(df_complete[row,])
}
# setkey(df_complete,name)
# subcolumn <- df_complete[.("Henry")]
# df_complete[is.na(assessment), assessment:="good"]
# df_complete[,"age"] <- c(1,2,3,4,5,6,7)
# 
# aggregate(df_complete$age, by=df_complete[,"assessment"],FUN=sum)
# df_complete[,"age"]
# total <- c()


#print(max(df_complete$age))
#age_limit <- df_complete[,2]
#sum(age_limit)
#age_limit2 <- df_complete[df_complete$age>13,]




