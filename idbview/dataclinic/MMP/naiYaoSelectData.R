
mydata <- readRDS("~/giteeShiny/MMP/naiYaoSelectData.RDS")
mydata$Number_of_Hospitalization <- as.integer(mydata$Number_of_Hospitalization)

plot(mydata$Number_of_Hospitalization, mydata$Days_of_Hospitalization)
cor(mydata$Number_of_Hospitalization, mydata$Days_of_Hospitalization)

hist(mydata$Days_of_Hospitalization )
hist(mydata$Age_Days/30 )
# hist(mydata$Age )

cor(mydata$Number_of_Hospitalization, mydata$Days_of_Hospitalization)

mydata$Months <- format( as.Date(mydata$Admission_Date, format=" %d/%m/%Y "),"%m ") %>% as.integer()

table(mydata$Genetic_Resistance_Mutation,mydata$Months)
c(4, 7, 9, 11, 6, 8, 10, 8, 16, 18, 10, 8) +
c(25, 41, 11, 11, 4, 7, 34, 50, 18, 14, 8, 10)
c(29, 48, 20, 22, 10, 15, 44, 58, 34, 32, 18, 18)

mydata$Body_Weight <- gsub('Kg','',mydata$Admission_Weight)
mydata$Body_Weight <- gsub('kg','',mydata$Body_Weight)
mydata$Body_Weight <- as.numeric(gsub(' ','',mydata$Body_Weight))

mydata$Body_Weight <- as.numeric(stringr::str_split_fixed(mydata$Admission_Weight,'',2)[,1])
plot(mydata$Body_Weight, mydata$Age_Days)

paste("[",round(mydata$Age_Days/30,1),sep = '',',',mydata$Body_Weight,']',',')

# 输出体重与年龄数据
write.csv( paste("[",round(mydata$Age_Days/30,1),sep = '',',',mydata$Body_Weight,']',',')
  ,file = 'd.csv',row.names = F,quote = FALSE)

mydata$Mycoplasma <- gsub('×','*',mydata$Mycoplasma)
mydataMMP <- mydata[,-c(23,32,33)]

colnames(mydataMMP)
mydataMMP <- mydataMMP[,c(1,12:13,2:11,14:30)]

mydata
saveRDS(mydataMMP,file = 'www/mydataMMP.RDS')



