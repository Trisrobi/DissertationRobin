library(fredr)
library(tidyverse)
library(glue)
library(reshape2)
library(plm)
#setwd("C:/Users/robin/OneDrive/Desktop/EC331 code/EC331 submission/end code/Rcode")
data <- read.csv("Additional Data.csv")

data1<-subset(data, select=-c(Series.Code, Country.Name))
for (i in 0:9) {
  col_prefix <- paste0("X200", i, "..YR")
  data1 <- data1 %>%
    rename_with(~sub(paste0("^", col_prefix), "",.), starts_with(col_prefix))
  data1 <- data1 %>%
    rename_with(~sub("^YR", "", .), starts_with("YR"))
}

for (i in 10:14) {
  col_prefix <- paste0("X20", i, "..YR")
  data1 <- data1 %>%
    rename_with(~sub(paste0("^", col_prefix), "", .), starts_with(col_prefix))
  data1 <- data1 %>%
    rename_with(~sub("^YR", "", .), starts_with("YR"))
}
# Remove trailing dots from all column names
colnames(data1) <- sub("\\.+$", "", colnames(data1))
  

# Convert data from wide to long format using gather()
long_addata <- gather(data1, key = "Year", value = "Value", -Country.Code, -Series.Name)
long_addata<-subset(long_addata,Country.Code !="")
wide_addata <- long_addata %>%
  pivot_wider(names_from = Series.Name, values_from = Value)

#choose exports or imports
gini_rows<-giniexports
#gini_rows<-giniimports

gini_rows$EU_2004 <- ifelse(gini_rows$Country %in% c("CZE", "EST", "HUN", "LVA", "LTU", "POL", "SVK", "SVN"), 1, 0)
gini_rows$EU_2014 <- ifelse(gini_rows$Country %in% c("LUX", "SVK", "MLT", "HUN", "CZE", "IRL", "EST", "BEL", "SVN", "BGR", "NLD", "LTU", "AUT", "POL", "ROU", "LVA", "PRT", "DNK", "SWE", "GRC", "FIN", "ESP", "CYP", "DEU", "FRA", "ITA", "GBR"), 1, 0)
#subset to all data that was in the EU in 2014
gini_rows_eu_2014<-subset(gini_rows,EU_2014==1)

wide_addata <- wide_addata %>%
  rename(Country = Country.Code)

result_data<-left_join(gini_rows_eu_2014, wide_addata, by=c("Country", "Year"))
new_column_names<-c("Country", "Year","gini","Eu_2004","Eu_2014","Educbasic","workagepop","R&D","FDIinflows")
colnames(result_data) <- new_column_names

result_data$Year <- as.factor(result_data$Year)
result_data$Eu_2004 <- as.factor(result_data$Eu_2004)
result_data$Eu_2014 <- as.factor(result_data$Eu_2014)
result_data$Country <- as.factor(result_data$Country)
result_data$Educbasic<-as.numeric(result_data$Educbasic)
result_data$FDIinflows<-as.numeric(result_data$FDIinflows)
result_data$workagepop<-as.numeric(result_data$workagepop)


#choose whether exports or imports
#write.csv(result_data,file="dataginifinalimports.csv")
write.csv(result_data,file="dataginifinalexports.csv")
write.csv(gini_rows,file="dataginiworldexports")
#write.csv(gini_rows,file="dataginiworldimports")
