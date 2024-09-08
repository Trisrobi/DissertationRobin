setwd("C:/Users/robin/OneDrive/Desktop/EC331 code/EC331 submission/end code")
#setting up the environment
library(igraph)
library(tidyverse)
library(glue)
library(reshape2)
library(haven)
load("C:/Users/robin/OneDrive/Desktop/EC331 code/EC331 submission/Rdata/WIOT2014_October16_ROW.RData")

remove_columns_gt_56 <- function(dataframe) {
  columns_to_remove <- names(dataframe)[grep("\\d+", names(dataframe))]
  
  # Extract only the numeric part of each column name
  columns_to_remove_numeric <- as.numeric(gsub("\\D", "", columns_to_remove))
  
  # Identify numeric parts greater than 56
  columns_to_remove_final <- columns_to_remove[columns_to_remove_numeric > 56]
  
  # Remove columns with numeric parts greater than 56
  dataframe <- dataframe[, !(names(dataframe) %in% columns_to_remove_final)]
  
  return(dataframe)
}

# Function to calculate Gini coefficient for each year
calculate_gini_coefficient_by_year <- function(data_frame, property_column_name, year_column_name) {
  unique_years <- unique(data_frame[[year_column_name]])
  gini_results <- data.frame(Year = numeric(), Gini_Coefficient = numeric())
  for (i in unique_years) {
    subset_data <- subset(data_frame, year == i)
    print(subset_data)
    
    # Get the property values from the subset
    property_values <- subset_data[[property_column_name]]
    
    # Sort nodes based on the property values
    sorted_nodes <- order(property_values)
    
    # Calculate the Gini coefficient
    n <- length(property_values)
    gini_coefficient <- (2 * sum(seq_along(sorted_nodes) * property_values[sorted_nodes]) - (n + 1) * sum(property_values)) / (n * sum(property_values))
    
    # Print or store the result for each year
    cat("Year:", i, " - Gini Coefficient:", gini_coefficient, "\n")
    gini_results <- rbind(gini_results, data.frame(Year = i, Gini_Coefficient = gini_coefficient))
    
  }
  return(gini_results)
  print(gini_results)
}





trades_final<-data.frame()


for (i in 14:0)
{
  r=2000+i
  load(glue("C:/Users/robin/OneDrive/Desktop/EC331 code/EC331 submission/Rdata/WIOT{r}_October16_ROW.RData"))
  trades<-wiot
  trades<-remove_columns_gt_56(trades)
  trades<-subset(trades, select=-c(TOT))
  trades<-subset(trades, Country!="TOT")
  new1=subset(trades,select=c(IndustryCode,Year,Country))
  year<-paste(new1$Year)
  industryCode<-paste(new1$IndustryCode)
  industryCountry<-paste(new1$IndustryCode,new1$Country)
  country<-paste(new1$Country)
  new1=subset(trades, select=-c(IndustryCode,IndustryDescription,RNr,Year,Country))
  colnames(new1)<-industryCountry
  new1<-new1 %>% 
    cbind(industryCode)
  new1<-new1 %>% 
    cbind(year)
  new1<-new1%>%
    cbind(country)
  new1$industryCode<- paste(new1$industryCode,new1$country)
  new1<-relocate(new1,"industryCode", .before="A01 AUS")
  new1<-relocate(new1,"year", .before ="A01 AUS")
  new1<-relocate(new1, country, .before="A01 AUS")
  trades_final<-trades_final %>% 
    rbind(new1)
}

#can now subset to each country we want; for example:#
trades_final<-subset(trades_final)
data<-trades_final
unique_countries<-unique(data$country)
gini_results_list_indegree<-list()

for (Country in unique_countries){
  # Subset data for the specific country
  country_data <- subset(data, Country == country)
  print(Country)
  new2<-melt(country_data)
  new2<-relocate(new2,"year", .after ="value")
  new2<-relocate(new2, "country", .after="year")
  colnames(new2)<-c("From","To","Value","Year" ,"Country")
  # Get unique years for the specific country
  years_country <- unique(country_data$year)
  # Initialize list for adjacency matrices
  adjacency_matrices <- list()
# Create adjacency matrices for each year
  for(i in years_country) {
    print(paste("Processing country:", Country, "Year:", i))
    year_data <- subset(new2, Year == i, Value!=0)
    g <- graph_from_data_frame(d=year_data, directed=TRUE, vertices=NULL)
    adjmat <- as_adjacency_matrix(g, type="both", attr="Value", sparse=FALSE)
    adjacency_matrices[[as.character(i)]] <- adjmat
  }
  normalised_adjacency_matrices<- list()
  row_sums<-list()
  results<-list()
  #factors<-list()
  # Define breaks and labels
  #my_breaks <- c(0, 1, 2, 3, 100)
  #my_labels <- c("Low", "Medium", "High", "Very High")
  for(i in names(adjacency_matrices)) {
    scale = rowSums(adjacency_matrices[[i]])
    scale[scale == 0] <- 1
    normalised_adjacency_matrices[[as.character(i)]] <- scale(adjacency_matrices[[i]], center = FALSE, scale = scale)
    results[[as.character(i)]]<- colSums(scale(adjacency_matrices[[i]],center=FALSE, scale=scale))
  }
  resultsdataframe<-data.frame()
  for(i in names(results)) {
    resultsdataframe<-rbind(resultsdataframe, results[[as.character(i)]])
  }
  resultsdataframe<-cbind(resultsdataframe, years_country)
  resultsdataframe[is.na(resultsdataframe)] <- 0
  colnames(resultsdataframe)<-c(as.character(unique(new2$To)), "year")
  # Convert from wide to long format
  long_results_df <- pivot_longer(resultsdataframe, cols = -year, names_to = "Statistic", values_to = "Value")
  gini_result_indegree<-calculate_gini_coefficient_by_year(long_results_df,"Value", "year")
  gini_results_list_indegree[[Country]]<-gini_result_indegree
}

# Combine list into a single dataframe
giniexports <- bind_rows(gini_results_list_indegree, .id="Country")
write.csv(giniexports, file="gini_results_exports.csv")
