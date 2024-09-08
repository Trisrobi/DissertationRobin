library(xtranat)
#setting up the environment
library(igraph)
library(tidyverse)
library(glue)
library(reshape2)

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


calculate_entropy_by_year <- function(data_frame, property_column_name, year_column_name) {
  unique_years <- unique(data_frame[[year_column_name]])
  entropy_results <- data.frame(Year = numeric(), Entropy = numeric())
  for (i in unique_years) {
    subset_data <- subset(data_frame, year == i)
    print(subset_data)
    
    # Get the property values from the subset
    property_values <- subset_data[[property_column_name]]
    
    # Normalize property values to create a probability distribution
    property_prob <- property_values / sum(property_values)
    cat(property_prob)
    
    # Calculate entropy
    entropy <- -sum(property_prob * log2(property_prob), na.rm = TRUE)
    
    # Print or store the result for each year
    cat("Year:", i, " - Entropy :", entropy, "\n")
    entropy_results <- rbind(entropy_results, data.frame(Year = i, Entropy = entropy))
  }
  
  return(entropy_results)
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

trades_final<-subset(trades_final)
data<-trades_final
unique_countries<-unique(data$country)
entropy_results_list<-list()

for (Country in unique_countries){
  
  # Subset data for the specific country
  country_data_entropy <- subset(data, Country == country)
  print(Country)
  new2_entropy<-melt(country_data_entropy)
  new2_entropy<-relocate(new2_entropy,"year", .after ="value")
  new2_entropy<-relocate(new2_entropy, "country", .after="year")
  
  colnames(new2_entropy)<-c("From","To","Value","Year" ,"Country")
  
  # Get unique years for the specific country
  years_country_entropy <- unique(country_data_entropy$year)
  
  # Initialize list for adjacency matrices
  adjacency_matrices_entropy <- list()
  
  # Create adjacency matrices for each year
  for(i in years_country_entropy) {
    print(paste("Processing country:", Country, "Year:", i))
    year_data_entropy <- subset(new2_entropy, Year == i, Value!=0)
    g_entropy <- graph_from_data_frame(d=year_data_entropy, directed=TRUE, vertices=NULL)
    adjmat_entropy <- as_adjacency_matrix(g_entropy, type="both", attr="Value", sparse=FALSE)
    adjacency_matrices_entropy[[as.character(i)]] <- adjmat_entropy
  }
  normalised_adjacency_matrices_entropy<- list()
  row_sums<-list()
  results_entropy<-list()
  
  for(i in names(adjacency_matrices_entropy)) {
    scale = colSums(adjacency_matrices_entropy[[i]])
    scale[scale == 0] <- 1
    normalised_adjacency_matrices_entropy[[as.character(i)]] <- scale(adjacency_matrices_entropy[[i]], center = FALSE, scale = scale)
    results_entropy[[as.character(i)]]<- rowSums(scale(adjacency_matrices_entropy[[i]],center=FALSE, scale=scale))
  }
  
  resultsdataframe_entropy<-data.frame()
  for(i in names(results)) {
    resultsdataframe_entropy<-rbind(resultsdataframe_entropy, results_entropy[[as.character(i)]])
  }
  resultsdataframe_entropy<-cbind(resultsdataframe_entropy, years_country_entropy)
  resultsdataframe_entropy[is.na(resultsdataframe_entropy)] <- 0
  colnames(resultsdataframe_entropy)<-c(as.character(unique(new2_entropy$To)), "year")
  # Convert from wide to long format
  long_results_df <- pivot_longer(resultsdataframe_entropy, cols = -year, names_to = "Statistic", values_to = "Value")
  entropy_result<-calculate_entropy_by_year(long_results_df,"Value", "year")
  entropy_results_list[[Country]]<-entropy_result
}

# Combine list into a single dataframe
entropy_rowsimports <- bind_rows(entropy_results_list, .id="Country")
write.csv(entropy_rowsimports, file="entropyforimports.csv")

