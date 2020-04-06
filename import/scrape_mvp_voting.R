
##### This program web scrapes MVP data #####

library(rvest); library(stringr); library(tidyr);library(plyr);library(dplyr)
library(ggplot2)

# Initialize final DataFrame
final <- data.frame()

#--- Loop for all years ---#
for (yr in 1956:2019) {
    
    #--- Define path for data ---#
  
    webpage <- paste0("https://www.basketball-reference.com/awards/awards_",yr,".html#mvp")
    node <- html_nodes(read_html(webpage), 'table')
        
    #--- Scrape data and format ---#
    
    # Scrape
    voting <- html_table(node, header=TRUE, fill=TRUE)[[1]]
    
    # Rename columns
    colnames(voting) <- voting[1,]
    voting <- voting[-1,]
    
    # Make year
    voting$year <- yr
        
    # Stack with final DataFrame
    final <- rbind.fill(final,voting)

}

# Basic checks
table(final$year)

# Read out DataFrame to local location
setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta/")
saveRDS(final,"Raw MVP Data.RData")
