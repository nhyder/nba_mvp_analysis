
##### This program web scrapes NBA standings data #####

library(rvest); library(stringr); library(tidyr);library(plyr);library(dplyr)
library(ggplot2)

# Initialize final DataFrame
final <- data.frame()

#--- Loop for years 1956 to 1970 (1 table only) ---#
for (yr in 1956:1970) {
  
  #--- Define path for data ---#
  
  webpage <- paste0("https://www.basketball-reference.com/leagues/NBA_",yr,"_standings.html")
  node <- html_nodes(read_html(webpage), 'table')
  
  #--- Scrape data and format ---#
  
  # Scrape
  standings <- html_table(node, header=TRUE, fill=TRUE)[[1]]
  names(standings)[1] <- "team"
  
  # Make year
  standings$year <- yr
  
  # Stack along
  final <- rbind.fill(final,standings)
  
}

#--- Loop for years 1971 to 2019 (2 tables) ---#

for (yr in 1971:2019) {
  
  #--- Define path for data ---#
  
  webpage <- paste0("https://www.basketball-reference.com/leagues/NBA_",yr,"_standings.html")
  node <- html_nodes(read_html(webpage), 'table')
  
  #--- Scrape data and format ---#
  
  # Scrape
  east <- html_table(node, header=TRUE, fill=TRUE)[[1]]
  names(east)[1] <- "team"
  
  west <- html_table(node, header=TRUE, fill=TRUE)[[2]]
  names(west)[1] <- "team"
  
  # Stack east and west
  standings <- rbind.fill(east,west)
  
  # Make year
  standings$year <- yr
  
  # Stack with final DataFrame
  final <- rbind.fill(final,standings)
  
}

# Checks
table(final$year)
table(final$team)

# Exclude the rows with "Division"
final2 <- filter(final, !grepl("Division", team))
table(final2$year)

# Write to local path
setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta/")
saveRDS(final2,"Raw Final Standings.RData")
 
    
    
