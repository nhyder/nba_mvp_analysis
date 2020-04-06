

##### This program builds analysis data #####

## Call packages
library(rvest); library(stringr); library(tidyr);library(plyr);library(dplyr)
library(ggplot2);library(taRifx)

#--- Bring in data ---#

setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta/")
standings <- readRDS("Raw Final Standings.RData")
  head(standings)
  
### Standings data
standings <- plyr::rename(standings, c("team"="team.old"))
  head(standings)
  str(standings)
  
# Rename
standings <- plyr::rename(standings, c("team"="team.old","W/L%"="WL.PCT","PS/G"="PS.G","PA/G"="PA.G"))
head(standings)
  
  
vars <- colnames(subset(standings,select=-c(team.old)))

  # Make numeric
standings[] <- lapply(standings, function(x) type.convert(as.character(x), as.is = TRUE))
head(standings)
str(standings)
  

### MVP data
mvp <- readRDS("Raw MVP Data.RData")
head(mvp)
str(mvp)
    
# Rename
mvp <- plyr::rename(mvp, c("Tm"="team","Pts Won"="pts.won","Pts Max"="pts.max","FG%"="FG.PCT","3P%"="3PT.PCT","FT%"="FT.PCT","WS/48"="WS.48"))
head(mvp)
    
# Make numeric
mvp[] <- lapply(mvp, function(x) type.convert(as.character(x), as.is = TRUE))
head(mvp)
str(mvp)

  
  
    
#--- Match on team name ---#

# Fill totals with second team
subset(mvp,team == "TOT")

  mvp$team[mvp$Player=="Moses Malone" & mvp$team=="TOT"] <- "HOU"
  mvp$team[mvp$Player=="Stephen Jackson" & mvp$team=="TOT"] <- "GSW"
  mvp$team[mvp$Player=="Fred Carter" & mvp$team=="TOT"] <- "PHI"
  mvp$team[mvp$Player=="Vince Carter" & mvp$team=="TOT"] <- "NJN"
  mvp$team[mvp$Player=="Chauncey Billups" & mvp$team=="TOT"] <- "DEN"
  mvp$team[mvp$Player=="Clyde Drexler" & mvp$team=="TOT"] <- "HOU"
  mvp$team[mvp$Player=="Walt Bellamy" & mvp$team=="TOT"] <- "ATL"
  mvp$team[mvp$Player=="Archie Clark" & mvp$team=="TOT"] <- "BAL"
  mvp$team[mvp$Player=="Wilt Chamberlain" & mvp$team=="TOT"] <- "PHI"
  mvp$team[mvp$Player=="Dominique Wilkins" & mvp$team=="TOT"] <- "LAC"
  
subset(mvp,team == "TOT")


table(mvp$team)
table(standings$team)

# Strip out team name
standings$teamname <- gsub("76ers", "#", standings$team.old)
standings$teamname <- gsub("[0-9]", "", standings$teamname)
standings$teamname <- gsub("[*]", "", standings$teamname)
standings$teamname <- gsub("[()]", "", standings$teamname)
standings$teamname <- gsub("#", "76ers", standings$teamname)
standings$teamname <- gsub("New Orleans/Oklahoma City Hornets", "New Orleans Hornets", standings$teamname)
standings$teamname <- str_trim(standings$teamname,c("both"))

unique(standings$teamname)





## Bring in and match on lookup

lkup <- read.csv("Team Lookup.csv", header=T)
head(lkup)

mvp <- merge(x=mvp,y=lkup,by="team",all.x=TRUE)
  unique(mvp[c("team","teamname")])
  unique(subset(mvp,!(is.na(teamname)))[c("team","teamname")])
  unique(subset(mvp,(is.na(teamname)))[c("team","teamname")])
  

#--- Merge MVP and standings data ---#


# Match on team wins
final <- merge(x=mvp,y=standings,by=c("year","teamname"),all.x=T)
head(final)

subset(final.1,is.na(W))




#--- Create the rest of the analysis variables ---#

# Effective FG %
final$EFG.PCT <- final$FG.PCT + final$FT.PCT/2

# MVP Indicator
final$mvp <- ifelse(str_trim(final$Rank)=="1",1,0)

table(final$mvp)
table(subset(final,mvp==1)$year)
max(mvp$year) - min(mvp$year) + 1


# Pct of games played

final$G.PCT <- final$G/(final$W + final$L)

head(subset(final[c("Player","G","W","L","G.PCT")]))


### Lag vars
final <- arrange(final,Player,year)


# First lag
final <- 
  final %>%
  group_by(Player) %>%
  mutate(lag.mvp1 = lag(mvp, n = 1, default = 0))

final <- 
  final %>%
  group_by(Player) %>%
  mutate(lag.Share1= lag(Share, default = 0))


# Second lag
final <- 
  final %>%
  group_by(Player) %>%
  mutate(lag.mvp2 = lag(lag.mvp1, n = 1, default = 0))

final <- 
  final %>%
  group_by(Player) %>%
  mutate(lag.Share2= lag(lag.Share1, default = 0))



### Standardize stats ###

## Calculate maxes
final <- final %>% group_by(year) %>% mutate(max_PTS=max(PTS))
final <- final %>% group_by(year) %>% mutate(max_AST=max(AST))
final <- final %>% group_by(year) %>% mutate(max_TRB=max(TRB))
final <- final %>% group_by(year) %>% mutate(max_STL=max(STL))
final <- final %>% group_by(year) %>% mutate(max_BLK=max(BLK))
final <- final %>% group_by(year) %>% mutate(max_WS.48=max(WS.48))
final <- final %>% group_by(year) %>% mutate(max_WL.PCT=max(WL.PCT))
final <- final %>% group_by(year) %>% mutate(max_SRS=max(SRS))


## Calculate maxes
final$diff_PTS <- final$PTS - final$max_PTS
final$diff_AST <- final$AST - final$max_AST
final$diff_TRB <- final$TRB - final$max_TRB
final$diff_STL <- final$STL - final$max_STL
final$diff_BLK <- final$BLK - final$max_BLK
final$diff_WS.48 <- final$WS.48 - final$max_WS.48
final$diff_WL.PCT <- final$WL.PCT - final$max_WL.PCT
final$diff_SRS <- final$SRS - final$max_SRS


# Flag leaders

final$lead_PTS <- ifelse(final$PTS == final$max_PTS,1,0)
final$lead_AST <- ifelse(final$AST == final$max_AST,1,0)
final$lead_TRB <- ifelse(final$TRB == final$max_TRB,1,0)
final$lead_STL <- ifelse(final$STL == final$max_STL,1,0)
final$lead_BLK <- ifelse(final$BLK == final$max_BLK,1,0)
final$lead_WS.48 <- ifelse(final$WS.48 == final$max_WS.48,1,0)
final$lead_WL.PCT <- ifelse(final$WL.PCT == final$max_WL.PCT,1,0)
final$lead_SRS <- ifelse(final$SRS == final$max_SRS,1,0)



## Output dataset
setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta")
saveRDS(final,"Analysis Base.RData")





