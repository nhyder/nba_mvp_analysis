

##### This program makes plots for the executive summary #####

## Call packages
library(rvest); library(stringr); library(tidyr);library(plyr);library(dplyr)
library(ggplot2)

setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta/")

base <- readRDS("Analysis Base.RData")

head(base)
str(base)

base <- subset(base,year > 1981)


table(base$year)


## Counts 
length(unique(base$Player))

per.year <- base %>%
            group_by(year) %>%
            summarise(n=n())

mean(per.year$n)




## Limit to MVP records
table(base$mvp)

mvp <- subset(base,mvp==1 & year>1981)
mvp <- mvp[order(mvp$year),]

table(mvp$Player)

mvp.count <- mvp %>%
              group_by(Player) %>%
                summarise(n.mvp = sum(mvp))

mvp.count <- mvp.count[order(-mvp.count$n.mvp),]



## Plot MVP winner stats by year

temp <- subset(mvp, select=c("Player","year","PTS","TRB","AST","Share","WL.PCT","WS"))
temp

players <- read.csv("Player_Groups.csv")

temp <- merge(temp,players,by="Player")

plot <- ggplot(temp,aes(x=year)) +
          geom_line(aes(y=AST),color="Red") +
          geom_line(aes(y=PTS),color="Green") +
          geom_line(aes(y=TRB),color="Blue")

plot



### Plot avg points across years
table(base$Rank)

subset(base,Rank %in% c("1T","2T","3T","4T","5T"))


top.five.avg <- subset(base,Rank %in% c("1","2","3","4","5","1T","2T","3T","4T","5T")) %>%
                  group_by(year) %>%
                    summarise(mean.pts=mean(PTS), mean.trb=mean(TRB), mean.ast=mean(AST), mean.mp=mean(MP))
top.five.avg



plot2 <- ggplot(top.five.avg,aes(x=year)) +
          geom_line(aes(y=mean.ast),color="Red") +
          geom_line(aes(y=mean.pts),color="Green") +
          geom_line(aes(y=mean.trb),color="Blue") +
          geom_line(aes(y=mean.mp),color="Gray")
plot2



## Plot winner vs top 4

two.to.four.avg <- subset(base,Rank %in% c("2","3","4","2T","3T","4T")) %>%
  group_by(year) %>%
  summarise(mean.pts=mean(PTS), mean.trb=mean(TRB), mean.ast=mean(AST), mean.mp=mean(MP))
two.to.four.avg


plot3 <- ggplot() +
          geom_line(data=subset(base,mvp==1),aes(x=year,y=PTS),color="black") +
          geom_line(data=two.to.four.avg,aes(x=year,y=mean.pts),color="black",linetype=2) + 
  
          geom_line(data=subset(base,mvp==1),aes(x=year,y=AST),color="blue") +
          geom_line(data=two.to.four.avg,aes(x=year,y=mean.ast),color="blue",linetype=2) + 
  
          ylim(0,40) +
  
          theme_bw()
plot3





## Share

table(base$Rank)

plot4 <- ggplot() +
  geom_line(data=subset(base,mvp==1),aes(x=year,y=Share),color="black") +
  geom_line(data=subset(base,Rank=="2"),aes(x=year,y=Share),color="blue") +
  
  ylim(0,1) +
  
  theme_bw()
plot4






              