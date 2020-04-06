

##### This program does statistical analyses #####

## Call packages
library(rvest); library(stringr); library(tidyr);library(plyr);library(dplyr)
library(ggplot2)

setwd("/Users/nicholashyder/Documents/Side Projects/NBA MVP Analysis/dta/")

base <- readRDS("Analysis Base.RData")

head(base)
str(base)

base <- subset(base,year > 1981)

base.all <- base

test <- (unique(base$Player))

test <- base %>% group_by(Player) %>%summarise(tot.mvps=sum(mvp))
test <- subset(test,test$tot.mvps>1)
test



## Break data into train and test ##
years <- unique(base$year)

test.years <- years[seq(1,length(years),2)]
test.years

train.base <- subset(base,!(year %in% test.years))
test.base <- subset(base,year %in% test.years)






#--- Logits to predict MVP ---#

#base <- train.base

mylogit <- glm(mvp ~ WL.PCT + SRS + diff_WS.48 + diff_PTS + diff_TRB + diff_AST + diff_BLK + diff_STL +
                    #lead_PTS + lead_TRB + lead_AST + lead_WS.48 +
                 lag.mvp1 + lag.mvp2, data = base, family = "binomial"(link="logit"))
summary(mylogit)

# Predict
base$prob <- predict(mylogit, newdata=base, type="response")


# Max out prob by year - max=prob means predicted mvp
maxs <- base %>% 
          group_by(year) %>% 
            summarise(max.prob = max(prob))

base <- merge(x=base,y=maxs,by="year")

base$mvp.pred <- ifelse(base$prob==base$max.prob,1,0)
table(base$year,base$mvp.pred)

base$match <- ifelse(base$mvp==base$mvp.pred,1,0)
chk <- subset(base,mvp.pred==1) %>% select(Player,year,prob,mvp)

mean(base$match)
mean(subset(base,mvp==1)$match)

wrong.years <- unique(subset(base,match==0 & mvp.pred==1)$year)
wrong.years

base <- arrange(base,year,-prob)
for (i in 1:length(wrong.years)) {

print(subset(base,year==wrong.years[i])[,c("year","Player","mvp","mvp.pred","prob","WL.PCT")])

}

subset(base,Player=="LeBron James") %>% select(Player,year,mvp,mvp.pred)
subset(base,Player=="Michael Jordan") %>% select(Player,year,mvp,mvp.pred)
subset(base,Player=="Magic Johnson") %>% select(Player,year,mvp,mvp.pred)
subset(base,Player=="Larry Bird") %>% select(Player,year,mvp,mvp.pred)

#--- Use results on test dataset ---#

# Predict
test.base$prob <- predict(mylogit, newdata=test.base, type="response")


# Max out prob by year - max=prob means predicted mvp
maxs <- test.base %>% 
  group_by(year) %>% 
  summarise(max.prob = max(prob))

test.base <- merge(x=test.base,y=maxs,by="year")

test.base$mvp.pred <- ifelse(test.base$prob==test.base$max.prob,1,0)
table(test.base$year,test.base$mvp.pred)

test.base$match <- ifelse(test.base$mvp==test.base$mvp.pred,1,0)

mean(test.base$match)
mean(subset(test.base,mvp==1)$match)

wrong.years <- unique(subset(test.base,match==0 & mvp.pred==1)$year)
wrong.years

test.base <- arrange(test.base,year,-prob)
for (i in 1:length(wrong.years)) {
  
  print(subset(test.base,year==wrong.years[i])[,c("year","Player","mvp","mvp.pred","prob","WL.PCT")])
  
}

other.years <- c(1993,2011)

for (i in 1:length(other.years)) {
  
  print(subset(test.base,year==other.years[i])[,c("year","Player","mvp","mvp.pred","prob","WL.PCT")])
  
}




#--- Regressions to predict points ---#
test.base <- subset(base,year %in% test.years)
train.base <- subset(base,!(year %in% test.years))

test.base <- base
train.base <- base

myreg <- glm(mvp ~ WL.PCT + SRS + diff_WS.48 + diff_PTS + diff_TRB + diff_AST + diff_BLK + diff_STL +
                 #lead_PTS + lead_TRB + lead_AST + lead_WS.48 +
               lag.Share1 + lag.Share2, data = base, family = "binomial"(link="logit"))
summary(myreg)


# Predict
test.base$pred.shares <- predict(myreg, newdata=test.base, type="response")


# Max out predicted shares by year - max means predicted mvp
maxs <- test.base %>% 
  group_by(year) %>% 
  summarise(max.pred.shares = max(pred.shares))

test.base <- merge(x=test.base,y=maxs,by="year")

test.base$mvp.pred <- ifelse(test.base$pred.shares==test.base$max.pred.shares,1,0)

test.base$match <- ifelse(test.base$mvp==test.base$mvp.pred,1,0)

mean(test.base$match)
mean(subset(test.base,mvp==1)$match)

wrong.years <- unique(subset(test.base,match==0 & mvp.pred==1)$year)
wrong.years




#--- Sensativity analysis ---#

# Only Player controls
mylogit2 <- glm(mvp ~ diff_WS.48 + diff_PTS + diff_TRB + diff_AST + diff_BLK + diff_STL +
                 lag.mvp1 + lag.mvp2, data = base, family = "binomial"(link="logit"))
summary(mylogit2)



# Only team controls
mylogit3 <- glm(mvp ~ WL.PCT + SRS +
                  lag.mvp1 + lag.mvp2, data = base, family = "binomial"(link="logit"))
summary(mylogit3)

