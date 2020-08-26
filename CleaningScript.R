library(readxl)

C5DB.main <- read_excel("C5main.xlsx") #tibble 

C6DB.main <- read_excel("C6main.xlsx") #tibble 

View(C5DB.main)
View(C6DB.main)
str(C5DB.main)
str(C6DB.main)

C5.DF <- as.data.frame(unclass(C5DB.main)) #classic DF for cat

C6.DF <- as.data.frame(unclass(C6DB.main)) #classic DF for cat

## create factors w/ levels

C5.DF[sapply(C5.DF, is.character)] <- lapply(C5.DF[sapply(C5.DF, is.character)], 
                                             as.factor)

C6.DF[sapply(C6.DF, is.character)] <- lapply(C6.DF[sapply(C6.DF, is.character)], 
                                             as.factor)

str(C5.DF)
str(C6.DF)

#library(tidyverse)

check.wtf <- C5.DF %>% filter(TypeOfArticle  == "long opinion")

check.wtf2 <- C6.DF %>% filter(TypeOfArticle  == "column")

library(plyr)

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Long opinion", "long opinion"),
                                 to = c("Long Opinion", "Long Opinion"))

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Short opinion", "Picture Article"),
                                 to = c("Short Opinion", "Pictorial"))

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Weekly coloumn"),
                                 to = c("Column"))

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Weekly coloumn"),
                                 to = c("Column"))

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Book review"),
                                 to = c("Book Review"))

C5.DF$TypeOfArticle <- mapvalues(C5.DF$TypeOfArticle, from = c("Book Review"),
                                 to = c("Advertorial"))
#########

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("column"),
                                 to = c("Column"))

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("Feauture", "Long  opinion"),
                                 to = c("Feature", "Long Opinion"))

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("Picture article", "Short opinion"),
                                 to = c("Picture Article", "Short Opinion" ))

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("news", "Long article"),
                                 to = c("News", "Long Opinion" ))

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("Long opinion", "Weekly coloumn"),
                                 to = c("Long Opinion", "Column" ))

C6.DF$TypeOfArticle <- mapvalues(C6.DF$TypeOfArticle, from = c("Picture Article"),
                                 to = c("Pictorial"))

levels(C5.DF$TypeOfArticle) == levels(C6.DF$TypeOfArticle) ## VALIDATION

##### CLEAN NEWSPAPER NAMES ####

levels(C5.DF$Newspaper) == levels(C6.DF$Newspaper) ## VALIDATION


## C5

C5.DF$Newspaper <- mapvalues(C5.DF$Newspaper, from = c("daily news"), to = c("Daily News"))

C5.DF$Newspaper <- mapvalues(C5.DF$Newspaper, from = c("Tamil mirror"), to = c("Tamil Mirror"))

C5.DF$Newspaper <- mapvalues(C5.DF$Newspaper, from = c("Thinakural"), to = c("Thinakkural"))

## C6

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("daily news", "Daily news"),
                             to = c("Daily News", "Daily News"))

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("Daily mirror"),
                             to = c("Daily Mirror"))

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("lankadeepa"),
                             to = c("Lankadeepa"))

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("the island"),
                             to = c("The Island"))

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("The Sunday times"),
                             to = c("The Sunday Times"))

C6.DF$Newspaper <- mapvalues(C6.DF$Newspaper, from = c("Thinakural"),
                             to = c("Thinakkural"))


vg.scores <- vector()

for(i in 1:nrow(C5.DF)) {
  if(C5.DF[i,4] == "Neutral") {
    vg.scores[i] <- paste0(0)
  } else if (C5.DF[i,4] == "Positive") {
    vg.scores[i] <-paste0(1)
  } else if (C5.DF[i,4] == "Negative") {
    vg.scores[i] <-paste0(-1)
  }}

C5.DF$VG.Score <- as.integer(vg.scores)

ng.scores <- vector()

for(i in 1:nrow(C5.DF)) {
  if(C5.DF[i,5] == "Neutral") {
    ng.scores[i] <- paste0(0)
  } else if (C5.DF[i,5] == "Positive") {
    ng.scores[i] <-paste0(1)
  } else if (C5.DF[i,5] == "Negative") {
    ng.scores[i] <-paste0(-1)
  }}

C5.DF$NG.Score <- as.integer(ng.scores)

#ng.scores <- mapvalues(ng.scores, from = NA, to = 1)

tot.score.C5 <- C5.DF$NG.Score + C5.DF$VG.Score

C5.DF$Total.Score <- as.integer(tot.score.C5)

FinalC5DB <- as.tibble(C5.DF)

### REPEAT FOR C6 

vg.scores.c6 <- vector()

for(i in 1:nrow(C6.DF)) {
  if(C6.DF[i,4] == "Neutral") {
    vg.scores.c6[i] <- paste0(0)
  } else if (C6.DF[i,4] == "Positive") {
    vg.scores.c6[i] <-paste0(1)
  } else if (C6.DF[i,4] == "Negative") {
    vg.scores.c6[i] <-paste0(-1)
  }}

C6.DF$VG.Score <- as.integer(vg.scores.c6)

ng.scores.c6 <- vector()

for(i in 1:nrow(C6.DF)) {
  if(C6.DF[i,5] == "Neutral") {
    ng.scores.c6[i] <- paste0(0)
  } else if (C6.DF[i,5] == "Positive") {
    ng.scores.c6[i] <-paste0(1)
  } else if (C6.DF[i,5] == "Negative") {
    ng.scores.c6[i] <-paste0(-1)
  }}

#ng.scores.c6 <- mapvalues(ng.scores.c6, from = NA, to = 1)

C6.DF$NG.Score <- as.integer(ng.scores.c6)

tot.score.C6 <- C6.DF$NG.Score + C6.DF$VG.Score

C6.DF$Total.Score <- as.integer(tot.score.C6)

FinalC6DB <- as.tibble(C6.DF)

str(FinalC6DB)

View(FinalC6DB)

write_csv(FinalC6DB, file.path(path.cd, "C6-CLEANDATA.csv"))

write_csv(FinalC5DB, file.path(path.cd, "C5-CLEANDATA.csv"))


