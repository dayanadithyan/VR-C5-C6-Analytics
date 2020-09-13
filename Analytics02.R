
library(dplyr)
library(magrittr)
install_formats()
library(rio)
  

  #####
head(FinalC5DB)
str(FinalC6DB)
head(FinalC6DB)
str(FinalC6DB)

mean(FinalC5DB$Total.Score, na.rm = TRUE)

mean(FinalC6DB$Total.Score, na.rm = TRUE)

sd(FinalC5DB$Total.Score, na.rm = TRUE)

sd(FinalC6DB$Total.Score, na.rm = TRUE)


aggregate(FinalC5DB$Total.Score,
          by = list(Newspaper = FinalC5DB$Newspaper,
                    Language = FinalC5DB$Language),
          mean)

aggregate(FinalC6DB$Total.Score,
          by = list(Newspaper = FinalC6DB$Newspaper,
                    Language = FinalC6DB$Language),
          mean)

aggregate(FinalC5DB$Total.Score,
          by = list(Newspaper = FinalC5DB$Newspaper,
                    Language = FinalC5DB$Language),
          sd)

aggregate(FinalC6DB$Total.Score,
          by = list(Newspaper = FinalC6DB$Newspaper,
                    Language = FinalC6DB$Language),
          sd)


#ARRAY 
tapply(FinalC5DB$Total.Score, list(FinalC5DB$Newspaper), mean)

tapply(FinalC6DB$Total.Score, list(FinalC6DB$Newspaper), mean)



C5_Sinhala <- subset(FinalC5DB, Language == "Sinhala")

C5_Tamil <- subset(FinalC5DB, Language == "Tamil")

C5_English <- subset(FinalC5DB, Language == "English")

C6_Sinhala <- subset(FinalC6DB, Language == "Sinhala")

C6_Tamil <- subset(FinalC6DB, Language == "Tamil")

C6_English <- subset(FinalC6DB, Language == "English")


aggregate(C5_Sinhala$Total.Score,
          by = list(Newspaper = C5_Sinhala$Newspaper),
          mean)

aggregate(C5_Sinhala$Total.Score,
          by = list(Newspaper = C5_Sinhala$Newspaper),
          sd)

aggregate(C5_English$Total.Score,
          by = list(Newspaper = C5_English$Newspaper),
          mean)

aggregate(C5_English$Total.Score,
          by = list(Newspaper = C5_English$Newspaper),
          sd)

aggregate(C5_Tamil$Total.Score,
          by = list(Newspaper = C5_Tamil$Newspaper),
          mean)

aggregate(C5_Tamil$Total.Score,
          by = list(Newspaper = C5_Tamil$Newspaper),
          sd)


################### dplyr ninjutsu


mean_bylang_C5<- FinalC5DB %>% 
  group_by(Language) %>% 
  summarise(mean(Total.Score))

library(gridExtra)
png("test.png", height = 50*nrow(mean_bylang_C5), width = 200*ncol(mean_bylang_C5))
grid.table(mean_bylang_C5)
dev.off()
###

C5_Sinhala_Mean <- FinalC5DB %>% 
filter(Language == "Sinhala") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score))
####

C5_Tamil_Mean <- FinalC5DB %>% 
  filter(Language == "Tamil") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score))
#####

C5_English_Mean <- FinalC5DB %>% 
  filter(Language == "English") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score)) 

####

C6_Sinhala_Mean <- FinalC6DB %>% 
  filter(Language == "Sinhala") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score))

png("C6SinhalaMean.png", height = 50*nrow(C6_Sinhala_Mean), width = 200*ncol(C6_Sinhala_Mean))
grid.table(C6_Sinhala_Mean)
dev.off()

print(C6_Sinhala_Mean)

######

C6_Tamil_Mean <- FinalC6DB %>% 
  filter(Language == "Tamil") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score))
  #####

print(C6_Tamil_Mean)

C6_English_Mean <- FinalC6DB %>% 
  filter(Language == "English") %>% 
  group_by(Newspaper) %>% 
  summarise(mean(Total.Score)) 
  
print(C6_English_Mean)

##############

C5_negative_press_Sinhala <- C5_Sinhala %>%
  filter(Total.Score < 0)

C5_negative_view_Sinhala<- C5_Sinhala %>%
  filter(VG.Score < 0)

C5_positive_view_Sinhala <- C5_Sinhala %>% 
  filter(VG.Score > 0)


C6_negative_press_Sinhala <- C6_Sinhala %>%
  filter(Total.Score < 0)

C6_positive_view_Sinhala<- C6_Sinhala %>%
  filter(VG.Score > 0)

C6_negative_view_Sinhala<- C6_Sinhala %>%
  filter(VG.Score < 0)


###############

print(C5_negative_press_Sinhala)

print(C5_positive_view_Sinhala)

print(C5_negative_view_Sinhala)

########

C5_Sinhala_Correspondents <-
  C5_Sinhala %>% 
  filter(!is.na(Coresspondent))

print(C5_Sinhala_Correspondents)


  C5_negativecorrespondenceSinhala <- 
    C5_Sinhala %>% 
    filter(!is.na(Coresspondent)) %>% 
    filter(NG.Score < 0)

  
  ### noted keerthi warnakalasuriya 
  
  ## accounts for 8/15 articles
  
  View(table(C5_negativecorrespondenceSinhala$Coresspondent))

  ##########
### ANALYSIS BY YEAR #####
  
 C5_2020 <-
    FinalC5DB %>% 
    dplyr::filter(Date >= as.POSIXct("2020-01-01"))
  
  C5_2019 <- 
    FinalC5DB %>% 
    dplyr::filter(Date >= as.POSIXct("2019-01-01") & Date <= "2019-12-31")
  
  C5_2018 <-
    FinalC5DB %>% 
    dplyr::filter(Date <= as.POSIXct("2018-12-31"))
  
  
 ########## C6 
  
  C6_2020 <-
    FinalC6DB %>% 
    dplyr::filter(Date >= as.POSIXct("2020-01-01"))
  
  C6_2019 <- 
    FinalC6DB %>% 
    dplyr::filter(Date >= as.POSIXct("2019-01-01") & Date <= "2019-12-31")
  
  C6_2018 <-
    FinalC6DB %>% 
    dplyr::filter(Date <= as.POSIXct("2018-12-31"))
  
  
  #######
  
  C5_2020 %>% select(Date,Newspaper,VG.Score)




