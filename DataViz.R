library(ggplot2)
install.packages("ggthemes")

library(ggthemes)

###########
###########

VR_red <- '#5f1613'

VR_Palette <- c("#2d9de5", "#3bbdbd", "#634792")

vg_palette <- c("#980320", "#367fc7", "#a6ead8")

lang_palette <- c("#338fe1", "#f59c1e", "#f95a57")
  
ggplot(C5_2020, aes(Newspaper, fill = View.Grade)) +
  geom_bar(color=VR_red, alpha = 0.9) +
  scale_fill_manual(values=vg_palette) +
  coord_flip() +
  ylab('Number of reports in 2020') +
  xlab('Newspaper') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = VR_red),
        panel.background = element_blank())

ggplot(C5_2018, aes(Newspaper, fill = View.Grade)) +
  geom_bar(color=VR_red, alpha = 0.9) +
  coord_flip() +
  ylab('Number of reports in 2020') +
  xlab('Newspaper') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = VR_red))

ggplot(C5_2020, aes(Newspaper)) +
  geom_bar(fill="#5f1613") +
  coord_flip() +
  ylab('Number of reports in 2020') +
  xlab('Newspaper') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#4d2238"))





ggplot(C5_2020, aes(Language)) +
  geom_bar(fill="#5f1613") +
  coord_flip() +
  ylab('Freq') +
  xlab('Newspaper') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(C6_2020, aes(View.Grade)) +
  geom_bar()

###########
###########


ggplot(FinalC5DB, aes(Newspaper)) +
  geom_bar() +
  coord_flip()

ggplot(FinalC6DB, aes(Newspaper)) +
  geom_bar() +
  coord_flip()

ggplot(FinalC6DB, aes(Coresspondent)) +
  geom_bar() +
  coord_flip()

ggplot(FinalC6DB, aes(TypeOfArticle)) +
  geom_bar() +
  coord_flip()

############
############

ggplot(FinalC5DB, aes(Total.Score)) +
  geom_density()

ggplot(C5_Sinhala, aes(Total.Score)) +
  geom_density()

ggplot(C6_Sinhala, aes(Total.Score)) +
  geom_density()
  
ggplot(C6_Sinhala, aes(Total.Score)) +
  geom_density()

####
####

library(plyr)
library(dplyr)
library(magrittr)

FinalC5DB %>%
  filter(View.Grade %in% c("Positive", "Negative")) %>%
  ggplot(aes(Total.Score, fill = Language)) +
  geom_density() +
 scale_fill_manual(values=lang_palette) 




FinalC6DB %>%
  filter(View.Grade %in% c("Positive", "Negative")) %>%
  ggplot(aes(Total.Score, fill = Language)) +
  geom_density()


FinalC6DB %>%
  filter(View.Grade %in% c("Positive", "Negative")) %>%
  ggplot(aes(Total.Score, fill = TypeOfArticle)) +
  geom_density()

FinalC6DB %>%
  filter(View.Grade %in% c("Negative")) %>%
  ggplot(aes(Total.Score, fill = TypeOfArticle)) +
  geom_density()


C5_2020 %>%
  filter(Language == "Sinhala") %>%
  ggplot(aes(x = Date, y = VG.Score)) +
  geom_line()


library(dplyr)

FinalC5DB %>%
  ggplot(aes(x = Date, y = Total.Score, shape = View.Grade)) +
  geom_point(position = 'jitter')




C5.finalTS %>% 
  ggplot(aes(x = Date, y = Score)) + 
           geom_line(color = "#5f1613")




daily_sentinment_c6 <- FinalC6DB %>% 
  mutate(day=as.Date(Date, format ="%Y-%m-%d")) %>% 
  group_by(day) %>% 
  summarise(sum(Total.Score)) 


colnames(daily_sentinment_c6) <- colnamesC5

require(tidyr)

raw.ts1.c6 <-
  daily_sentinment_c6 %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), #complete dates, gaps filled with NAs
                           max(Date), by = "day"))

library(plyr)

raw.ts1.c6$Score <- mapvalues(raw.ts1.c6$Score, from = NA,
                           to = 0)
#

C6.FinalTS <- raw.ts1.c6

C6.FinalTS %>% 
  ggplot(aes(x = Date, y = Score)) + 
  geom_line(color = "#5f1613")

library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra)

C6.Copy.TS <- C6.FinalTS

library(dplyr)
library(plyr)
C6.Copy.TS$Score <- mapvalues(raw.ts1.c6$Score, from = 0,
                              to = NA)
#
  
qplot(x=Date, y=Score,
      data=C6.Copy.TS, na.rm=TRUE,
      main="Time Series",
      xlab="Date", ylab="Score")

ggplot(C6.Copy.TS, aes(Date, Score)) +
  geom_density2d(position='jitter', na.rm = TRUE) +
   stat_smooth(colour="green")



##################
##################

library(ggplot2)

## important 

ggplot(C5_2020, aes(Language, Total.Score, color = TypeOfArticle, size = Total.Score)) +
  geom_point() 


### remember geom_smooth

## edit aes inside geom_point


  




