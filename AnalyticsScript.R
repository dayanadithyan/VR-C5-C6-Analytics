# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

check.ts <- as.data.frame(FinalC6DB$Date)
check.ts$score <- FinalC6DB$Total.Score


daily_sentinment_c5 <- FinalC5DB %>% 
  mutate(day=as.Date(Date, format ="%Y-%m-%d")) %>% 
  group_by(day) %>% 
  summarise(sum(Total.Score)) 

colnamesC5 <- c("Date", "Score")

colnames(daily_sentinment_c5) <- colnamesC5

raw.ts1 <-
  daily_sentinment_c5 %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), #complete dates, gaps filled with NAs
                                     max(Date), by = "day"))
#

raw.ts1$Score <- mapvalues(raw.ts1$Score, from = NA,
                             to = 0)

C5.finalTS <- raw.ts1

ggplot(data = C5.finalTS, aes(x = Date, y = Score))+
  geom_line(color = "#5f1613", size = 2) +
  stat_smooth(
    color = "#4d2238", fill = "#4d2238",
    method = "loess"
  )

ggplot(data = C5.testTS, aes(x = Date, y = Score))+
  geom_line(color = "#5f1613", size = 2) +
  stat_smooth(
    color = "#4d2238", fill = "#4d2238",
    method = "loess"
  )



monthly_C5_data <- C5.finalTS %>% 
  select(Date, Score) %>% 
  mutate(year_month = floor_date(Date, "months")) %>% 
  group_by() %>% 
  summarize(monthlyscore = sum(Score)) 

C5.testTS <- C5.finalTS

C5.testTS$Score <- mapvalues(C5.testTS$Score, from = 0, to = NA)

#heatmatrix <- FinalC5DB %>% 
  #select(Language, VG.Score, NG.Score)

#library(reshape2)
#melted_C5heat <- melt(heatmatrix)

#library(ggplot2)
#ggplot(data = heatmatrix, aes(x=Language, fill=NG.Score))
  

#heat.matrix <- as.data.frame(FinalC5DB$VG.Score)

#ggplot(data = FinalC5DB, aes(x = Date, y= Total.Score)) +
  #geom_line(color = "#00AFBB", size = 2)

#positivesentC5 <- subset(FinalC5DB, Date > as.POSIXct(2020))

#ggplot(data = positivesentC5, aes(x = Date, y=Total.Score))+ 
  #geom_line(color = "#00AFBB", size = 2)

#spectrum(check.ts)
