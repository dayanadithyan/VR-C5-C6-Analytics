# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("tidytext")
install.packages("glue")
install.packages("cowplot")
install.packages("magrittr")
install.packages("plotly")
install.packages("tidyverse")
install.packages("widyr")
install.packages("igraph")
install.packages("networkD3")
install.packages("hms")
install.packages("lubridate") 


# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidytext)
library(glue)
library(cowplot)
library(magrittr)
library(plotly)
library(tidyverse)
library(widyr)
library(igraph)
library(networkD3)
library(hms)
library(lubridate) 


C5text<- readLines("C5SummaryMine.txt")

C5.corpus <- Corpus(VectorSource(C5text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

C5.corpus <- tm_map(C5.corpus, toSpace, "/")
C5.corpus <- tm_map(C5.corpus, toSpace, "@")
C5.corpus <- tm_map(C5.corpus, toSpace, "\\|")

C5.text.tibble <- as_tibble(C5text)

bi.gram.words.c5 <- C5.text.tibble %>% 
  unnest_tokens(
    input = value, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.c5 %>% 
  select(bigram) %>% 
  tail(10) ## lmao the tail is more interesting than head, one for the notebook

stopwords.df <- tibble(
  word = c(stopwords(kind = 'en')))
           
bi.gram.words.c5 %<>% 
  separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2))

bi.gram.count.c5 <- bi.gram.words.c5 %>% 
  count(word1, word2, sort = TRUE) %>% 
  # We rename the weight column so that the 
  # associated network gets the weights (see below).
  rename(weight = n)

bi.gram.count.c5 %>% head()

bi.gram.count.c5 %>% 
  ggplot(mapping = aes(x = weight)) +
  theme_light() +
  geom_histogram() +
  labs(title = "Bigram Weight Distribution")

threshold <- 10

ScaleWeight <- function(x, lambda) {
  x / lambda
}

network.c5 <-  bi.gram.count.c5 %>%
  filter(weight > threshold) %>%
  mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
  graph_from_data_frame(directed = FALSE)

is.weighted(network.c5)


plot(
  network.c5, 
  vertex.size = 1,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.7, 
  vertex.label.dist = 1,
  edge.color = 'gray', 
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

V(network.c5)$degree <- strength(graph = network.c5)

E(network.c5)$width <- E(network.c5)$weight/max(E(network.c5)$weight)

plot(
  network.c5, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 2*V(network.c5)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(network.c5)$width ,
  main = 'Bigram Count Network', 
  sub = glue('Weight Threshold: {threshold}'), 
  alpha = 50
)

clusters(graph = network.c5)

V(network.c5)$cluster <- clusters(graph = network.c5)$membership

c5.clust.net <- induced_subgraph(
  graph = network.c5,
  vids = which(V(network.c5)$cluster == which.max(clusters(graph = network.c5)$csize))
)

c5.clust.net

V(c5.clust.net)$degree <- strength(graph = c5.clust.net)

E(c5.clust.net)$width <- E(c5.clust.net)$weight/max(E(c5.clust.net)$weight)

plot(
  c5.clust.net, 
  vertex.color = 'lightblue',
  # Scale node size by degree.
  vertex.size = 10*V(c5.clust.net)$degree,
  vertex.label.color = 'black', 
  vertex.label.cex = 0.6, 
  vertex.label.dist = 1.6,
  edge.color = 'gray', 
  # Set edge width proportional to the weight relative value.
  edge.width = 3*E(c5.clust.net)$width ,
  main = 'Bigram Count Network (Biggest Connected Component)', 
  sub = glue('Weiight Threshold: {threshold}'), 
  alpha = 50
)




