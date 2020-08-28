network <-  bi.gram.count.c5 %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Store the degree.
V(network)$degree <- strength(graph = network)
# Compute the weight shares.
E(network)$width <- E(network)$weight/max(E(network)$weight)

# Create networkD3 object.
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  #  JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)

skip.window <- 2

skip.gram.words.c5 <- C5.text.tibble %>% 
  unnest_tokens(
    input = value, 
    output = skipgram, 
    token = 'skip_ngrams', 
    n = skip.window
  ) %>% 
  filter(! is.na(skipgram))

skip.gram.words.c5 %>% 
  select(skipgram) %>% 
  slice(10:20)

require(ngram)
skip.gram.words.c5$num_words <- skip.gram.words.c5$skipgram %>% 
  map_int(.f = ~ ngram::wordcount(.x))

skip.gram.words.c5 %<>% filter(num_words == 2) %>% select(- num_words)

skip.gram.words.c5 %<>% 
  separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
  filter(! word1 %in% stopwords.df$word) %>% 
  filter(! word2 %in% stopwords.df$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) 

skip.gram.count.c5 <- skip.gram.words.c5  %>% 
  count(word1, word2, sort = TRUE) %>% 
  rename(weight = n)


skip.gram.count.c5 %>% head()

######


network.c5.skip <-  skip.gram.count.c5 %>%
  filter(weight > threshold) %>%
  graph_from_data_frame(directed = FALSE)

# Select biggest connected component.  
V(network.c5.skip)$cluster <- clusters(graph = network.c5.skip)$membership

clust.network.c5.skip <- induced_subgraph(
  graph = network.c5.skip,
  vids = which(V(network.c5.skip)$cluster == which.max(clusters(graph = network.c5.skip)$csize))
)

# Store the degree.
V(clust.network.c5.skip)$degree <- strength(graph = clust.network.c5.skip)
# Compute the weight shares.
E(clust.network.c5.skip)$width <- E(clust.network.c5.skip)$weight/max(E(clust.network.c5.skip)$weight)

# Create networkD3 object.
network.D3.c5.skip <- igraph_to_networkD3(g = clust.network.c5.skip)
# Define node size.
network.D3.c5.skip$nodes %<>% mutate(Degree = (1E-2)*V(clust.network.c5.skip)$degree)
# Degine color group (I will explore this feature later).
network.D3.c5.skip$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3.c5.skip$links$Width <- 10*E(clust.network.c5.skip)$width

forceNetwork(
  Links = network.D3.c5.skip$links, 
  Nodes = network.D3.c5.skip$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)
###
comm.det.obj <- cluster_louvain(
  graph = c5.clust.net, 
  weights = E(c5.clust.net)$weight
)

V(c5.clust.net)$membership <- membership(comm.det.obj)

# Create networkD3 object.
network.D3.col.c5 <- igraph_to_networkD3(g = network.c5)
# Define node size.
network.D3.col.c5$nodes %<>% mutate(Degree = (1E-2)*V(network.c5)$degree)
# Degine color group (I will explore this feature later).
network.D3.col.c5$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3.col.c5$links$Width <- 10*E(network.c5)$width


network.D3.col.c5$nodes$Group <- V(c5.clust.net)$membership

forceNetwork(
  Links = network.D3.col.c5$links, 
  Nodes = network.D3.col.c5$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)
