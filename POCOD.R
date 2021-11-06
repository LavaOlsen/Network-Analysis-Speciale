library(igraph)
library(readr)
library(readxl)

#Load the data
data <- read_excel("~/GitHub/Network-Analysis-Speciale/Distance til 3 nodes 20 forsendelser PivotLonger.xlsx")

dat1 <- data[data$Type == 'A',] #Type = A
dat2 <- data[data$Type == 'M',] #Type = M

#Get the vector of pickup --> Terminal
v1p <- c(dat1$Afsenderpostnr,rep('OD',20))
v2p <- c(rep('OD',20),dat2$Afsenderpostnr)
distances <- c(dat1$FCODkm,dat2$FCODkm)

  #### REMOVED AR AS THIS IS THE LEAST IMPORTANT VERTICE FOR THE NETWORK ####
# #Get the vector of Terminal --> delivery
# v1p <- c(v1p,dat1$Afsenderpostnr,rep('AR',20))
# v2p <- c(v2p,c(rep('AR',20),dat2$Afsenderpostnr))
# distances <- c(distances,c(dat1$FCARkm,dat2$FCARkm))

#Combine and add weights
d <- data.frame(V1 = v1p, V2 = v2p, weight = distances)
d

#Create the graph data object based on the dataframe
g <- graph_from_data_frame(d, directed = FALSE)
g

#U = UNDIRECTED
#N = name Attribute N
#W = it's Weighted
#- = It's not bipartite

V(g)$label <- V(g)$name # set labels.

# set type - these are merely used for plotting
V(g)$type <- 1 #Start / slut
V(g)[16]$type <- 2 #Terminal

V(g)$type

#Colors for the plot
col <- c("steelblue", "orange", "green") #Green and circle is for a type 3 if we want to add that
shape <- c("circle", "square", "circle")

#Create vectors to nicely arrange nodes in columns
V(g)$x <- c(rep(1,15),2,rep(3,16))
V(g)$y <- c(seq(1,15,1),7,seq(1,16,1))

# #Get latitude and longitude for spatial plotting
#   
#   #Create empty vectors
#   x <- c()
#   y <- c()
#   
#   #Loop over the labels for the vertices
#   for (i in 1:length(V(g)$label)) {
#     #Vertice / node for the current iteration
#     node <- V(g)$label[i]
#     #Lookup the node in the table to find lat and lon
#     x[i] <- as.numeric(data[data$Afsenderpostnr == node,5][1,])
#     y[i] <- as.numeric(data[data$Afsenderpostnr == node,4][1,])
#   }
#   
#   #Manually insert locations for the terminals
#   x[16:17] <- c(10.399862,10.059749)
#   y[16:17] <- c(55.386985,56.160736)
#   
#   #Insert lat and lon as x and y values
#   V(g)$x <- x
#   V(g)$y <- y


#This is the plot
plot.igraph(x = g, #The graph
            vertex.color = col[V(g)$type], #Define colors type 1 = steelblue etc.
            vertex.shape = shape[V(g)$type],
            edge.width = E(g)$weight / 100 #Divide by 100 to decrease width of the edges
            ,axes = T
)



dist_matrix = distances(
  g,
  v = V(g),
  to = V(g),
  mode = c("all", "out", "in"),
  weights = NULL,
  algorithm = c("dijkstra")
)

############################
###### Mean  Distance ######
############################

mean(dist_matrix) #Denne tæller diagonalen med

#Mean distance
sum(dist_matrix) / (dim(dist_matrix)[1] * dim(dist_matrix)[2] - length(diag(dist_matrix)))
mean_distance(g) #Mean distance disregarding weights

dist_matrix = distances(
  g,
  v = V(g),
  to = V(g),
  mode = c("all", "out", "in"),
  weights = NA,
  algorithm = c("dijkstra")
)


############################
#### Centrality Indices ####
############################

options(scipen = 999)
igraph::betweenness(g) #Betweenness
sort(closeness(g),decreasing = T) #Closeness
plot(sort(closeness(g),decreasing = T)) #Closeness plotte
sort(eigen_centrality(g)$vector,decreasing = T) %>% t() %>% t() #Eigen vector centrality

