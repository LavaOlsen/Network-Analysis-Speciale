library(igraph)
library(readr)
library(readxl)

set.seed(123)
#V1 <- sample(LETTERS[1:10], size = 10, replace = TRUE)
#V2 <- sample(1:10, size = 10, replace = TRUE)

#Load the data
data <- read_excel("Distance til 3 nodes 20 forsendelser PivotLonger.xlsx")


dat1 <- data[data$Type == 'A',] #Type = A
dat2 <- data[data$Type == 'M',] #Type = M

#Get the vector of pickup --> Terminal
v1p <- c(dat1$Afsenderpostnr,rep('OD',20))
v2p <- c(rep('OD',20),dat2$Afsenderpostnr)
distances <- c(dat1$FCODkm,dat2$FCODkm)

#Get the vector of Terminal --> delivery
v1p <- c(v1p,dat1$Afsenderpostnr,rep('AR',20))
v2p <- c(v2p,c(rep('AR',20),dat2$Afsenderpostnr))
distances <- c(distances,c(dat1$FCARkm,dat2$FCARkm))

#Combine and add weights
d <- data.frame(V1 = v1p, V2 = v2p, weight = distances)
d

#Create the graph data object based on the dataframe
g <- graph_from_data_frame(d, directed = FALSE)
V(g)$label <- V(g)$name # set labels.

# set type - these are merely used for plotting
V(g)$type <- 1 #Start / slut
V(g)[16:17]$type <- 2 #Terminal

V(g)$type

#Colors for the plot
col <- c("steelblue", "orange", "green") #Green and circle is for a type 3 if we want to add that
shape <- c("circle", "square", "circle")

#Create vectors to nicely arrange nodes in columns
V(g)$x <- c(rep(1,15),2,2,rep(3,16))
V(g)$y <- c(seq(1,15,1),3,13,seq(1,16,1))

#Get latitude and longitude for spatial plotting
  
  #Create empty vectors
  x <- c()
  y <- c()
  
  #Loop over the labels for the vertices
  for (i in 1:length(V(g)$label)) {
    #Vertice / node for the current iteration
    node <- V(g)$label[i]
    #Lookup the node in the table to find lat and lon
    x[i] <- as.numeric(data[data$Afsenderpostnr == node,5][1,])
    y[i] <- as.numeric(data[data$Afsenderpostnr == node,4][1,])
  }
  
  #Manually insert locations for the terminals
  x[16:17] <- c(10.399862,10.059749)
  y[16:17] <- c(55.386985,56.160736)
  
  #Insert lat and lon as x and y values
  V(g)$x <- x
  V(g)$y <- y


### PLOTTINGGGG

library(raster)
library(ggplot2)
  
#This is a colorfull map
denmark  <- getData("GADM",country="Denmark",level=2)
ggplot(denmark,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=id),color="grey30")+
  coord_map(xlim=c(-0.7,0)+bbox(denmark)["x",],ylim=c(0,0)+bbox(denmark)["y",])+
  scale_fill_discrete(guide="none")+
  theme_bw()+theme(panel.grid=element_blank())


#This is the plot
plot.igraph(x = g, #The graph
            vertex.color = col[V(g)$type], #Define colors type 1 = steelblue etc.
            vertex.shape = shape[V(g)$type],
            edge.width = E(g)$weight / 100 #Divide by 100 to decrease width of the edges
            ,axes = T
            )

distances(
  g,
  v = V(g),
  to = V(g),
  mode = c("all", "out", "in"),
  weights = NULL,
  algorithm = c("dijkstra")
)

mean_distance(graph = g)



###### I THINK THIS IS JUST FOR SOME CENTERING FROM STACKOVERFLOW

# create a graph connecting central node FOO to each V2.
e <- expand.grid(V2 = unique(d$V2), V2 = "OD1")

g2 <- graph.data.frame(e, directed = T)

# join the two graphs.
g <- g + g2

# set type.
V(g)$type <- 1
V(g)[name %in% 1:10]$type <- 2
V(g)[name %in% "OD1"]$type <- 3

V(g)$type


col <- c("steelblue", "orange", "green")
shape <- c("circle", "square", "circle")

#library(rTRM) # Bioconductor package containing layout.concentric()
# the fist element in the list for concentric is the central node.
#l <- layout.concentric(g, concentric = list("FOO", 1:10, LETTERS[1:10]))
plot(g,
     #layout = l,
     vertex.color = col[V(g)$type],
     vertex.shape = shape[V(g)$type],
     edge.width = E(g)$weights * 10# * 5 # optional, plot edges width proportional to weights.
)


