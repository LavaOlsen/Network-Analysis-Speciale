

df <- read.csv("PostNrLatLon.csv",sep = ";")
df <- df[df$Postnr > 1999,]

v = paste(df$Latitude,df$Longitude,sep = "+")

w <- expand.grid(v,v)
w$round <- rep(seq(from = 1,to = 6),nrow(w)/6)


m <- matrix(nrow = 10,ncol = 10)

rn <- v[c(1:ncol(m))]
cn <- v[c(1:ncol(m))]c(1:nrow(m))

rownames(m) <- rn
colnames(m) <- rownames(m)

for (i in 1:nrow(m)) {
  
  r <- rn[i]
  
  for (i2 in 1:ncol(m)) {
    cn <- cn[i2]
    
    if (i == i2) {
      m[i,i2] <- 0
    } else if (i > i2) {
      m[i,i2] <- 0
    } else {
      
      #Print lookup
      print(paste(rownames(m)[i],colnames(m)[i2],sep = "-->"))
      
      origin <- rownames(m)[i]
      destination <- colnames(m)[i2]
      
      #Get distance
      results = gmapsdistance(origin = origin
                              ,destination = destination
                              ,mode = "driving"
      )
      
      #Store result
      m[i,i2] <- results$Distance
      
      }
  }
  
}

saveRDS(m,file = "Files/TestDistanceMatrix.rds")


