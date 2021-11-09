

#devtools::install_github("rodazuero/gmapsdistance")
library(gmapsdistance)
source("Keys.R")

results = gmapsdistance(origin = "Washington+DC"
                        ,destination = "New+York+City+NY"
                        ,mode = "driving"
                        )
results
