

library(igraph)
library(keras)
library(tensorflow)
library(dplyr)


# Download movielens data and prepare the data ----

download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip",destfile = "GNN/Data/movielens.zip")
unzip(zipfile = "GNN/Data/movielens.zip",exdir = "GNN/Data/")

#Load movies
movies = read.csv("GNN/Data/ml-latest-small/movies.csv")
movies$movieId = paste0("movie_",movies$movieId)

#Load ratings
ratings = read.csv("GNN/Data/ml-latest-small/ratings.csv")
ratings$movieId = paste0("movie_",ratings$movieId)
dim(movies)
dim(ratings)

get_movie_title_by_id = function(movieId){
  return(list(movies$title[movies$movieId == movieId])[1])
}

get_movie_id_by_title = function(title){
  return(list(movies$movieId[movies$title == title])[1])
}

get_movie_title_by_id(movieId = "movie_1")
get_movie_id_by_title(title = "Toy Story (1995)")


# Construct the Movies graph ----

## Step 1 ----

# min_rating = 5
# pair_frequency = matrix()
# item_frequency = matrix()
# 
# #Filter instances where rating is greater than or equal to min_rating.
# rated_movies = ratings[ratings$rating >= min_rating,]
# #Group instances by user
# movies_grouped_by_users = list(group_by(.data = rated_movies,"userId"))
# 
# for (group in unique(rated_movies$userId)) {
#   
#   #Get list of movies rated by the user
#   current_movies = rated_movies$movieId[rated_movies$userId == group]
#   
#   for (i in 1:nrow(current_movies)) {
#     item_frequency[current_movies[i]] = item_frequency[current_movies[i]] + 1 
#   }
#   
# }

# Load tfdataset ----

library(tfdatasets)

targets = read.csv("GNN/Data/tfdataset/targets.txt",header = F)
targets = targets$V1
contexts = read.csv("GNN/Data/tfdataset/contexts.txt",header = F)
contexts = contexts$V1
labels = read.csv("GNN/Data/tfdataset/labels.txt",header = F)
# labels = labels$V1
weights = read.csv("GNN/Data/tfdataset/weights.txt",header = F)
# weights = weights$V1

batch_size = 1024

# dataset = data.frame(targets = targets
#                      ,contexts = contexts
#                      ,labels = labels
#                      ,weights = weights
#                      ,batch_size = batch_size) %>%
#   setNames(c("targets","contexts","labels","weights","batch_size"))

#str(dataset)

create_dataset = function(targets, contexts, labels, weights, batch_size){
  
  inputs = list(
    "target" = targets,
    "context" = contexts
    )
  
  dataset = tfdatasets::tensor_slices_dataset(c(inputs,labels,weights))
  dataset = tfdatasets::dataset_shuffle(dataset = dataset,buffer_size = as.integer(batch_size * 2))
  dataset = tfdatasets::dataset_batch(dataset = dataset,batch_size = batch_size,drop_remainder = T)
  #dataset = tfdatasets::dataset_prefetch(tensorflow::)
  
}

dataset = create_dataset(targets = targets,contexts = contexts
                         ,labels = labels,weights = weights
                         ,batch_size = batch_size)

dataset

# Train the skip-gram model ----

learning_rate = 0.001
embedding_dim = 50
num_epochs = 10

vocabulary_size = 1406

## Implemnting the model ----



model = keras_model_sequential() %>% 
  layer_input() %>% 
  layer_embedding(
    input_dim = vocabulary_size
    ,output_dim = embedding_dim
    ,embeddings_initializer = "he_normal"
    ,embeddings_regularizer = regularizer_l2(l = 0.000001)
    ,name = "Item_embeddings"
  ) %>% 
  layer_dot(axes = 1,normalize = F,name = "dot_similarity")
