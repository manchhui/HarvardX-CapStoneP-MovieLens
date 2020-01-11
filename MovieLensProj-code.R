################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(dslabs)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################
# Start of Project Code #
################################

#Basic Exploration
edxrows <- dim(edx)[1]
edxcols <- dim(edx)[2]
n_movies <- n_distinct(edx$movieId)
n_users <- n_distinct(edx$userId)
n_genres <- n_distinct(edx$genres)
edxtrows <- head(edx)
save(edxrows, file = "rda/edxrows.rda")
save(edxcols, file = "rda/edxcols.rda")
save(n_movies, file = "rda/n_movies.rda")
save(n_users, file = "rda/n_users.rda")
save(edxtrows, file = "rda/edxtrows.rda")

#Indepth Exploration
p1 <- edx %>% 
  group_by(movieId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  labs(title="Movies (edx dataset)", x="No. Of Ratings Per Movie", y="Count")

p2 <- edx %>% 
  group_by(userId) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  labs(title="Users (edx dataset)", x="No. Of Ratings Per User", y="Count")

p3 <- edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x) + 
  scale_y_continuous(limits = c(1.5,5)) +
  labs(title="Timestamp (edx dataset)", x="Date", y="Rating")

p4 <- edx %>% 
  group_by(genres) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  labs(title="Genres (edx dataset)", x="No. Of Ratings Per Genre", y="Count")
save(p1, file = "rda/p1.rda")
save(p2, file = "rda/p2.rda")
save(p3, file = "rda/p3.rda")
save(p4, file = "rda/p4.rda")

# Creat training and test set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.20, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId, movieId and genres in test_set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "genres")

# Add rows removed from test_set back into train_set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#RMSE Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Choosing the penalty term
lambdas <- seq(0, 10, 0.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres, userId) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  print(l) #Added to monitor progress
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by= c('userId','genres'))
  predicted_ratings$b_g[is.na(predicted_ratings$b_g)] = 0
  predicted_ratings <- predicted_ratings %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

p5 <- qplot(lambdas, rmses)
save(lambdas, file = "rda/lambdas.rda")
save(rmses, file = "rda/rmses.rda")
save(p5, file = "rda/p5.rda")

lambda <- lambdas[which.min(rmses)]
save(lambda, file = "rda/lambda.rda")

#Calculate estimates with the best lambda
mu <- mean(train_set$rating)
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda))
b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres, userId) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))

#Check accuracy against the validation database
predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by= c('userId','genres'))
predicted_ratings$b_g[is.na(predicted_ratings$b_g)] = 0
predicted_ratings <- predicted_ratings %>% 
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
RMSE_Final <- RMSE(predicted_ratings, validation$rating)
RMSE_Final
save(RMSE_Final, file = "rda/RMSE_Final.rda")
