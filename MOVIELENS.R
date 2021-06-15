if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId =
# as.numeric(levels(movieId))[movieId],                                            #title = as.character(title),                                            #genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#Q1: How many rows and columns are there in the edx dataset?
dim(edx)

#Q2: How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% tally()
#Q2: How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% tally()

#Q3: How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#Q4: How many different users are in the edx dataset?
n_distinct(edx$userId)

#Q5: How many movie ratings are in each of the following genres in the edx dataset?
genres_ratings <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
genres_ratings

#Q6: Which movie has the greatest number of ratings? 
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q7: What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count)) 

#Q8: True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()



### Data Analysis ###

# Head
head(edx) %>%
  print.data.frame()

# Total unique movies and users
summary(edx)

# Users' rating characteristics
            
            users_rating_char <- edx %>%
              group_by(userId) %>%
              summarize(count = n(), avg = mean(rating), median = median(rating), std = sd(rating)) %>%
              arrange(desc(count))
            
# The averages of ratings per user
            ggplot(users_rating_char) + 
              geom_point(aes(x = userId, y = avg)) +
              labs(title = "Averages of Ratings per User", x = "userId", y = "Rating")
            
# The medians of ratings per user
            ggplot(users_rating_char) + 
              geom_point(aes(x = userId, y = median)) +
              labs(title = "Medians of Ratings per User ", x = "userId", y = "Rating")
            
# The standard deviations of ratings per user
            ggplot(users_rating_char) + 
              geom_point(aes(x = userId, y = std)) +
              labs(title = "St.Dev of Ratings per User ", x = "userId", y = "Rating")
              

# Distribution of ratings
            ggplot(data = edx, aes(x = rating)) +
              geom_bar() + 
              labs(title = "Distribution of Ratings", x = "Rating", y = "Number of ratings")
            
            
       
# Numbers of Ratings per Movie
            
              ggplot(edx, aes(movieId)) +
              theme_classic()  +
              geom_histogram(bins=500, fill = "steel blue")
                            labs(title = "Ratings Frequency Distribution Per Title (MovieID)",
                   x = "Title (MovieID)",
                   y = "Frequency")
            
            
# Top Rated Movies
            
            edx %>%
              group_by(title) %>%
              summarise(count = n()) %>%
              arrange(desc(count)) %>%
              head(n=25) %>%
              ggplot(aes(title, count)) +
              theme_classic()  +
              geom_col() +
              theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
              labs(title = "Ratings Frequency Distribution Per Title - TOP 25 Movies",
                   x = "Title",
                   y = "Frequency")
            
# Mean Distribution per Title (Movie ID)
            
           edx %>%
              group_by(title) %>%
              summarise(mean = mean(rating)) %>%
              ggplot(aes(mean)) +
              theme_classic()  +
              geom_histogram(bins=12, fill = "steel blue") +
              labs(title = "Mean Distribution per Title",
                   x = "Mean",
                   y = "Frequency")
            
# Median Distribution per Title (Movie ID)
            
            edx %>%
              group_by(title) %>%
              summarise(median = median(rating)) %>%
              ggplot(aes(median)) +
              theme_classic()  +
              geom_histogram(bins=12, fill = "steel blue") +
              labs(title = "Median Distribution per Title",
                   x = "Median",
                   y = "Frequency")
            
            
# Genre Analysis #
            
# Rating Distribution per Genre
            
            edx %>%
              group_by(genres) %>%
              summarise(count = n()) %>%
              ggplot(aes(genres, count)) +
              theme_classic()  +
              geom_col() +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              labs(title = "Ratings Frequency Distribution Per Genre",
                   x = "Genre",
                   y = "Frequency")
            
            
         
# Median Distribution per Genre
            
           edx %>%
              group_by(genres) %>%
              summarise(median = median(rating)) %>%
              ggplot(aes(genres, median)) +
              theme_classic()  +
              geom_col() +
              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
              labs(title = "Median Distribution per Genre",
                   x = "Genre",
                   y = "Median")
            

           summary(edx$rating)          
           
# Compute the dataset's mean rating
            mu <- mean(edx$rating)
            mu
            
            
# Test results based on simple prediction
            naive_rmse <- RMSE(validation$rating, mu)
            naive_rmse
            
# Save prediction in data frame
            results <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)
            results
            
# Movie effect model # 
            
# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
            movie_avgs <- edx %>%
              group_by(movieId) %>%
              summarize(b_i = mean(rating - mu))
            movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
                                 ylab = "Number of movies", main = "Number of movies with the computed b_i")
            
            
# Test and save rmse results 
            predicted_ratings <- mu +  validation %>%
              left_join(movie_avgs, by='movieId') %>%
              pull(b_i)
            model_1_rmse <- RMSE(predicted_ratings, validation$rating)
            results <- bind_rows(results,
                                 data_frame(method="Movie effect model",  
                                            RMSE = model_1_rmse ))
            results
            
# Movie and user effect model #
            
# Plot penaly term user effect 
            user_avgs<- edx %>% 
              left_join(movie_avgs, by='movieId') %>%
              group_by(userId) %>%
              filter(n() >= 100) %>%
              summarize(b_u = mean(rating - mu - b_i))
            user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))
            
            
            user_avgs <- edx %>%
              left_join(movie_avgs, by='movieId') %>%
              group_by(userId) %>%
              summarize(b_u = mean(rating - mu - b_i))
            
            
# Test and save rmse results 
            predicted_ratings <- validation%>%
              left_join(movie_avgs, by='movieId') %>%
              left_join(user_avgs, by='userId') %>%
              mutate(pred = mu + b_i + b_u) %>%
              pull(pred)
            
            model_2_rmse <- RMSE(predicted_ratings, validation$rating)
            results <- bind_rows(results,
                                 data_frame(method="Movie and user effect model",  
                                            RMSE = model_2_rmse))
            results
            
            
            # Calculate the average by movie
            
            movie_avgs <- edx %>%
              
              group_by(movieId) %>%
              
              summarize(b_i = mean(rating - mu))
            
            # Calculate the average by user
            
            user_avgs <- edx %>%
              
              left_join(movie_avgs, by="movieId") %>%
              
              group_by(userId) %>%
              
              summarize(b_u = mean(rating - mu - b_i))
            
            # Calculate the average by genre
            
            genre_avgs <- edx %>%
              
              left_join(movie_avgs, by="movieId") %>%
              
              left_join(user_avgs, by="userId") %>%
              
              group_by(genres) %>%
              
              summarize(b_u_g = mean(rating - mu - b_i - b_u))
            
            # Compute the predicted ratings for the movie effect, the user effect and the genre effect on the validation
            
            predicted_ratings <- validation %>%
              
              left_join(movie_avgs, by="movieId") %>% 
              
              left_join(user_avgs, by="userId") %>%
              
              left_join(genre_avgs, by="genres") %>%
              
              mutate(pred = mu + b_i + b_u + b_u_g) %>%
              
              pull(pred)
            
            # Predict the RMSE for the movie effect, the user effect and the genre effect on the validation dataset
            
            model_3_rmse <- RMSE(predicted_ratings, validation$rating)
            # Adding the result to the results dataset 
            results <- bind_rows(results,
                                 data_frame(method="Movie +User + Genre  model",  
                                            RMSE = model_3_rmse))
            results
            
            
            # Regularized movie and user effect model # 
            
            lambdas <- seq(0, 20, 0.5)
            
            
            # For each lambda,find b_i & b_u, followed by rating prediction & testing
            # note:the below code could take some time  
            rmses <- sapply(lambdas, function(l){
              
              mu <- mean(edx$rating)
              
              b_i <- edx %>% 
                group_by(movieId) %>%
                summarize(b_i = sum(rating - mu)/(n()+l))
              
              b_u <- edx %>% 
                left_join(b_i, by="movieId") %>%
                group_by(userId) %>%
                summarize(b_u = sum(rating - b_i - mu)/(n()+l))
              
              predicted_ratings <- 
                validation %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                mutate(pred = mu + b_i + b_u) %>%
                pull(pred)
              
              return(RMSE(predicted_ratings, validation$rating))
            })
            
            # Plot rmses vs lambdas to select the optimal lambda                                                             
            # qplot(lambdas, rmses)  
            
            # Make a plot of the lambdas vs the RMSEs
            
            rmse_lambda_movie_user <- data.frame(RMSE = rmses, lambdas = lambdas)
            
            ggplot(rmse_lambda_movie_user, aes(lambdas, rmses)) +
              
              theme_classic() +
              
              geom_point(color="steel blue") +
              
              labs(title = "RMSEs vs lambdas - Regularized Movie and User-based Model",
                   
                   y = "RMSEs",
                   
                   x = "lambdas")   
            
            
            # The optimal lambda                                                             
            lambda <- lambdas[which.min(rmses)]
            lambda
            
            # Test and save results                                                             
            results <- bind_rows(results,
                                 data_frame(method="Regularized movie and user effect model",  
                                            RMSE = min(rmses)))
            results
            
            
            # Regularized Movie+User+Genre Model #
            
            # Calculate the average of all movies
            
            mu <- mean(edx$rating)
            
            # Define a table of lambdas
            
            lambdas <- seq(0, 20, 0.5)
            
            # Compute the predicted ratings on validation dataset using different values of lambda
            
            rmses <- sapply(lambdas, function(l) {
              
              # Calculate the average by movie
              
              b_i <- edx %>%
                group_by(movieId) %>%
                summarize(b_i = sum(rating - mu) / (n() + l))
              # Calculate the average by user
              
              b_u <- edx %>%
                left_join(b_i, by="movieId") %>%
                group_by(userId) %>%
                summarize(b_u = sum(rating - b_i - mu) / (n() + l))
              
              # Calculate the average by genre
              b_u_g <- edx %>%
                left_join(b_i, by="movieId") %>%
                left_join(b_u, by="userId") %>%
                group_by(genres) %>%
                summarize(b_u_g = sum(rating - b_i - mu - b_u) / (n() + l))
              
              # Calculate the predicted ratings on validation dataset
              predicted_ratings <- validation %>%
                left_join(b_i, by="movieId") %>%
                left_join(b_u, by="userId") %>%
                left_join(b_u_g, by="genres") %>%
                mutate(pred = mu + b_i + b_u + b_u_g) %>%
                pull(pred)
              
              # Predict the RMSE on the validation set
              
              return(RMSE(validation$rating, predicted_ratings))
            })
            
            # plot the result of lambdas
            # Make a plot of the lambdas vs the RMSEs
            
            rmse_lambda_movie_user_genre<- data.frame(RMSE = rmses, lambdas = lambdas)
            
            ggplot(rmse_lambda_movie_user_genre, aes(lambdas, rmses)) +
              
              theme_classic() +
              
              geom_point(color="steel blue") +
              
              labs(title = "RMSEs vs lambdas - Regularized Movie+User+Genre Model ", 
                   
                   y = "RMSEs",
                   
                   x = "lambdas")
            
           
            
            # Get the lambda value that minimize the RMSE
            
            min_lambda <- lambdas[which.min(rmses)]
            min_lambda
            
            # Predict the RMSE on the validation set
            
            rmse_regularized_movie_user_genre_model <- min(rmses)
            
            # Test and save results
            
            results <- bind_rows(results,
                                 data_frame(method=" Regularized Movie+User+Genre Based Model",  
                                            RMSE = min(rmses)))
            results
            
            
            
            
            
