library(readr)
library(dplyr)
library(stringr)
library(highcharter)


movies <- 
  read_delim("ml-10M100k/movies.dat", delim = "::", col_names = c("movie_id", NA, "movie_title", NA, "movie_genres")) %>% 
  select(movie_id, movie_title, movie_genres)

# data.frame(movie_id=c(NA), movie_title=c(NA), movie_genre=c(NA)) -> genre_movies
# movie_count <- dim(movies)[1]
# for (i in 1:movie_count) {
#   movies[i,] %>% .$movie_genres %>%  str_extract_all("\\w+") %>% .[[1]] -> movie_genres
#   movies[i,] %>% .$movie_id -> movie_id
#   movies[i,] %>% .$movie_title -> movie_title
#   for (movie_genre in movie_genres) {
#     data.frame(movie_id=c(movie_id), movie_title=c(movie_title), movie_genre=c(movie_genre)) %>% rbind(genre_movies) -> genre_movies
#   }
# }

# save(genre_movies, file = 'saved_data/genre_movies.RData')
load(file = 'saved_data/genre_movies.RData')
genre_movies <- genre_movies %>% .[complete.cases(.),]


extract_year_from_timestamp <- function(timestamp) {
  return(timeDate::timeDate(timestamp) %>% .@Data %>% format('%Y'))
}

read_delim("ml-10M100k/tags.dat", delim = "::", col_names = c("user_id", NA, "movie_id", NA, "tag", NA, "timestamp")) %>%
  select(user_id, movie_id, tag, timestamp) -> tags
tags %>% mutate(year = extract_year_from_timestamp(timestamp)) -> tags

read_delim("ml-10M100k/ratings.dat", delim = "::", col_names = c("user_id", NA, "movie_id", NA, "rating", NA, "timestamp")) %>% 
  select(user_id, movie_id, rating, timestamp) -> ratings
ratings %>% mutate(year = extract_year_from_timestamp(timestamp)) -> ratings

########################################################################################################
## Q1 - most loved movie

ratings %>% group_by(movie_id) %>% summarise(mean_rate = mean(rating, na.rm = TRUE), votes = n()) %>%
  full_join(movies, by = 'movie_id') %>% select(movie_id, mean_rate, movie_title, votes) %>% 
  .[complete.cases(.),] %>% ungroup -> q1_data

q1_data %>% filter(votes > 10000) %>% arrange(desc(mean_rate)) %>% slice(1:20) %>% print

## Q1 - most votes

q1_data %>% arrange(desc(votes)) %>% slice(1:20) %>% print

## Q1 - most hated movie

q1_data %>% filter(votes > 1000) %>% arrange(mean_rate) %>% slice(1:20) %>% print


## Q1 - movie counts each year

# ratings %>% ungroup %>% group_by(movie_id) %>% mutate(prod_year = min(year, na.rm = TRUE)) %>%
#   select(movie_id, prod_year) %>% unique -> movie_year
# save(movie_year, file = 'saved_data/movie_year.RData')
load(file = 'saved_data/movie_year.RData')

# movie_year %>% ungroup %>% group_by(prod_year) %>% summarise(movie_count = n()) %>%
#   hchart(type = 'line', hcaes(x = prod_year, y = movie_count))

get_year_from_name <- function(name) {
  return(name %>% str_extract(pattern = regex('\\(\\d{4}\\)')) %>% str_sub(2, 5))
}
movies %>% mutate(prod_year = get_year_from_name(movie_title)) -> movie_year_actual

movie_year_actual %>% select(movie_title, prod_year) %>% group_by(prod_year) %>%
  summarise(movie_count = n()) %>% .[complete.cases(.),] %>%
  hchart(type = 'line', hcaes(x = prod_year, y = movie_count))

## Q1 - popular gener each year

q1_data %>% full_join(
    movie_year %>% rename(year = 'prod_year'),
    by = 'movie_id'
  ) %>% full_join(
    genre_movies %>% select(-movie_title),
    by = 'movie_id'
  ) %>% ungroup %>% group_by(year, movie_genre) %>%
  summarise(tot_votes = sum(votes), mean_rate = sum(mean_rate * votes)/tot_votes) %>% ungroup %>% 
  filter(tot_votes > 10000) %>% group_by(year) %>% arrange(desc(mean_rate)) %>% top_n(1) %>% print


########################################################################################################
## Q2 - film count per genre

genre_movies -> q2_data
q2_data$movie_genre <- as.character(q2_data$movie_genre)

q2_data %>% group_by(movie_genre) %>% summarise(movie_count = n()) %>% filter(movie_count > 10) %>%
  arrange(desc(movie_count)) %>% mutate(movie_genre = ifelse(movie_genre == 'Fi', 'Sci-Fi', movie_genre)) %>% 
  filter(!movie_genre %in% c('Sci', 'IMAX', 'Film')) -> q2_plot_data
q2_plot_data %>% hchart(type = 'column', hcaes(x = movie_genre, y = movie_count))

## Q2 - corelation plot for genres

q2_plot_data %>% filter(movie_genre != 'Sci-Fi') %>% .$movie_genre -> main_genres
main_genres %>% length -> main_genres_count
q2_data %>% filter(movie_genre != 'Sci-Fi') %>% select(movie_title) %>% unique -> main_movies
main_movies %>% dim %>% .[1] -> main_movies_count

q2_data %>% filter(movie_genre %in% main_genres) -> q2_data
q2_data$movie_title <- as.character(q2_data$movie_title)


q2_mat_data <- data.frame(matrix(nrow = main_movies_count, ncol = main_genres_count, data = 0))
rownames(q2_mat_data) <- main_movies$movie_title
colnames(q2_mat_data) <- main_genres

for (index in 1:dim(q2_data)[1]) {
  title <- q2_data[index,][2] %>% as.character
  genre <- q2_data[index,][3] %>% as.character
  q2_mat_data[title, genre] <- 1
}

q2_mat_data %>% cor() %>% hchart()

## Q2 - average genre rating

q1_data %>% full_join(
  movie_year %>% rename(year = 'prod_year'),
  by = 'movie_id'
) %>% full_join(
  genre_movies %>% select(-movie_title),
  by = 'movie_id'
) %>% ungroup %>% group_by(movie_genre) %>%
  summarise(tot_votes = sum(votes), mean_rate = sum(mean_rate * votes)/tot_votes) %>% ungroup %>% 
  filter(tot_votes > 80000) %>% arrange(desc(mean_rate)) %>% print


## Q2 - Golden Era

movie_year_actual %>% select(movie_title, prod_year) %>% group_by(prod_year) %>%
  summarise(movie_count = n()) %>% .[complete.cases(.),] %>%
  hchart(type = 'line', hcaes(x = prod_year, y = movie_count))

movie_year_actual %>% select(prod_year, movie_id) %>% full_join(
  q1_data %>% select(-movie_title),
  by = 'movie_id'
) %>% ungroup %>% group_by(prod_year) %>% arrange(desc(mean_rate)) %>% filter(votes > 5000) %>%
  top_n(5, mean_rate) %>% ungroup %>% group_by(prod_year) %>%
  summarise(mean_top_rate = sum(mean_rate * votes)) %>% .[complete.cases(.),] -> rate_data

rate_data %>% hchart(type = 'line', hcaes(x = prod_year, y = mean_top_rate), color = 'red')


# 1990s -> 1994 maybe


########################################################################################################
## Q3

movies %>% select(movie_title) -> q3_titles

library(tidytext)
extract_words <- function(data) {
  data %>%
    str_replace_all(pattern = '"', replacement = '') %>% 
    str_replace_all(pattern = '`', replacement = '') %>%
    str_replace_all(pattern = '\'', replacement = '') %>%
    str_replace_all(pattern = '.*\\|.*', replacement = '') %>%
    str_replace_all(pattern = '\\d+', replacement = '') %>%
    str_replace_all(pattern = '�', replacement = ' ') %>% 
    str_replace_all(pattern = '[[:punct:]]', replacement = ' ') %>%
    str_split(pattern = "\\s+") %>%
    unlist() %>% table() %>% as.data.frame() %>% slice(-1) -> result
  colnames(result) <- c('word', 'count')
  return(result)
}
extract_semantic_words <- function(data) {
  data %>% extract_words() %>% filter(!(tolower(word) %in% stop_words$word)) -> result
  return(result)
}


q3_titles %>% extract_semantic_words %>% arrange(desc(count)) %>% slice(1:100) -> wordcloud_data

library(wordcloud)

wordcloud(wordcloud_data$word,wordcloud_data$count,
          c(5,.3), random.order = FALSE, colors=brewer.pal(8, "Dark2"))

########################################################################################################
## Q4

library(tidyr)

# ratings %>% select(user_id, movie_id, rating) %>% spread(key = 'movie_id', value = 'rating') -> transactions
# transactions[!is.na(transactions)] <- 1
# transactions[is.na(transactions)] <- 0
# save(transactions, file = 'saved_data/transactions.RData')
load(file = 'saved_data/transactions.RData')
transactions_matrix <- transactions %>% select(-user_id) %>% as.matrix

transactions_obj <- as(transactions_matrix[1:10000,], 'transactions')



library(arules)
library(arulesViz)
library(colorspace)

grules <- apriori(transactions_obj, parameter = list(support = 0.009, confidence = 0.25, minlen = 2, maxlen = 2))

grules %>% inspect
 
movies %>% View

# Castle in the Sky (Tenkû no shiro Rapyuta) (1986) -> 6350
subset(grules, subset = lhs %in% c('6350')) %>% sort(by = 'lift', decreasing = TRUE) %>%
  inspect
# 3000 -> Princess Mononoke (Mononoke-hime) (1997)
# 5618 -> Spirited Away (Sen to Chihiro no kamikakushi) (2001)
# 8961 -> Incredibles, The (2004)

# Cast Away (2000)                                  -> 4022
subset(grules, subset = lhs %in% c('4022')) %>% sort(by = 'lift', decreasing = TRUE) %>%
  inspect
# 5388 -> Insomnia (2002)
# 4310 -> Pearl Harbor (2001)
# 5502 -> Signs (2002)

# Memento (2000)                                    -> 4226
subset(grules, subset = lhs %in% c('4226')) %>% sort(by = 'lift', decreasing = TRUE) %>%
  inspect
# 4848 -> Mulholland Drive (2001)
# 3949 -> Requiem for a Dream (2000)
# 5902 -> Adaptation (2002)

# Shawshank Redemption, The (1994)                  -> 318
subset(grules, subset = lhs %in% c('318')) %>% sort(by = 'lift', decreasing = TRUE) %>%
  inspect
# 300  -> Quiz Show (1994)
# 1704 -> Good Will Hunting (1997)
# 508  -> Philadelphia (1993)

########################################################################################################
## Q5

########################################################################################################
## Q6

########################################################################################################
## Q7

########################################################################################################
## Q8

########################################################################################################
## Q9

########################################################################################################
## Q10

