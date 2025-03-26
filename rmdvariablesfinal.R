#Final R file
library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(forcats)
library(tidytext)
library(httr)
library(jsonlite)
library(caret)
library(randomForest)
library(nnet)
library(doParallel)
library(tm)
library(SnowballC)
library(FactoMineR)
cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)


#importing data 
london_data<-read_csv("https://data.insideairbnb.com/united-kingdom/england/london/2024-12-11/data/listings.csv.gz")
london_calendar<-read_csv("https://data.insideairbnb.com/united-kingdom/england/london/2024-12-11/data/calendar.csv.gz")


#temporarily importing dataset

##formatting data
#converting price to numeric
london_data$price <- as.numeric(gsub("\\$", "", london_data$price))

#converting variables measured in percentage to numeric
london_data <- london_data %>%
  mutate(host_response_rate = as.numeric(gsub("%", "", host_response_rate)) / 100)
london_data <- london_data %>%
  mutate(host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)) / 100)

#converting date variables to correct format and measuring days since first listing
london_data$host_since <- as.Date(london_data$host_since, format = "%Y-%m-%d")
london_data$days_since <- as.numeric(Sys.Date() - london_data$host_since)
range(london_data$host_since)

london_data$last_scraped <- as.Date(london_data$last_scraped, format = "%Y-%m-%d")
range(london_data$last_scraped, na.rm = TRUE)
summary(london_data$last_scraped)

#coding binary variables
london_calendar$available <- as.numeric(london_calendar$available == "t")
london_data$host_identity_verified <- as.numeric(london_data$host_identity_verified == "t")
london_data$host_has_profile_pic <- as.numeric(london_data$host_has_profile_pic == "t")
london_data$host_is_superhost <- as.numeric(london_data$host_is_superhost == "t")


#investigating price to find that a third of data is missing
sum(is.na(london_data$price))



#introducing logit model to check if any variables in the data set can predict price being missing

#create data set with data filtered for observations with price missing
london_data_clean <- london_data %>% filter(!is.na(price))



#exploratory data analysis
#Begin with numerical variables
#split data into smaller data set to better visualise scatter plots
sample_data <- london_data %>% sample_frac(0.1)
#remove data where price is missing
sample_data_clean <- sample_data %>% filter(!is.na(price))

num_vars <- c("review_scores_rating", "review_scores_accuracy","review_scores_location", "review_scores_communication", "review_scores_cleanliness", "review_scores_checkin", "review_scores_value","host_total_listings_count", "host_response_rate", "host_acceptance_rate")
sample_data_long <- sample_data_clean %>%
  select(price, all_of(num_vars)) %>%
  pivot_longer(cols = all_of(num_vars), names_to = "Variable", values_to = "Value")

scatter_plot_numvars<-ggplot(sample_data_long, aes(x = Value, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Scatter Plots of Price Against Numerical Variables",
       x = "Variable Value",
       y = "Price (£)")

print(scatter_plot_numvars)

#checking correlation matrix
# Add price to list
vars_for_correlation <- c("price", num_vars)
cor_data <- london_data_clean %>%
  select(all_of(vars_for_correlation)) %>%
  drop_na()
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_price <- cor_matrix["price", -which(colnames(cor_matrix) == "price")]
print(round(cor_price, 3))


#plotting barcharts for average price across levels of other variables
#begin with variables relating to host capacity
#making bar charts based on averages of variables related to capacity
capacity_vars<-c("accommodates", "bedrooms", "bathrooms", "beds")
# Convert categorical variables to factors
london_data_clean <- london_data_clean %>%
  mutate(across(all_of(capacity_vars), ~ factor(as.numeric(as.character(.)))))

# Pivot longer to one column of variable names and one of values
capacity_data <- london_data_clean %>%
  pivot_longer(cols = all_of(capacity_vars), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

barchart_data <- barchart_data %>%
  mutate(Category_num = as.numeric(as.character(Category))) %>%   
  arrange(Variable, Category_num) %>%                              
  mutate(Category = factor(as.character(Category_num), 
                           levels = sort(unique(Category_num))))

barplot_capacity <- ggplot(barchart_data, aes(x = Category, y = avg_price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "Average Airbnb Price by Capacity Variables",
       x = "Category", y = "Average Price (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#making bar plot to check for potential time variation
#bar chart to show date variation
bar_vars_date<-"last_scraped"

barchartdate_data <- london_data_clean %>%
  pivot_longer(cols = all_of(bar_vars_date), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")

# Plot as faceted bar chart
barplot2<-ggplot(barchartdate_data, aes(x = fct_reorder(Category, avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "Average Airbnb Price by Date", x = "Category", y = "Average Price (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

plot(barplot2)

#checking for location features through heat map
# Calculate mean price per borough
# Average price per borough
borough_prices <- london_data %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

london_map <- st_read("https://data.insideairbnb.com/united-kingdom/england/london/2024-12-11/visualisations/neighbourhoods.geojson")
london_map <- london_map %>%
  left_join(borough_prices, by = c("neighbourhood"="neighbourhood_cleansed"))  

#plot map
mapplot <- ggplot(london_map) +
  geom_sf(aes(fill = avg_price), color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Avg Price (£)") +  
  theme_minimal() +
  labs(title = "Average Airbnb Price per Borough in London",
       subtitle = "Data from Airbnb Listings")

#analysing text data
#define stemming function
clean_text <- function(text_column) {
  corpus <- VCorpus(VectorSource(text_column))  
  corpus <- tm_map(corpus, content_transformer(tolower)) 
  corpus <- tm_map(corpus, removePunctuation)  
  corpus <- tm_map(corpus, removeNumbers)  
  corpus <- tm_map(corpus, removeWords, stopwords("en"))  
  corpus <- tm_map(corpus, stemDocument) 
  cleaned_text <- sapply(corpus, as.character)
  return(cleaned_text)
  return(corpus)
}

#apply stemming function to columns of interest
london_data$description_clean <- clean_text(london_data$description)
london_data$name_clean <- clean_text(london_data$name)
london_data$amenities_clean <- clean_text(london_data$amenities)



#create correlation matrix for each column and examine most common words for each column
# Create a text corpus from name_clean
corpus_name <- VCorpus(VectorSource(london_data$name_clean))

# Convert to Document-Term Matrix (TF-IDF)
dtm_name <- DocumentTermMatrix(corpus_name, control = list(weighting = weightTfIdf))

# Convert to DataFrame
dtm_name_df <- as.data.frame(as.matrix(dtm_name))

# Add price column
dtm_name_df$price <- london_data$price

# Compute correlations
# Compute correlations while ensuring column names are preserved
correlations_name <- cor(dtm_name_df[, -which(names(dtm_name_df) == "price")], 
                         dtm_name_df$price, 
                         use = "pairwise.complete.obs")

# Assign column names as names for the vector (if missing)
names(correlations_name) <- colnames(dtm_name_df)[-which(names(dtm_name_df) == "price")]

# Now, sort and extract top words
correlations_sorted_name <- sort(correlations_name, decreasing = TRUE)

# Extract top words
top_words <- names(correlations_sorted_name)[1:50]
print(top_words)


#word cloud
library(wordcloud)

# Create word cloud
png("H:/data science/namewordcloud.png", width = 12, height = 9, units = "in", res = 300)
wordcloud(words = top_words, freq = correlations_sorted_name[1:50], colors = "blue")
dev.off()

#same for amenities
# Create a text corpus from name_clean
corpus_amenities <- VCorpus(VectorSource(london_data$amenities_clean))

# Convert to Document-Term Matrix (TF-IDF)
dtm_amenities <- DocumentTermMatrix(corpus_amenities, control = list(weighting = weightTfIdf))

# Convert to DataFrame
dtm_amenities_df <- as.data.frame(as.matrix(dtm_amenities))

# Add price column
dtm_amenities_df$price <- london_data$price

# Compute correlations
# Compute correlations while ensuring column names are preserved
correlations_amenities <- cor(dtm_amenities_df[, -which(names(dtm_amenities_df) == "price")], 
                              dtm_amenities_df$price, 
                              use = "pairwise.complete.obs")

# Assign column names as names for the vector (if missing)
names(correlations_amenities) <- colnames(dtm_amenities_df)[-which(names(dtm_amenities_df) == "price")]

# Now, sort and extract top words
correlations_sorted_amenities <- sort(correlations_amenities, decreasing = TRUE)

# Extract top words
top_words_amenities <- names(correlations_sorted_amenities)[1:50]
print(top_words_amenities)


#word cloud
library(wordcloud)
# Select top 50 most correlated words
top_words_amenities <- names(correlations_sorted_amenities[1:50])
# Create word cloud
png("H:/data science/amenitieswordcloud.png", width = 12, height = 8, units = "in", res = 300)
wordcloud(words = top_words_amenities, freq = correlations_sorted_amenities[1:50], colors = "blue")
dev.off()

#same for description
# Create a text corpus from name_clean
corpus_description <- VCorpus(VectorSource(london_data$description_clean))

# Convert to Document-Term Matrix (TF-IDF)
dtm_description <- DocumentTermMatrix(corpus_description, control = list(weighting = weightTfIdf))
dtm_description <- removeSparseTerms(dtm_description, 0.99)

# Convert to DataFrame
dtm_description_df <- as.data.frame(as.matrix(dtm_description))

# Add price column
dtm_description_df$price <- london_data$price

# Compute correlations
# Compute correlations while ensuring column names are preserved
correlations_description <- cor(dtm_description_df[, -which(names(dtm_description_df) == "price")], 
                                dtm_description_df$price, 
                                use = "pairwise.complete.obs")

# Assign column names as names for the vector (if missing)
names(correlations_description) <- colnames(dtm_description_df)[-which(names(dtm_description_df) == "price")]

# Now, sort and extract top words
correlations_sorted_description <- sort(correlations_description, decreasing = TRUE)

# Extract top words
top_words_description <- names(correlations_sorted_description)[1:50]
print(top_words_description)


#word cloud
library(wordcloud)
# Select top 50 most correlated words
top_words_description <- names(correlations_sorted_description[1:50])
# Create word cloud
wordcloud(words = top_words_description, freq = correlations_sorted_description[1:50], colors = "blue")
print(top_words_description)


#feature engineering
#location features: comparing listings with proxy for central London (Big Ben), gps coords. '-0.116773, 51.510357'
big_ben_coords <- c(-0.116773, 51.510357)

london_data <- london_data %>%
  mutate(
    lat = as.numeric(latitude),
    long = as.numeric(longitude)
  ) %>%
  filter(!is.na(lat) & !is.na(long))

# Calculate distances (in meters)
london_data$dist_to_big_ben_m <- distHaversine(
  cbind(london_data$long, london_data$lat),  # matrix of listing coords
  big_ben_coords
)

#converting to km
london_data$dist_to_big_ben_km <- london_data$dist_to_big_ben_m / 1000

#testing for negative correlation
cor(london_data$price, london_data$dist_to_big_ben_km, use='pairwise.complete.obs')

#location features: incorporating transport amenities through measuring distance of each listing to nearest tube station
# Define the URL for tube stops from the TfL API
url <- "https://api.tfl.gov.uk/StopPoint/Mode/tube"
# Query the API and get the raw response text
response <- GET(url)
raw_text <- content(response, as = "text", encoding = "UTF-8")
# Parsing response
data <- fromJSON(raw_text, flatten = TRUE)
# Extract the tube station data from the "stopPoints" field
tube_stations <- as.data.frame(data$stopPoints)
# Select only the relevant columns
tube_stations <- tube_stations[, c("id", "commonName", "lat", "lon")]
#create a loop which is going to create a closest tube station as well as distance in kilometers from station
# Convert london_data to an sf object (assuming it has 'longitude' and 'latitude')
london_sf <- st_as_sf(london_data, coords = c("longitude", "latitude"), crs = 4326)
tube_sf <- st_as_sf(tube_stations, coords = c("lon", "lat"), crs = 4326)
london_sf <- st_transform(london_sf, 27700)
tube_sf <- st_transform(tube_sf, 27700)
nearest_idx <- st_nearest_feature(london_sf, tube_sf)
london_data<- london_sf %>%
  mutate(nearest_tube = tube_sf$commonName[nearest_idx])

# Calculate the distance (in meters) to the nearest tube station
london_data<- london_sf %>%
  mutate(distance_to_tube = as.numeric(st_distance(., tube_sf[nearest_idx, ], by_element = TRUE)))

#checking correlation
cor(london_data$price,london_data$distance_to_tube, use='pairwise.complete.obs')

#vacancy rate from calendar data
london_data <- london_data %>%
  rename(listing_id = id)

london_calendar <- london_calendar %>%
  group_by(listing_id) %>%
  mutate(vacant_ratio = mean(1 - available, na.rm = TRUE))

london_data$vacant_ratio <- london_calendar$vacant_ratio[match(london_data$listing_id, london_calendar$listing_id)]

#checking for correlation
cor(london_data$price, london_data$vacant_ratio, use='pairwise.complete.obs')

#coding measure of bathrooms per person
london_data$bathroomperperson<- (london_data$bathrooms/london_data$accommodates)

cor(london_data$price, london_data$bathroomperperson, use = "pairwise.complete.obs")
summary(london_data$bathroomperperson)

#coding binary variable for excess beds
london_data$excessbeds<-ifelse((london_data$beds>london_data$bedrooms), 1, 0)
summary(london_data$excessbeds)

#checking for relationship
london_data %get>%
  group_by(excessbeds) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

##text features
#summing correlations for each listing to build overall measure of 'important words'

word_cols <- c(top_words, top_words_amenities, top_words_description)
london_data$important_words_score <- rowSums(dtm_name_df[, top_words]) +
  rowSums(dtm_amenities_df[, top_words_amenities]) +
  rowSums(dtm_description_df[, top_words_description])
#checking for correlation
cor(london_data_clean$price, london_data_clean$important_words_score)
