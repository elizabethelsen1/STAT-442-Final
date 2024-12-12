library(tidyverse)
library(ggplot2)


# data prep
#####
#get the big one in
library(readr)
library(dplyr)

library(lubridate) # for date and time stuff


# setup data
#####
universal_top_spotify_songs <- read_csv("universal_top_spotify_songs.csv", 
                                        col_types = cols(snapshot_date = col_datetime(format = "%Y-%m-%d"), 
                                                         album_release_date = col_datetime(format = "%Y-%m-%d"), 
                                                         key = col_factor(levels = c("0", 
                                                                                     "1", "2", "3", "4", "5", "6", 
                                                                                     "7", "8", "9", "10", "11")), 
                                                         mode = col_factor(levels = c("0", 
                                                                                      "1")), time_signature = col_factor(levels = c("0", 
                                                                                                                                    "1", "2", "3", "4", "5"))))


# adding columns 
universal_top_spotify_songs <- universal_top_spotify_songs %>%
  mutate(key_mode = paste(key, "in", mode),
         age = difftime(universal_top_spotify_songs$snapshot_date, 
                        universal_top_spotify_songs$album_release_date, 
                        units = "days") 
  ) 

universal_top_spotify_songs$age <- as.numeric(universal_top_spotify_songs$age)


universal_top_spotify_songs <- universal_top_spotify_songs %>%
  group_by(spotify_id) %>%
  filter(popularity == max(popularity)) %>%
  ungroup()


save(universal_top_spotify_songs, file = "universal_top_spotify_songs.RData")


# region assignments
latin_american <- c("MX", "SV", "GT", "HN", "NI", "CR", "PA", "DO", "VE", "CO", "EC", "PE", "BO", "CL", "AR", "PY", "UY")

portuguese_speaking <- c("BR", "PT")

east_asian <- c("JP", "KR", "TW", "VN", "HK")

southeast_asian <- c("TH", "PH", "MY", "ID", "SG")

south_central_asian <- c("IN", "PK", "KZ")

middle_eastern <- c("SA", "AE", "IL", "EG")

nordic_scandinavian <- c("SE", "NO", "DK", "FI", "IS")

western_european <- c("FR", "ES", "IT", "DE", "NL", "BE", "LU", "CH", "AT")

eastern_european <- c("UA", "RO", "PL", "HU", "CZ", "SK", "EE", "LV", "LT", "BG", "BY")

african <- c("ZA", "NG", "MA")

anglo_western <- c("US", "CA", "GB", "IE", "AU", "NZ")


universal_top_spotify_songs$region <- case_when(
  universal_top_spotify_songs$country %in% latin_american ~ "Latin American",
  universal_top_spotify_songs$country %in% portuguese_speaking ~ "Portuguese-Speaking",
  universal_top_spotify_songs$country %in% east_asian ~ "East Asian",
  universal_top_spotify_songs$country %in% southeast_asian ~ "Southeast Asian",
  universal_top_spotify_songs$country %in% south_central_asian ~ "South-Central Asian",
  universal_top_spotify_songs$country %in% middle_eastern ~ "Middle Eastern",
  universal_top_spotify_songs$country %in% nordic_scandinavian ~ "Nordic/Scandinavian",
  universal_top_spotify_songs$country %in% western_european ~ "Western European",
  universal_top_spotify_songs$country %in% eastern_european ~ "Eastern European",
  universal_top_spotify_songs$country %in% african ~ "African",
  universal_top_spotify_songs$country %in% anglo_western ~ "Anglo-Western",
  TRUE ~ "Overall" # Fallback category for any unmatched countries
)







# cluster variables
#####

# popularity, is_explicit, duration_ms, dancability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, libeliness, valance, tempo, time_signature, age

# categorical: is_explicit, key, mode, time_signature

# numerical normal: popularity, is_explicit, duration_ms, dancability, energy, loudness, speechiness, acousticness, instrumentalness, liveliness, valance, tempo, age

cluster_df_cov <- universal_top_spotify_songs %>%
  subset(select = c(-name, -artists, -daily_rank, -daily_movement, -weekly_movement,
                    -snapshot_date, -album_name, -album_release_date, -country,
                    -key_mode
  ))



cluster_df_cov_clean <- na.omit(cluster_df)  # Remove rows with NA




cluster_df_region <- universal_top_spotify_songs %>%
  subset(select = c(-name, -artists, -daily_rank, -daily_movement, -weekly_movement,
                    -snapshot_date, -album_name, -album_release_date, -country,
                    -key_mode
                    ))


cluster_df_clean_region <- na.omit(cluster_df_region)# Remove rows with NA

cluster_df_clean <- cluster_df_clean_region %>%
  select(-region, -mode)


save(cluster_df_clean, file = "cluster_df_clean.RData")


# Define the types for each column (10 numerical and 5 categorical)
types_vector <- c(rep('n', 12), rep('c', 4))

# Perform k-prototype clustering
library(clustMixType)
kproto_result <- kproto(cluster_df_clean, k = 2, types = types_vector)

cluster_df_clean_region$cluster_kproto <- as.factor(kproto_result$cluster)





# evaluate clusters
#####
# adapted from Mason Padgett's Assignment 6

# Standardization (z-score normalization) function
standardize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Standardize numerical columns in cluster_df_clean (excluding the cluster label)
cluster_df_clean_standardized <- cluster_df_clean %>%
  mutate(
    popularity = standardize(popularity),
    duration_ms = standardize(duration_ms),

    loudness = standardize(loudness),
    speechiness = standardize(speechiness),
    instrumentalness = standardize(instrumentalness),
    liveness = standardize(liveness),  # Corrected spelling
    valence = standardize(valence),    # Corrected spelling
    tempo = standardize(tempo),
    age = standardize(age)
  )

# Summarize the standardized data by cluster
line_chart_standardized_data <- cluster_df_clean_standardized %>%
  group_by(cluster_kproto) %>%
  summarise(
    MeanPopularity = mean(popularity, na.rm = TRUE),
    MeanDuration = mean(duration_ms, na.rm = TRUE),
    MeanLoudness = mean(loudness, na.rm = TRUE),
    MeanSpeechiness = mean(speechiness, na.rm = TRUE),
    MeanInstrumentalness = mean(instrumentalness, na.rm = TRUE),
    MeanLiveness = mean(liveness, na.rm = TRUE),
    MeanValence = mean(valence, na.rm = TRUE),
    MeanTempo = mean(tempo, na.rm = TRUE),
    MeanAge = mean(age, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("Mean"), 
               names_to = "Variable", 
               values_to = "DistanceFromMean")

# Rename the variables for better readability in the plot
line_chart_standardized_data$Variable <- recode(line_chart_standardized_data$Variable,
                                                "MeanPopularity" = "Popularity",
                                                "MeanDuration" = "Duration (ms)",
                                                "MeanDanceability" = "Danceability",  # Corrected spelling
                                                "MeanEnergy" = "Energy",
                                                "MeanLoudness" = "Loudness",
                                                "MeanSpeechiness" = "Speechiness",
                                                "MeanAcousticness" = "Acousticness",
                                                "MeanInstrumentalness" = "Instrumentalness",
                                                "MeanLiveness" = "Liveness",          # Corrected spelling
                                                "MeanValence" = "Valence",            # Corrected spelling
                                                "MeanTempo" = "Tempo",
                                                "MeanAge" = "Age")

# Create the line chart using the standardized means for each cluster
ggplot(line_chart_standardized_data, aes(x = Variable, y = DistanceFromMean, group = cluster_kproto, color = factor(cluster_kproto))) +
  geom_line() +
  geom_point() +
  labs(title = "Distance from the Mean of Variables by Cluster",
       x = "Variables",
       y = "Distance from Mean (Standard Deviations)",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# then I need to check categorical variables
# categorical: is_explicit, key, mode, time_signature
ggplot(cluster_df_clean, aes(x = is_explicit, fill= cluster_kproto)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kproto, x = key)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kproto, x = mode)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kproto, x = time_signature)) +
  geom_bar(position = "fill")




cluster_df_clean_country <- universal_top_spotify_songs %>%
  select(spotify_id, region) %>%
  na.omit() 
  






# how many clusters
#######

library(clustMixType)

# Function to calculate total dissimilarity for different K values
elbow_method <- function(data, max_k = 10) {
  wss <- numeric(max_k)
  for (k in 1:max_k) {
    model <- kproto(data, k)
    wss[k] <- model$tot.withinss  # Total within-cluster sum of squares
  }
  
  # Plot to visualize the elbow
  plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
       main = "Elbow Method for K-Prototypes")
}

# Apply elbow method to your dataset
kproto_elbow <- elbow_method(cluster_df_clean, max_k = 6)


set.seed(123)  # For reproducibility
# Function to compute within-cluster sum of squares (wss) for k-prototypes
elbow_method <- function(data, max_k = 10) {
  wss <- numeric(max_k)
  for (k in 1:max_k) {
    model <- kproto(data, k)
    wss[k] <- model$tot.withinss  # Total within-cluster sum of squares
  }
  return(wss)
}

# Compute wss values
kproto_wss <- elbow_method(cluster_df_clean, max_k = 6)


save(kproto_wss, file = "kproto_wss.RData")





library(corrplot)


cluster_df_clean_cov <- cluster_df_cov_clean%>%
  select( -is_explicit, -key, -mode, -time_signature, -spotify_id, -region )



cov_matrix <- cov(cluster_df_clean_cov)


# Convert the covariance matrix to a correlation matrix
cor_matrix <- cov2cor(cov_matrix)


save(cor_matrix, file = "cor_matrix.RData")

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix Heatmap")


