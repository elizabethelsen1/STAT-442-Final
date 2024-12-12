###########


# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)


# data prep
#####
#get the big one in
library(readr)
library(dplyr)

library(lubridate) # for date and time stuff



cluster_df_clean_cov <- cluster_df_clean %>%
  select(-spotify_id, -is_explicit, -key, -time_signature)


# Standardize numeric variables (if needed)
cluster_df_clean_scaled <- cluster_df_clean_cov %>%
  mutate(across(where(is.numeric), scale)) 


set.seed(123)  # For reproducibility

sampled_data <- cluster_df_clean_scaled[sample(nrow(cluster_df_clean_scaled), size = 0.1 * nrow(cluster_df_clean_scaled)), ]



wss <- function(k) {
  kmeans(sampled_data, centers = k, nstart = 10, iter.max = 100)$tot.withinss
}

# Apply WSS for a range of cluster numbers
k_values <- 1:7

wss_values <- sapply(k_values, wss)


# Plot the Elbow Method
plot(k_values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)", ylab = "Total within-cluster sum of squares")


set.seed(123)  # For reproducibility

# Perform K-means clustering
k <- 6  # Replace with the chosen number of clusters
kmeans_result <-kmeans(cluster_df_clean_scaled, centers = k, nstart = 25)

# Add cluster assignments to the dataframe
cluster_df_clean_region$cluster_kmeans6 <- as.factor(kmeans_result$cluster)


k <- 4  # Replace with the chosen number of clusters
kmeans_result <- kmeans(cluster_df_clean_scaled, centers = k, nstart = 25)

# Add cluster assignments to the dataframe
cluster_df_clean_region$cluster_kmeans4 <- as.factor(kmeans_result$cluster)


k <- 2  # Replace with the chosen number of clusters
kmeans_result <- kmeans(cluster_df_clean_scaled, centers = k, nstart = 25)

# Add cluster assignments to the dataframe
cluster_df_clean_region$cluster_kmeans2 <- as.factor(kmeans_result$cluster)


save(cluster_df_clean_region, file = "cluster_df_clean_region.RData")



library(ggplot2)



ggplot(cluster_df_clean, aes(x= cluster_kmeans2, fill = cluster_kproto))+
  geom_bar(position = "fill")



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
  group_by(cluster_kmeans2) %>%
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
ggplot(line_chart_standardized_data, aes(x = Variable, y = DistanceFromMean, group = cluster_kmeans2, color = factor(cluster_kmeans2))) +
  geom_line() +
  geom_point() +
  labs(title = "Distance from the Mean of Variables by Cluster",
       x = "Variables",
       y = "Distance from Mean (Standard Deviations)",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

##########################

# Summarize the standardized data by cluster
line_chart_standardized_data <- cluster_df_clean_standardized %>%
  group_by(cluster_kmeans2) %>%
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
ggplot(line_chart_standardized_data, aes(x = Variable, y = DistanceFromMean, group = cluster_kmeans2, color = factor(cluster_kmeans2))) +
  geom_line() +
  geom_point() +
  labs(title = "Distance from the Mean of Variables by Cluster",
       x = "Variables",
       y = "Distance from Mean (Standard Deviations)",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



# 2 k means

#   1 is low age, short, low inst, high loudness, higher speechiness, higher tempo, and slightly happier
#       new, happy, loud songs

#   2 are older, long, higher inst, quiet, slightly higher popularity peaks, less speechy, slower tempo. and more somber
#       older, somber, long, songs


ggplot(cluster_df_clean, aes(fill = cluster_kmeans2, x = region)) +
  geom_bar(position = "fill") +
  labs(
    x = "Region",
    y = "Proportion",
    fill = "2 K Means Clusters",
    title = "100% Stacked Bar Chart by Region"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels at 45 degrees
  )+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gold")  # Add horizontal line at 50%


# Likes 1: Eastern European, Latin American, Nordic, overall, portuguese, western european
# Likes 2: southeast asian
# within 5% of 50%: African, Anglo-Western, East Asian, Middle Eastern, south centeral asian



################

# Summarize the standardized data by cluster
line_chart_standardized_data <- cluster_df_clean_standardized %>%
  group_by(cluster_kmeans4) %>%
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
ggplot(line_chart_standardized_data, aes(x = Variable, y = DistanceFromMean, group = cluster_kmeans4, color = factor(cluster_kmeans4))) +
  geom_line() +
  geom_point() +
  labs(title = "Distance from the Mean of Variables by Cluster",
       x = "Variables",
       y = "Distance from Mean (Standard Deviations)",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


ggplot(cluster_df_clean, aes(fill = cluster_kmeans4, x = region)) +
  geom_bar(position = "fill") +
  labs(
    x = "Region",
    y = "Proportion",
    fill = "4 K Means Clusters",
    title = "100% Stacked Bar Chart by Region"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels at 45 degrees
  )+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gold")  # Add horizontal line at 50%


###############


# Summarize the standardized data by cluster
line_chart_standardized_data <- cluster_df_clean_standardized %>%
  group_by(cluster_kmeans6) %>%
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
ggplot(line_chart_standardized_data, aes(x = Variable, y = DistanceFromMean, group = cluster_kmeans6, color = factor(cluster_kmeans6))) +
  geom_line() +
  geom_point() +
  labs(title = "Distance from the Mean of Variables by Cluster",
       x = "Variables",
       y = "Distance from Mean (Standard Deviations)",
       color = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


ggplot(cluster_df_clean, aes(fill = cluster_kmeans6, x = region)) +
  geom_bar(position = "fill") +
  labs(
    x = "Region",
    y = "Proportion",
    fill = "6 K Means Clusters",
    title = "100% Stacked Bar Chart by Region"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels at 45 degrees
  )+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gold")  # Add horizontal line at 50%


#############


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


ggplot(cluster_df_clean, aes(fill = cluster_kproto, x = region)) +
  geom_bar(position = "fill") +
  labs(
    x = "Region",
    y = "Proportion",
    fill = "2 K Prototype Clusters",
    title = "100% Stacked Bar Chart by Region"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis to percentages
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Tilt x-axis labels at 45 degrees
  )+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gold")  # Add horizontal line at 50%






#########################
# then I need to check categorical variables
# categorical: is_explicit, key, mode, time_signature
ggplot(cluster_df_clean, aes(x = is_explicit, fill= cluster_kmeans2)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kmeans2, x = key)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kmeans2, x = mode)) +
  geom_bar(position = "fill")

ggplot(cluster_df_clean, aes(fill= cluster_kmeans2, x = time_signature)) +
  geom_bar(position = "fill")




ggplot(cluster_df_clean, aes(fill= cluster_kmeans6, x = region)) +
  geom_bar(position = "fill")




