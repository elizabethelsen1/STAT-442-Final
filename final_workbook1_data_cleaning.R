#get the big one in
library(readr)
library(dplyr)

library(lubridate) # for date and time stuff
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


# getting only the date with peak popularity
universal_top_spotify_songs <- universal_top_spotify_songs %>%
  group_by(spotify_id) %>%
  filter(popularity == max(popularity)) %>%
  ungroup()

# separation by region
# I asked Chat GPT to separate the list of countries 
#    by similar linguistic characteristics
# with the expectation that I can dive in further later



overall_df <- universal_top_spotify_songs %>%
  filter(is.na(country))


# creating country lists

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



# filtering data frames for each group

latin_american_df <- universal_top_spotify_songs %>%
  filter(country %in% latin_american)

portuguese_speaking_df <- universal_top_spotify_songs %>%
  filter(country %in% portuguese_speaking)

east_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% east_asian)

southeast_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% southeast_asian)

south_central_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% south_central_asian)

middle_eastern_df <- universal_top_spotify_songs %>%
  filter(country %in% middle_eastern)

nordic_scandinavian_df <- universal_top_spotify_songs %>%
  filter(country %in% nordic_scandinavian)

western_european_df <- universal_top_spotify_songs %>%
  filter(country %in% western_european)

eastern_european_df <- universal_top_spotify_songs %>%
  filter(country %in% eastern_european)

african_df <- universal_top_spotify_songs %>%
  filter(country %in% african)

anglo_western_df <- universal_top_spotify_songs %>%
  filter(country %in% anglo_western)



# summary creation by key signiture

calculate_averages_by_diff <- function(data, group_var = "mode") {
  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      number_in_section = n_distinct(spotify_id),
      avg_age = mean(age, na.rm = TRUE),
      avg_danceability = mean(danceability, na.rm = TRUE),
      avg_explicit = mean(as.numeric(is_explicit), na.rm = TRUE),
      avg_duration = mean(duration_ms, na.rm = TRUE),
      avg_energy = mean(energy, na.rm = TRUE),
      avg_loudness = mean(loudness, na.rm = TRUE),
      avg_speechiness = mean(speechiness, na.rm = TRUE),
      avg_acousticness = mean(acousticness, na.rm = TRUE),
      avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
      avg_liveness = mean(liveness, na.rm = TRUE),
      avg_valence = mean(valence, na.rm = TRUE),
      avg_tempo = mean(tempo, na.rm = TRUE),

      median_age = median(age, na.rm = TRUE),
      median_danceability = median(danceability, na.rm = TRUE),
      median_explicit = median(as.numeric(is_explicit), na.rm = TRUE),
      median_duration = median(duration_ms, na.rm = TRUE),
      median_energy = median(energy, na.rm = TRUE),
      median_loudness = median(loudness, na.rm = TRUE),
      median_speechiness = median(speechiness, na.rm = TRUE),
      median_acousticness = median(acousticness, na.rm = TRUE),
      median_instrumentalness = median(instrumentalness, na.rm = TRUE),
      median_liveness = median(liveness, na.rm = TRUE),
      median_valence = median(valence, na.rm = TRUE),
      median_tempo = median(tempo, na.rm = TRUE)
    )
}

# 
# # using the function
# 
# overall_averages_ks <- calculate_averages_by_ks(overall_df)
# 
# latin_american_averages_ks <- calculate_averages_by_ks(latin_american_df)
# 
# portuguese_speaking_averages_ks <- calculate_averages_by_ks(portuguese_speaking_df)
# 
# east_asian_averages_ks <- calculate_averages_by_ks(east_asian_df)
# 
# southeast_asian_averages_ks <- calculate_averages_by_ks(southeast_asian_df)
# 
# south_central_asian_averages_ks <- calculate_averages_by_ks(south_central_asian_df)
# 
# middle_eastern_averages_ks <- calculate_averages_by_ks(middle_eastern_df)
# 
# nordic_scandinavian_averages_ks <- calculate_averages_by_ks(nordic_scandinavian_df)
# 
# western_european_averages_ks <- calculate_averages_by_ks(western_european_df)
# 
# eastern_european_averages_ks <- calculate_averages_by_ks(eastern_european_df)
# 
# african_averages_ks <- calculate_averages_by_ks(african_df)
# 
# anglo_western_averages_ks <- calculate_averages_by_ks(anglo_western_df)
# 




# overall averages

calculate_averages <- function(data) {
  data %>%
    summarise(
      number_in_section = n_distinct(spotify_id),
      avg_age = mean(age, na.rm = TRUE),
      avg_danceability = mean(danceability, na.rm = TRUE),
      avg_explicit = mean(as.numeric(is_explicit), na.rm = TRUE),
      avg_duration = mean(duration_ms, na.rm = TRUE),
      avg_energy = mean(energy, na.rm = TRUE),
      avg_loudness = mean(loudness, na.rm = TRUE),
      avg_speechiness = mean(speechiness, na.rm = TRUE),
      avg_acousticness = mean(acousticness, na.rm = TRUE),
      avg_instrumentalness = mean(instrumentalness, na.rm = TRUE),
      avg_liveness = mean(liveness, na.rm = TRUE),
      avg_valence = mean(valence, na.rm = TRUE),
      avg_tempo = mean(tempo, na.rm = TRUE),
      
      median_age = median(age, na.rm = TRUE),
      median_danceability = median(danceability, na.rm = TRUE),
      median_explicit = median(as.numeric(is_explicit), na.rm = TRUE),
      median_duration = median(duration_ms, na.rm = TRUE),
      median_energy = median(energy, na.rm = TRUE),
      median_loudness = median(loudness, na.rm = TRUE),
      median_speechiness = median(speechiness, na.rm = TRUE),
      median_acousticness = median(acousticness, na.rm = TRUE),
      median_instrumentalness = median(instrumentalness, na.rm = TRUE),
      median_liveness = median(liveness, na.rm = TRUE),
      median_valence = median(valence, na.rm = TRUE),
      median_tempo = median(tempo, na.rm = TRUE)
    )
}

# using the function

overall_averages <- calculate_averages(overall_df)

latin_american_averages <- calculate_averages(latin_american_df)

portuguese_speaking_averages <- calculate_averages(portuguese_speaking_df)

east_asian_averages <- calculate_averages(east_asian_df)

southeast_asian_averages <- calculate_averages(southeast_asian_df)

south_central_asian_averages <- calculate_averages(south_central_asian_df)

middle_eastern_averages <- calculate_averages(middle_eastern_df)

nordic_scandinavian_averages <- calculate_averages(nordic_scandinavian_df)

western_european_averages <- calculate_averages(western_european_df)

eastern_european_averages <- calculate_averages(eastern_european_df)

african_averages <- calculate_averages(african_df)

anglo_western_averages <- calculate_averages(anglo_western_df)

overall_averages$place <- "Overall"

latin_american_averages$place <- "Latin American"

portuguese_speaking_averages$place <- "Portuguese Speaking"

east_asian_averages$place <- "East Asian"

southeast_asian_averages$place <- "Southeast Asian"

south_central_asian_averages$place <- "South Centeral Asian"

middle_eastern_averages$place <- "Middle Eastern"

nordic_scandinavian_averages$place <- "Nordic Scandinavian"

western_european_averages$place <- "Western European"

eastern_european_averages$place <- "Eastern European"

african_averages$place <- "African"

anglo_western_averages$place <- "Anglo Western"


all_averages <- rbind(overall_averages,latin_american_averages,portuguese_speaking_averages,
                      east_asian_averages,southeast_asian_averages, south_central_asian_averages,
                      middle_eastern_averages,nordic_scandinavian_averages,western_european_averages,
                      eastern_european_averages,african_averages,anglo_western_averages)



rm(overall_averages,latin_american_averages,portuguese_speaking_averages,
   east_asian_averages,southeast_asian_averages, south_central_asian_averages,
   middle_eastern_averages,nordic_scandinavian_averages,western_european_averages,
   eastern_european_averages,african_averages,anglo_western_averages)

# which region has the largest number of songs grace the charts
# new songs per snapshot? 
# (unique(spotify_id)/n_distinct(country_list))/n_distinct(african_df$snapshot_date)

save(all_averages, file = "appAverages.RData")
save(universal_top_spotify_songs, file = "appUniversal.RData")


#seasons
#####

# making seperate dfs by season of year
winter <- c("December","January","Febuary")
spring <- c("March","April","May")
summer <- c("June","July","August")
fall <- c("September","October","November")


winter_df <- universal_top_spotify_songs %>%
  filter(months(snapshot_date) %in% winter)


spring_df <- universal_top_spotify_songs %>%
  filter(months(snapshot_date) %in% spring)


summer_df <- universal_top_spotify_songs %>%
  filter(months(snapshot_date) %in% summer)


fall_df <- universal_top_spotify_songs %>%
  filter(months(snapshot_date) %in% fall)


winter_averages <- calculate_averages(winter_df)

winter_averages_ks <- calculate_averages_by_ks(winter_df)


spring_averages <- calculate_averages(spring_df)

spring_averages_ks <- calculate_averages_by_ks(spring_df)


summer_averages <- calculate_averages(summer_df)

summer_averages_ks <- calculate_averages_by_ks(summer_df)


fall_averages <- calculate_averages(fall_df)

fall_averages_ks <- calculate_averages_by_ks(fall_df)




#Tasks

# compare country averages_ks to eachother
# compare season averages_ks to eachother
# compare country averages to eachother
# compare season averages to eachother



# decision tree
# clustering - what types of songs are there?






# should divine it up by years:

df2023 <- universal_top_spotify_songs %>%
  filter(year(snapshot_date) == 2023)

df2024 <- universal_top_spotify_songs %>%
  filter(year(snapshot_date) == 2024)


winter_df_2023 <- df2023 %>%
  filter(months(snapshot_date) %in% winter)


# spring_df_2023 <- df2023 %>%
#   filter(months(snapshot_date) %in% spring)
# 
# 
# summer_df_2023 <- df2023 %>%
#   filter(months(snapshot_date) %in% summer)


fall_df_2023 <- df2023 %>%
  filter(months(snapshot_date) %in% fall)

winter_df_2024 <- df2024 %>%
  filter(months(snapshot_date) %in% winter)


spring_df_2024 <- df2024 %>%
  filter(months(snapshot_date) %in% spring)


summer_df_2024 <- df2024 %>%
  filter(months(snapshot_date) %in% summer)


fall_df_2024 <- df2024 %>%
  filter(months(snapshot_date) %in% fall)



winter_averages_2023 <- calculate_averages(winter_df_2023)

# spring_averages_2023 <- calculate_averages(spring_df_2023)
# 
# summer_averages_2023 <- calculate_averages(summer_df_2023)

fall_averages_2023 <- calculate_averages(fall_df_2023)


winter_averages_2024 <- calculate_averages(winter_df_2024)

spring_averages_2024 <- calculate_averages(spring_df_2024)

summer_averages_2024 <- calculate_averages(summer_df_2024)

fall_averages_2024 <- calculate_averages(fall_df_2024)



fall_averages_2024$time <- "fall 24"
winter_averages_2024$time <- "winter 24"
summer_averages_2024$time <- "summer 24"
spring_averages_2024$time <- "spring 24"

fall_averages_2023$time <- "fall 23"
winter_averages_2023$time <- "winter 23"
# summer_averages_2023$time <- "summer 23"
# spring_averages_2023$time <- "spring 23"





all_averages_seasons <- rbind(fall_averages_2024,summer_averages_2024,
                              spring_averages_2024,fall_averages_2023,
                              winter_averages_2023, winter_averages_2024)


save(all_averages_seasons, file = "appSeasons.RData")


ggplot(all_averages_seasons, aes(x = time, y = avg_explicit)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Explicit", title = "Bar Chart of Average Explicit by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(all_averages_seasons, aes(x = time, y = avg_age)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Age", title = "Bar Chart of Average Age by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(all_averages_seasons, aes(x = time, y = avg_danceability)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Dancability", title = "Bar Chart of Average Dancability by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages_seasons, aes(x = time, y = avg_energy)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Energy", title = "Bar Chart of Average Energy by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(all_averages_seasons, aes(x = time, y = avg_duration)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average duration", title = "Bar Chart of Average duration by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages_seasons, aes(x = time, y = avg_loudness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average loudness", title = "Bar Chart of Average loudness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(all_averages_seasons, aes(x = time, y = avg_speechiness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average speechiness", title = "Bar Chart of Average speechiness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages_seasons, aes(x = time, y = avg_acousticness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(all_averages_seasons, aes(x = time, y = avg_tempo)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average tempo", title = "Bar Chart of Average tempo by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages_seasons, aes(x = time, y = avg_valence)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average valence", title = "Bar Chart of Average valence by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages_seasons, aes(x = time, y = avg_acousticness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(all_averages_seasons, aes(x = time, y = avg_instrumentalness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average instrumentalness", title = "Bar Chart of Average instrumentalness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





######

### basically the time of year does not matter outside of christmas songs













library(ggplot2)

# mode = 0 little searing is middle east, east asian and south centeral asian
#         near .5 searing for latin american



ggplot(all_averages, aes(x = place, y = avg_explicit)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Explicit", title = "Bar Chart of Average Explicit by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# us anglo western nations like our oldies

# mode = 0: somehow overall is the oldest. next is anglo

ggplot(all_averages, aes(x = place, y = avg_age)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Age", title = "Bar Chart of Average Age by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# for everyone constant hight energy and dancability
# mode = 0 still consistently over .6 dancability and energy

ggplot(all_averages, aes(x = place, y = avg_danceability)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Dancability", title = "Bar Chart of Average Dancability by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages, aes(x = place, y = avg_energy)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average Energy", title = "Bar Chart of Average Energy by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# mode = 0: just barely aftican the longest

ggplot(all_averages, aes(x = place, y = avg_duration)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average duration", title = "Bar Chart of Average duration by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# mode = 0: east asian/ latin american has loudest
#           african and south centeral asian has quietest
ggplot(all_averages, aes(x = place, y = avg_loudness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average loudness", title = "Bar Chart of Average loudness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# mode = 1 has high african and protuguese
# mode = 0 same
ggplot(all_averages, aes(x = place, y = avg_speechiness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average speechiness", title = "Bar Chart of Average speechiness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages, aes(x = place, y = avg_acousticness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# mode = 1 tempo near constant
ggplot(all_averages, aes(x = place, y = avg_tempo)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average tempo", title = "Bar Chart of Average tempo by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages, aes(x = place, y = avg_valence)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average valence", title = "Bar Chart of Average valence by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(all_averages, aes(x = place, y = avg_acousticness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# big differences
ggplot(all_averages, aes(x = place, y = avg_instrumentalness)) +
  geom_bar(stat = "identity") +
  labs(x = "Place", y = "Average instrumentalness", title = "Bar Chart of Average instrumentalness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








# making it be major vs minor
##### 



latin_american_df <- universal_top_spotify_songs %>%
  filter(country %in% latin_american)

portuguese_speaking_df <- universal_top_spotify_songs %>%
  filter(country %in% portuguese_speaking)

east_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% east_asian)

southeast_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% southeast_asian)

south_central_asian_df <- universal_top_spotify_songs %>%
  filter(country %in% south_central_asian)

middle_eastern_df <- universal_top_spotify_songs %>%
  filter(country %in% middle_eastern)

nordic_scandinavian_df <- universal_top_spotify_songs %>%
  filter(country %in% nordic_scandinavian)

western_european_df <- universal_top_spotify_songs %>%
  filter(country %in% western_european)

eastern_european_df <- universal_top_spotify_songs %>%
  filter(country %in% eastern_european)

african_df <- universal_top_spotify_songs %>%
  filter(country %in% african)

anglo_western_df <- universal_top_spotify_songs %>%
  filter(country %in% anglo_western)




overall_averages_mode <- calculate_averages_by_diff(overall_df)

latin_american_averages_mode <- calculate_averages_by_diff(latin_american_df)

portuguese_speaking_averages_mode <- calculate_averages_by_diff(portuguese_speaking_df)

east_asian_averages_mode <- calculate_averages_by_diff(east_asian_df)

southeast_asian_averages_mode <- calculate_averages_by_diff(southeast_asian_df)

south_central_asian_averages_mode <- calculate_averages_by_diff(south_central_asian_df)

middle_eastern_averages_mode <- calculate_averages_by_diff(middle_eastern_df)

nordic_scandinavian_averages_mode <- calculate_averages_by_diff(nordic_scandinavian_df)

western_european_averages_mode <- calculate_averages_by_diff(western_european_df)

eastern_european_averages_mode <- calculate_averages_by_diff(eastern_european_df)

african_averages_mode <- calculate_averages_by_diff(african_df)

anglo_western_averages_mode <- calculate_averages_by_diff(anglo_western_df)

overall_averages_mode$place <- "Overall"

latin_american_averages_mode$place <- "Latin American"

portuguese_speaking_averages_mode$place <- "Portuguese Speaking"

east_asian_averages_mode$place <- "East Asian"

southeast_asian_averages_mode$place <- "Southeast Asian"

south_central_asian_averages_mode$place <- "South Centeral Asian"

middle_eastern_averages_mode$place <- "Middle Eastern"

nordic_scandinavian_averages_mode$place <- "Nordic Scandinavian"

western_european_averages_mode$place <- "Western European"

eastern_european_averages_mode$place <- "Eastern European"

african_averages_mode$place <- "African"

anglo_western_averages_mode$place <- "Anglo Western"


all_averages_mode <- rbind(overall_averages_mode,latin_american_averages_mode,portuguese_speaking_averages_mode,
                      east_asian_averages_mode,southeast_asian_averages_mode, south_central_asian_averages_mode,
                      middle_eastern_averages_mode,nordic_scandinavian_averages_mode,western_european_averages_mode,
                      eastern_european_averages_mode,african_averages_mode,anglo_western_averages_mode)




save(all_averages_mode, file = "appMode.RData")


rm(overall_averages_mode,latin_american_averages_mode,portuguese_speaking_averages_mode,
   east_asian_averages_mode,southeast_asian_averages_mode, south_central_asian_averages_mode,
   middle_eastern_averages_mode,nordic_scandinavian_averages_mode,western_european_averages_mode,
   eastern_european_averages_mode,african_averages_mode,anglo_western_averages_mode)


# middle eastern and eastern european minor keys are more explicit
ggplot(all_averages_mode, aes(x = place, y = avg_explicit, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average Explicit", title = "Bar Chart of Average Explicit by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# minor key songs are older everywhere
ggplot(all_averages_mode, aes(x = place, y = avg_age, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average Age", title = "Bar Chart of Average Age by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# minor songs are slightly more danceable in africa
ggplot(all_averages_mode, aes(x = place, y = avg_danceability, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average Dancability", title = "Bar Chart of Average Dancability by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# portuguese songs that are minor have more energy on average then major songs
ggplot(all_averages_mode, aes(x = place, y = avg_energy, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average Energy", title = "Bar Chart of Average Energy by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# hovering aroung same length
ggplot(all_averages_mode, aes(x = place, y = avg_duration, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average duration", title = "Bar Chart of Average duration by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# mix again, latin american more loud overall, portuguese close
ggplot(all_averages_mode, aes(x = place, y = avg_loudness, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average loudness", title = "Bar Chart of Average loudness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# portuguese more speechy, major significantly more
ggplot(all_averages_mode, aes(x = place, y = avg_speechiness, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average speechiness", title = "Bar Chart of Average speechiness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# strange
ggplot(all_averages_mode, aes(x = place, y = avg_acousticness, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# stange because tempos are near constant worldwide and major and minor doesn't really change it
ggplot(all_averages_mode, aes(x = place, y = avg_tempo, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average tempo", title = "Bar Chart of Average tempo by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#"measure of musical positiveness" 
# close for all, 
# boxplots might be nice


ggplot(all_averages_mode, aes(x = place, y = avg_valence, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average valence", title = "Bar Chart of Average valence by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# dominated  by monor key wxcept in african, eastern eu, latin america
# middle east, portuguese
ggplot(all_averages_mode, aes(x = place, y = avg_acousticness, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average acousticness", title = "Bar Chart of Average acousticness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# stange
ggplot(all_averages_mode, aes(x = place, y = avg_instrumentalness, fill = mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Place", y = "Average instrumentalness", title = "Bar Chart of Average instrumentalness by Place") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 1 is minor 0 is major, 



# maybe cluster songs without country in it, and then see what portion of clusters are in each country







# what are the distributions
#####

# fairly normal
ggplot(universal_top_spotify_songs, aes(tempo))+
  geom_histogram(
  )


#skewed right
ggplot(universal_top_spotify_songs, aes(popularity))+
  geom_histogram(
  )

# fairly normalish
ggplot(universal_top_spotify_songs, aes(loudness,
                                        fill = region
                                        ))+
  geom_histogram(
  )

# HEAVILY skewed right
ggplot(universal_top_spotify_songs, aes(age))+
  geom_histogram(
  )

#dancability barely normalish skewed right
ggplot(universal_top_spotify_songs, aes(danceability))+
  geom_histogram(
  )


# NOT NORMAL
ggplot(universal_top_spotify_songs, aes(acousticness))+
  geom_histogram(
  )




# NOT NORMALLLLLL
ggplot(universal_top_spotify_songs, aes(instrumentalness))+
  geom_histogram(
  )


# not really normal
ggplot(universal_top_spotify_songs, aes(valence))+
  geom_histogram(
  )

# skewed right
ggplot(universal_top_spotify_songs, aes(liveness))+
  geom_histogram(
  )



# fairly normal
ggplot(universal_top_spotify_songs, aes(loudness))+
  geom_histogram(
  )
ks.test(universal_top_spotify_songs$loudness, "pnorm", mean = mean(universal_top_spotify_songs$loudness), sd = sd(universal_top_spotify_songs$loudness))


# weighted on 4
ggplot(universal_top_spotify_songs, aes(time_signature, fill = region))+
  geom_bar(
           
  )


ggplot(universal_top_spotify_songs, aes(key, fill = mode))+
  geom_bar(position = "dodge"
  )




set.seed(123) # For reproducibility
sample_data <- sample(universal_top_spotify_songs$tempo, 5000) # Take a random sample
shapiro.test(sample_data)


sample_data <- sample(universal_top_spotify_songs$loudness, 5000) # Take a random sample
shapiro.test(sample_data)



aov_response <- aov(universal_top_spotify_songs$mode ~ universal_top_spotify_songs$key, data = universal_top_spotify_songs)
summary(aov_response)

aov_response <- aov(response_variable ~ factor_variable, data = dataset)
summary(aov_response)

aov_response <- aov(response_variable ~ factor_variable, data = dataset)
summary(aov_response)

aov_response <- aov(response_variable ~ factor_variable, data = dataset)
summary(aov_response)





contingency_table <- table(universal_top_spotify_songs$mode, universal_top_spotify_songs$key)
chisq.test(contingency_table)

contingency_table <- table(universal_top_spotify_songs$mode, universal_top_spotify_songs$is_explicit)
chisq.test(contingency_table)

contingency_table <- table(universal_top_spotify_songs$is_explicit, universal_top_spotify_songs$key)
chisq.test(contingency_table)


