# Load required libraries
library(dplyr)
library(readxl)

install.packages("stringdist")
library(stringdist)
setwd('D:\\#YPR\\VCU\\Summer Courses\\SCMA\\Data')
getwd()

# Load the Data
ipl_bbb <- read.csv('IPL_ball_by_ball_updated till 2024.csv', stringsAsFactors = FALSE)
ipl_salary <- read_excel('IPL SALARIES 2024.xlsx')

# Display Salary Data
head(ipl_salary)

# Display Salary Data
head(ipl_bbb)

# Group the IPL Data
grouped_data <- ipl_bbb %>%
  group_by(Season, Innings.No, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored), wicket_confirmation = sum(wicket_confirmation))

head(grouped_data)

# Aggregate Runs and Wickets
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored))

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation))

head(player_runs)
head(player_wickets)

# Top Performers
top_run_getters <- player_runs %>%
  filter(Season == '2024') %>%
  arrange(desc(runs_scored)) %>%
  slice_head(n = 3)

bottom_wicket_takers <- player_wickets %>%
  arrange(desc(wicket_confirmation)) %>%
  slice_head(n = 3)

print("Top Three Run Getters:")
print(top_run_getters)
print("Top Three Wicket Takers:")
print(bottom_wicket_takers)

# Year Extraction and Preparation
ipl_year_id <- data.frame(id = ipl_bbb$`Match id`, year = as.numeric(format(as.Date(ipl_bbb$Date, "%d-%m-%Y"), "%Y")))

ipl_bbbc <- ipl_bbb
ipl_bbbc$year <- as.numeric(format(as.Date(ipl_bbb$Date, "%d-%m-%Y"), "%Y"))

head(ipl_bbbc[c("Match.id", "year", "runs_scored", "wicket_confirmation", "Bowler", "Striker")])

# Fit the Best Distribution
library(fitdistrplus)

get_best_distribution <- function(data) {
  dist_names <- c('gamma', 'lognorm', 'norm', 't', 'weibull')
  dist_results <- list()
  params <- list()
  
  for (dist_name in dist_names) {
    dist_fit <- fitdist(data, dist_name)
    dist_results[[dist_name]] <- dist_fit$loglik
    params[[dist_name]] <- dist_fit$estimate
  }
  
  best_dist <- names(dist_results)[which.max(dist_results)]
  print(paste("Best fitting distribution:", best_dist))
  print(paste("Parameters for the best fit:", params[[best_dist]]))
  return(list(best_dist = best_dist, params = params[[best_dist]]))
}

# Top Batsmen Runs in Last Three Years
total_run_each_year <- ipl_bbbc %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored))

total_run_each_year <- total_run_each_year %>%
  arrange(desc(year), desc(runs_scored))

list_top_batsman_last_three_year <- lapply(unique(total_run_each_year$year)[1:3], function(x) {
  total_run_each_year %>%
    filter(year == x) %>%
    slice_head(n = 3) %>%
    pull(Striker) %>%
    unique() %>%
    as.character()
})

print(list_top_batsman_last_three_year)

# Top Bowlers Wickets in Last Three Years
total_wicket_each_year <- ipl_bbbc %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation))

total_wicket_each_year <- total_wicket_each_year %>%
  arrange(desc(year), desc(wicket_confirmation))

list_top_bowler_last_three_year <- lapply(unique(total_wicket_each_year$year)[1:3], function(x) {
  total_wicket_each_year %>%
    filter(year == x) %>%
    slice_head(n = 3) %>%
    pull(Bowler) %>%
    unique() %>%
    as.character()
})

print(list_top_bowler_last_three_year)


# Filter data for Faf du Plessis
faf_data <- subset(ipl_bbbc, Striker == "F du Plessis")

# Fit the Poisson distribution to runs scored
runs_hist <- hist(faf_data$runs_scored, breaks = seq(0, max(faf_data$runs_scored) + 1, by = 1), plot = FALSE)
runs_fit <- fitdistr(runs_hist$counts, "Poisson")

# Print fitted parameters
print("Fitted Parameters for Runs Scored (Poisson):")
print(runs_fit$estimate)


# Filter data for the last three years (assuming your data has a column named "year")
last_three_years <- subset(ipl_salary, year >= 2022 & year <= 2024)

# Filter data for the latest salary in 2024
latest_salary <- last_three_years[last_three_years$year == 2024, ]

# Display the last three-year performance with the latest salary in 2024
print("Last Three-Year Performance with Latest Salary in 2024:")
print(latest_salary)

# Relationship between the performance of a player and the salary he gets
R2024 <- total_run_each_year %>%
  filter(year == 2024)

df_salary <- as.data.frame(ipl_salary)
df_runs <- as.data.frame(R2024)

match_names <- function(name, names_list) {
  match_result <- stringdist::amatch(name, names_list, maxDist = 3)  # Using stringdist for approximate matching
  if (length(match_result$target) > 0 && match_result$dist < 4) {  # Check the first match and distance
    return(names_list[match_result$target[1]])
  } else {
    return(NA)
  }
}

# Create 'Matched_Player' column in df_salary using match_names function
df_salary$Matched_Player <- sapply(df_salary$Player, match_names, names_list = df_runs$Striker)

# Remove rows with NA in 'Matched_Player' column
df_salary <- df_salary[!is.na(df_salary$Matched_Player), ]

# Merge the DataFrames
df_merged <- merge(df_salary, df_runs, by.x = "Matched_Player", by.y = "Striker", all.x = TRUE)

# Check for NAs in 'Matched_Player' column after merge
sum(is.na(df_merged$Matched_Player))

# Calculate the correlation
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")

# Print correlation
print("Correlation between Salary and Runs:")
print(correlation)
