# Load necessary libraries
library(tidyverse)
library(broom)
library(uuid)
library(pbapply)

# At the moment, simulation written so that only political ideology questions have some specified priors. All others covariates simulated as random. 

# Set seed for reproducibility
set.seed(123L)

# Define parameters
n_tweets <- 100
n_users <- 1000
lambda_interactions <- 5 # Mean number of interactions per user

# Number of iterations
n_iterations <- 100

# Define custom probabilities for political orientation
custom_probs <- c(0.1, 0.2, 0.4, 0.2, 0.1)

# Define news media options and their biases
news_media_options <- c("BBC News", "ITV News", "Sky News", 
                        "Channel 4 News", "Daily Mail/Mail on Sunday", "Commercial radio news", 
                        "Regional or local newspaper", "GB News", 
                        "Guardian/Observer", "Metro", "Sun/Sun on Sunday", 
                        "The Times/Sunday Times", "Al Jazeera", "Daily Telegraph/Sunday Telegraph", 
                        "Channel 5 News", "CNN")

news_media_bias <- c("Non-biased", "Non-biased", "Non-biased", 
                     "Left-leaning", "Right-leaning", "Non-biased", 
                     "Non-biased", "Right-leaning", 
                     "Left-leaning", "Non-biased", "Right-leaning", 
                     "Right-leaning", "Left-leaning", "Right-leaning", 
                     "Non-biased", "Non-biased")

# Create a data frame for the news sources and their biases
news_sources <- tibble(
  source = news_media_options,
  bias = news_media_bias,
  score = case_when(
    news_media_bias == "Left-leaning" ~ -1,
    news_media_bias == "Right-leaning" ~ 1,
    TRUE ~ 0
  )
)

# Function to run one iteration of the simulation and model estimation
run_simulation <- function(iteration) {
  # Generate tweet IDs and their political orientation
  tweets <- tibble(
    tweetid = sample(1:1e10, n_tweets),
    political_orientation_tweet = sample(c("Conservative", "Liberal"), n_tweets, replace = TRUE)
  )
  
  # Generate users with a preference for a political orientation
  users <- tibble(
    session_id = replicate(n_users, UUIDgenerate()),
    age = sample(18:65, n_users, replace = TRUE),
    gender = sample(c("Male", "Female"), n_users, replace = TRUE),
    social_media_use = sample(0:12, n_users, replace = TRUE),
    party_id = sample(c("Conservative", "Labour"), n_users, replace = TRUE),
    partyid_strength = sample(1:5, n_users, replace = TRUE), # 1 = not very strong, 5 = very strong
    political_interest = sample(1:5, n_users, replace = TRUE), # 1 = not interested, 5 = very interested
    income = sample(1:10, n_users, replace = TRUE), # 1 = lowest, 10 = highest
    university_education = sample(c(0, 1), n_users, replace = TRUE), # 0 = no, 1 = yes
    nonelec_participation = sample(0:10, n_users, replace = TRUE), # Number of non-electoral participation activities
    social_endorsement = sample(0:1000, n_users, replace = TRUE), # Number of likes/retweets on a given tweet
    exposure_to_diff_opinions = sample(1:5, n_users, replace = TRUE), # 1 = never, 5 = very often
    political_orientation_user = sample(1:5, n_users, replace = TRUE, prob = custom_probs)
  ) %>%
    mutate(
      political_orientation_group = case_when(
        political_orientation_user %in% 1:2 ~ "Conservative",
        political_orientation_user %in% 4:5 ~ "Liberal",
        TRUE ~ "Moderate"
      ),
      political_orientation_user = factor(political_orientation_user, levels = c(3, 1, 2, 4, 5)),
      n_interactions = rpois(n_users, lambda_interactions) # Number of interactions per user
    )
  
  # Simulate Likert scale responses for each news source
  news_responses <- matrix(sample(1:5, n_users * length(news_media_options), replace = TRUE), ncol = length(news_media_options))
  colnames(news_responses) <- news_media_options
  news_responses <- as_tibble(news_responses)
  users <- bind_cols(users, news_responses)
  
  # Calculate the news diversity score for each user
  users <- users %>%
    rowwise() %>%
    mutate(
      news_diet_diversity_score = sum(c_across(all_of(news_media_options)) * news_sources$score)
    ) %>%
    ungroup()
  
  # Simulate interactions
  interactions <- users %>%
    rowwise() %>%
    do({
      tibble(
        session_id = .$session_id,
        timestamp = sample(seq(as.POSIXct('2024-01-01'), as.POSIXct('2024-12-31'), by="hour"), .$n_interactions, replace = TRUE)
      )
    }) %>%
    ungroup()
  
  # Merge interactions with users
  interactions <- interactions %>%
    left_join(users, by = "session_id")
  
  # Assign tweet interactions based on user preferences
  interactions <- interactions %>%
    rowwise() %>%
    mutate(
      tweetid = {
        prob <- runif(1)
        conservative_bias <- ifelse(political_orientation_user == 1, 0.05, 0)
        liberal_bias <- ifelse(political_orientation_user == 5, 0.05, 0)
        
        if (political_orientation_group == "Conservative" && prob < (0.6 + conservative_bias)) {
          sample(tweets$tweetid[tweets$political_orientation_tweet == "Conservative"], 1)
        } else if (political_orientation_group == "Liberal" && prob < (0.6 + liberal_bias)) {
          sample(tweets$tweetid[tweets$political_orientation_tweet == "Liberal"], 1)
        } else if (political_orientation_group == "Moderate") {
          if (prob < 0.5) {
            sample(tweets$tweetid[tweets$political_orientation_tweet == "Conservative"], 1)
          } else {
            sample(tweets$tweetid[tweets$political_orientation_tweet == "Liberal"], 1)
          }
        } else {
          sample(tweets$tweetid[tweets$political_orientation_tweet != political_orientation_group], 1)
        }
      }
    ) %>%
    ungroup()
  
  # Merge interactions with tweets
  final_data <- interactions %>%
    filter(!is.na(tweetid)) %>%
    left_join(tweets, by = "tweetid")
  
  # Convert political_orientation_tweet to a binary variable
  final_data <- final_data %>%
    mutate(
      political_orientation_tweet_binary = ifelse(political_orientation_tweet == "Conservative", 1, 0)
    )
  
  # Fit logistic regression model
  model <- glm(
    political_orientation_tweet_binary ~ age + gender + social_media_use + party_id + partyid_strength + political_interest + news_diet_diversity_score + income + university_education + nonelec_participation + social_endorsement + exposure_to_diff_opinions + political_orientation_user,
    data = final_data,
    family = binomial
  )
  
  # Extract coefficients for all covariates with readable names
  coefs <- tidy(model) %>%
    filter(term %in% c("age", "genderMale", "social_media_use", "party_idLabour", "party_idConservative",
                       "partyid_strength", "political_interest", "news_diet_diversity_score", "income", "university_education", 
                       "nonelec_participation", "social_endorsement", "exposure_to_diff_opinions",
                       "political_orientation_user1", "political_orientation_user2", "political_orientation_user4", "political_orientation_user5")) %>%
    select(term, estimate) %>%
    mutate(term = recode(term,
                         "age" = "Age",
                         "genderMale" = "Gender: Male",
                         "social_media_use" = "Social Media Use",
                         "party_idLabour" = "Party ID: Labour",
                         "party_idConservative" = "Party ID: Conservative",
                         "partyid_strength" = "Party ID Strength",
                         "political_interest" = "Political Interest",
                         "news_diet_diversity_score" = "News Media Diversity Score",
                         "income" = "Income",
                         "university_education" = "University Education",
                         "nonelec_participation" = "Non-Electoral Participation",
                         "social_endorsement" = "Social Endorsement",
                         "exposure_to_diff_opinions" = "Exposure to Different Opinions",
                         "political_orientation_user1" = "Very Conservative",
                         "political_orientation_user2" = "Somewhat Conservative",
                         "political_orientation_user4" = "Somewhat Liberal",
                         "political_orientation_user5" = "Very Liberal"))
  
  return(list(coefs = coefs, final_data = final_data))
}

# Run the simulation 100 times with a progress bar
results <- pbreplicate(n_iterations, run_simulation(1), simplify = FALSE)

# Combine coefficients into a single data frame
combined_coefs <- bind_rows(lapply(results, function(x) x$coefs), .id = "iteration")

# Combine all final data into a single data frame
combined_final_data <- bind_rows(lapply(results, function(x) x$final_data), .id = "iteration")

# Plot the distribution of the estimated effect sizes for all covariates with readable names
plot <- ggplot(combined_coefs, aes(x = estimate, fill = term)) +
  geom_density(alpha = 0.5, color = "black") +
  facet_wrap(~ term, scales = "free") +
  scale_fill_grey(start = 0.6, end = 0.9) +
  labs(
    title = "Distribution of Estimated Effect Size for All Covariates",
    x = "Estimated Coefficient",
    y = "Density",
    fill = "Covariate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

# Print the plot
print(plot)

# Ensure the plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save the plot to the plots directory
ggsave("plots/simulated_effect_sizes_all_covariates.png", plot, width = 14, height = 8)

# Ensure the plots directory exists
if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}

# Save final data to a CSV file
write_csv(combined_final_data, "data/processed/simulated_final_data.csv")
