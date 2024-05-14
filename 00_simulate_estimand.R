# Load necessary libraries
library(tidyverse)
library(broom)
library(uuid)
library(pbapply)

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
    social_media_frequency = sample(c("several times a week", "once a week", "once a month"), n_users, replace = TRUE),
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
    political_orientation_tweet_binary ~ age + gender + social_media_frequency + political_orientation_user,
    data = final_data,
    family = binomial
  )
  
  # Extract coefficients for political_orientation_user4 and political_orientation_user5
  coefs <- tidy(model) %>%
    filter(term %in% c("political_orientation_user1", "political_orientation_user2", 
                       "political_orientation_user4", "political_orientation_user5")) %>%
    select(term, estimate)
  
  return(list(coefs = coefs, final_data = final_data))
}

# Run the simulation 100 times with a progress bar
results <- pbreplicate(n_iterations, run_simulation(1), simplify = FALSE)

# Combine coefficients into a single data frame
combined_coefs <- bind_rows(lapply(results, function(x) x$coefs), .id = "iteration")

# Combine all final data into a single data frame
combined_final_data <- bind_rows(lapply(results, function(x) x$final_data), .id = "iteration")

# Convert coefficients to a data frame
coef_df <- combined_coefs %>%
  mutate(term = factor(recode(term, 
                              `political_orientation_user1` = "Very conservative",
                              `political_orientation_user2` = "Somewhat conservative",
                              `political_orientation_user4` = "Somewhat liberal",
                              `political_orientation_user5` = "Very liberal"),
                       levels = c("Somewhat liberal", "Very liberal", "Somewhat conservative", "Very conservative")))

# Calculate mean effect size for each term
mean_effects <- coef_df %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate))

# Define common x-axis limits
x_limits <- range(coef_df$estimate, na.rm = TRUE)

# Plot the distribution of the estimated effect sizes as kernel density plots
plot <- ggplot(coef_df, aes(x = estimate, fill = term)) +
  geom_density(alpha = 0.5, color = "black") +
  geom_vline(data = mean_effects, aes(xintercept = mean_estimate, color = term), linetype = "dotted", size = 1) +
  geom_text(data = mean_effects, aes(x = mean_estimate, y = 0.1, label = round(mean_estimate, 3), color = term),
            vjust = -0.5, hjust = 1.2, size = 3, show.legend = FALSE) +
  facet_wrap(~ term, scales = "fixed") +
  scale_x_continuous(limits = x_limits) +
  scale_fill_grey(start = 0.6, end = 0.9) +
  scale_color_grey(start = 0.2, end = 0.4) +
  labs(
    title = "Distribution of estimated effect size by user ideology",
    x = "Estimated coefficient",
    y = "Density",
    fill = "User political orientation",
    color = "Mean"
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
ggsave("plots/simulated_effect_sizes.png", plot, width = 10, height = 6)

# Ensure the plots directory exists
if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}

# Save final data to a CSV file
write_csv(combined_final_data, "data/processed/simulated_final_data.csv")
