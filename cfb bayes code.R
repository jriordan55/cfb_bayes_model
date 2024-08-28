# Load libraries
library(ggplot2)        
library(ggridges)       
library(viridis)        
library(bayesplot)      
library(rstan)          
library(tidyverse)      
library(lubridate)      
library(parallel)      
library(reshape2)       
library(loo)            
library(brms)           
library(pbapply)
library(readxl)


# Load the data
data <- read_excel("C:/Users/student/Downloads/cfb_data_2023.xlsx")

# Create a unique game identifier by combining game_id and date
data <- data %>% mutate(game_id_date = paste0(game_id, "_", date))

# Separate home and away team data
home_data <- data %>% filter(venue == "Home")
away_data <- data %>% filter(venue == "Road")

# Join the home and away team data on game_id_date
games <- home_data %>%
    inner_join(away_data, by = "game_id_date", suffix = c("_home", "_away"))

# Select relevant columns and rename them for clarity
games <- games %>%
    select(date = date_home, week = week_home,
           home_team = team_home, away_team = team_away,
           home_total_points = total_points_home, away_total_points = total_points_away,
           home_rush_td = rush_td_home, away_rush_td = rush_td_away,
           home_pass_td = pass_td_home, away_pass_td = pass_td_away,
           home_safeties = safeties_home, away_safeties = safeties_away,
           home_two_pointConv = two_pointConv_home, away_two_pointConv = two_pointConv_away,
           home_extra_points = extra_points_home, away_extra_points = extra_points_away,
           home_field_goals = field_goals_home, away_field_goals = field_goals_away,
           home_defense_td = defense_td_home, away_defense_td = defense_td_away)

# Include defensive touchdowns and rushing touchdowns for each team
games <- games %>%
    mutate(home_rush_td = home_rush_td + home_defense_td,
           away_rush_td = away_rush_td + away_defense_td)

# Calculate estimated points for home and away teams
games <- games %>%
    mutate(home_estimated_points = home_rush_td * 6 + home_pass_td * 6 + home_extra_points * 1 +
               home_field_goals * 3 + home_safeties * 2 + home_two_pointConv * 2,
           away_estimated_points = away_rush_td * 6 + away_pass_td * 6 + away_extra_points * 1 +
               away_field_goals * 3 + away_safeties * 2 + away_two_pointConv * 2)

# Conditional scoring: extra points and 2pt conversions
# Calculate the total number of touchdowns for each game
games <- games %>%
    mutate(home_touchdowns = home_rush_td + home_pass_td,
           away_touchdowns = away_rush_td + away_pass_td)

# Calculate margin of victory and determine winners
rankings <- games %>%
    mutate(home_mov = home_total_points - away_total_points,
           away_mov = away_total_points - home_total_points,
           home_win = ifelse(home_total_points > away_total_points, 1, 0),
           away_win = ifelse(away_total_points > home_total_points, 1, 0))

# Combine home and away data
rankings <- rankings %>%
    select(
        team = home_team,
        points = home_total_points,
        opp_points = away_total_points,
        mov = home_mov,
        win = home_win) %>%
    bind_rows(
        rankings %>%
                  select(
                      team = away_team,
                      points = away_total_points,
                      opp_points = home_total_points,
                      mov = away_mov,
                      win = away_win))

# Calculate average MOV and number of wins
rankings <- rankings %>%
    group_by(team) %>%
    summarise(
        mov = mean(mov),
        wins = sum(win)) %>%
    arrange(desc(mov))

#### 4. Bayesian model selection with brms & LOO-PSIS 
scoring_vars <- games %>%
    select(home_rush_td, away_rush_td, home_pass_td, away_pass_td, home_defense_td, away_defense_td,
           home_extra_points, away_extra_points, home_field_goals, away_field_goals,
           home_safeties, away_safeties, home_two_pointConv, away_two_pointConv) %>%
    gather(key = "type", value = "count")

# Fit Poisson and Negative Binomial models using brms
fit_poisson <- function(data, formula) {
    brm(formula, data = data, family = poisson(), cores = 4, iter = 2000, chains = 4)
}
fit_nbinom <- function(data, formula) {
    brm(formula, data = data, family = negbinomial(), cores = 4, iter = 2000, chains = 4)
}

# Fit models for each scoring variable
scoring_vars_list <- list(
    "rush_td" = scoring_vars %>% filter(type %in% c("home_rush_td", "away_rush_td")),
    "pass_td" = scoring_vars %>% filter(type %in% c("home_pass_td", "away_pass_td")),
    "extra_points" = scoring_vars %>% filter(type %in% c("home_extra_points", "away_extra_points")),
    "field_goals" = scoring_vars %>% filter(type %in% c("home_field_goals", "away_field_goals")),
    "safeties" = scoring_vars %>% filter(type %in% c("home_safeties", "away_safeties")),
    "two_pointConv" = scoring_vars %>% filter(type %in% c("home_two_pointConv", "away_two_pointConv")))

fit_models <- function(data) {
    list(
        poisson = fit_poisson(data, count ~ 1),
        nbinom = fit_nbinom(data, count ~ 1)
    )
}
models <- lapply(scoring_vars_list, fit_models)

# Modeling in Stan
# Create unique team IDs
teams <- unique(c(games$home_team, games$away_team))
team_ids <- setNames(seq_along(teams), teams)

# Map team names to their corresponding IDs
games <- games %>% mutate(
    home_team_id = team_ids[home_team],
    away_team_id = team_ids[away_team])

# Prepare the data for Stan
stan_data <- list(
    N = nrow(games),
    T = length(unique(c(games$home_team_id, games$away_team_id))),
    home_team = games$home_team_id,
    away_team = games$away_team_id,
    home_rush_td = games$home_rush_td,
    away_rush_td = games$away_rush_td,
    home_pass_td = games$home_pass_td,
    away_pass_td = games$away_pass_td,
    home_safeties = games$home_safeties,
    away_safeties = games$away_safeties,
    home_two_pointConv = games$home_two_pointConv,
    away_two_pointConv = games$away_two_pointConv,
    home_extra_points = games$home_extra_points,
    away_extra_points = games$away_extra_points,
    home_field_goals = games$home_field_goals,
    away_field_goals = games$away_field_goals,
    home_touchdowns = games$home_touchdowns,
    away_touchdowns = games$away_touchdowns)

# Specify Stan model
stan_model_code <- "
data {
  int<lower=1> N;                       // Number of games
  int<lower=1> T;                       // Number of teams
  int home_team[N];                     // Home team index
  int away_team[N];                     // Away team index

  int home_rush_td[N];                  // Rush TD home team
  int away_rush_td[N];                  // Rush TD away team
  int home_pass_td[N];                  // Pass TD home team
  int away_pass_td[N];                  // Pass TD away team
  int home_safeties[N];                 // Safeties home team
  int away_safeties[N];                 // Safeties away team
  int home_two_pointConv[N];            // 2PT conversions home team
  int away_two_pointConv[N];            // 2PT conversions away team
  int home_extra_points[N];             // XP home team
  int away_extra_points[N];             // XP away team
  int home_field_goals[N];              // Field goals home team
  int away_field_goals[N];              // Field goals away team
  int home_touchdowns[N];               // Total TDs home team
  int away_touchdowns[N];               // Total TDs away team
}

parameters {
  real<lower=0> home_advantage;         // home advantage effect
  real<lower=0> int_td_pass;            // intercept for Pass TD
  real<lower=0> int_td_rush;            // intercept for Rush TD
  real<lower=0> int_safety;             // intercept for Safeties
  real<lower=0> int_two_pointConv;      // intercept for 2PT conversions
  real<lower=0> int_extra_point;        // intercept for XP
  real<lower=0> int_field_goal;         // intercept for Field goals

  simplex[2] p_attempts;                // probabilities for attempting an XP or a 2PT conversion

  vector[T] att_rush_td_raw;            // Raw OFF abilities for Rush TD
  vector[T] def_rush_td_raw;            // Raw DEF abilities for Rush TD
  vector[T] att_pass_td_raw;            // Raw OFF abilities for Pass TD
  vector[T] def_pass_td_raw;            // Raw DEF abilities for Pass TD
  vector[T] att_safeties_raw;           // Raw OFF abilities for Safety
  vector[T] def_safeties_raw;           // Raw DEF abilities for Safety
  vector[T] att_two_pointConv_raw;      // Raw OFF abilities for 2PT conversion
  vector[T] def_two_pointConv_raw;      // Raw DEF abilities for 2PT conversion
  vector[T] att_extra_points_raw;       // Raw OFF abilities for XP
  vector[T] def_extra_points_raw;       // Raw DEF abilities for XP
  vector[T] att_field_goals_raw;        // Raw OFF abilities for Field goal
  vector[T] def_field_goals_raw;        // Raw OFF abilities for Field goal
}

transformed parameters {
  vector[T] att_rush_td = att_rush_td_raw - mean(att_rush_td_raw);                   // centered OFF Rush TD
  vector[T] def_rush_td = def_rush_td_raw - mean(def_rush_td_raw);                   // centered DEF Rush TD
  vector[T] att_pass_td = att_pass_td_raw - mean(att_pass_td_raw);                   // centered OFF Pass TD
  vector[T] def_pass_td = def_pass_td_raw - mean(def_pass_td_raw);                   // centered DEF Pass TD
  vector[T] att_safeties = att_safeties_raw - mean(att_safeties_raw);                // centered OFF Safety
  vector[T] def_safeties = def_safeties_raw - mean(def_safeties_raw);                // centered DEF Safety
  vector[T] att_two_pointConv = att_two_pointConv_raw - mean(att_two_pointConv_raw); // centered OFF 2PT conversion
  vector[T] def_two_pointConv = def_two_pointConv_raw - mean(def_two_pointConv_raw); // centered DEF 2PT conversion
  vector[T] att_extra_points = att_extra_points_raw - mean(att_extra_points_raw);    // centered OFF XP
  vector[T] def_extra_points = def_extra_points_raw - mean(def_extra_points_raw);    // centered DEF XP
  vector[T] att_field_goals = att_field_goals_raw - mean(att_field_goals_raw);       // centered OFF Field goal
  vector[T] def_field_goals = def_field_goals_raw - mean(def_field_goals_raw);       // centered DEF Field goal

  real p_extra_point = p_attempts[1];               // probability of going for XP
  real p_two_pointConv = p_attempts[2];             // probability of going for 2PT conversion
}

model {
  // Priors
  home_advantage ~ normal(0, 0.2);                  // Home advantage prior
  int_td_pass ~ normal(0, 0.2);                     // Pass TD intercept prior
  int_td_rush ~ normal(0, 0.2);                     // Rush TD intercept prior
  int_safety ~ normal(0, 0.2);                      // Safety intercept prior
  int_two_pointConv ~ normal(0, 0.2);               // 2PT conversion intercept prior
  int_extra_point ~ normal(0, 0.2);                 // XP intercept prior
  int_field_goal ~ normal(0, 0.2);                  // Field goal intercept prior

  p_attempts ~ dirichlet([9, 1]);                   // Post TD score attempt prior (90% XP) & (10% 2PT conversion)

  att_rush_td_raw ~ normal(0, 1);                   // OFF Rush TD pripr
  def_rush_td_raw ~ normal(0, 1);                   // DEF Rush TD prior
  att_pass_td_raw ~ normal(0, 1);                   // OFF Pass TD prior
  def_pass_td_raw ~ normal(0, 1);                   // DEF Pass TD prior
  att_safeties_raw ~ normal(0, 1);                  // OFF Safety prior
  def_safeties_raw ~ normal(0, 1);                  // DEF Safety prior
  att_two_pointConv_raw ~ normal(0, 1);             // OFF 2PT conversion prior
  def_two_pointConv_raw ~ normal(0, 1);             // DEF 2PT conversion prior
  att_extra_points_raw ~ normal(0, 1);              // OFF XP prior
  def_extra_points_raw ~ normal(0, 1);              // DEF XP prior
  att_field_goals_raw ~ normal(0, 1);               // OFF Field goal prior
  def_field_goals_raw ~ normal(0, 1);               // DEF Field goal prior

  // Likelihoods

  // Rush TD
  home_rush_td ~ poisson_log(att_rush_td[home_team] + def_rush_td[away_team] + home_advantage + int_td_rush);
  away_rush_td ~ poisson_log(att_rush_td[away_team] + def_rush_td[home_team] + int_td_rush);

  // Pass TD
  home_pass_td ~ poisson_log(att_pass_td[home_team] + def_pass_td[away_team] + home_advantage + int_td_pass);
  away_pass_td ~ poisson_log(att_pass_td[away_team] + def_pass_td[home_team] + int_td_pass);

  // Safety
  home_safeties ~ poisson_log(att_safeties[home_team] + def_safeties[away_team] + int_safety);
  away_safeties ~ poisson_log(att_safeties[away_team] + def_safeties[home_team] + int_safety);

  // XP & 2PT conversion
  for (i in 1:N) {
    real home_td_real = home_touchdowns[i];
    real away_td_real = away_touchdowns[i];
    home_two_pointConv[i] ~ poisson_log((att_two_pointConv[home_team[i]] + def_two_pointConv[away_team[i]] + int_two_pointConv) + log(p_two_pointConv) + log(home_td_real + 1e-10));
    home_extra_points[i] ~ poisson_log((att_extra_points[home_team[i]] + def_extra_points[away_team[i]] + int_extra_point) + log(p_extra_point) + log(home_td_real + 1e-10));
    away_two_pointConv[i] ~ poisson_log((att_two_pointConv[away_team[i]] + def_two_pointConv[home_team[i]] + int_two_pointConv) + log(p_two_pointConv) + log(away_td_real + 1e-10));
    away_extra_points[i] ~ poisson_log((att_extra_points[away_team[i]] + def_extra_points[home_team[i]] + int_extra_point) + log(p_extra_point) + log(away_td_real + 1e-10));
  }

  // Field goal
  home_field_goals ~ poisson_log(att_field_goals[home_team] + def_field_goals[away_team] + home_advantage + int_field_goal);
  away_field_goals ~ poisson_log(att_field_goals[away_team] + def_field_goals[home_team] + int_field_goal);
}
"

# fit Stan model with mcmc
fit <- stan(
    model_code = stan_model_code,
    data = stan_data,
    iter = 15000,
    warmup = 5000,
    chains = 4,
    cores = 6,
    seed = 1,
    init = "random",
    control = list(max_treedepth = 12))

# Print Stan fit
print(fit)

# Extract parameters from the fitted model
posterior <- rstan::extract(fit)

# Prepare data for plotting
team_strengths <- data.frame(
    team = rep(teams, each = nrow(posterior$att_rush_td)),
    att_rush_td = c(posterior$att_rush_td),
    def_rush_td = c(posterior$def_rush_td),
    att_pass_td = c(posterior$att_pass_td),
    def_pass_td = c(posterior$def_pass_td),
    att_safeties = c(posterior$att_safeties),
    def_safeties = c(posterior$def_safeties),
    att_two_pointConv = c(posterior$att_two_pointConv),
    def_two_pointConv = c(posterior$def_two_pointConv),
    att_extra_points = c(posterior$att_extra_points),
    def_extra_points = c(posterior$def_extra_points),
    att_field_goals = c(posterior$att_field_goals),
    def_field_goals = c(posterior$def_field_goals))

# Calculate the aggregate ratings for each team
team_strengths_agg <- data.frame(
    team = rep(teams, each = nrow(posterior$att_rush_td)),
    rush_td_diff = c(posterior$att_rush_td - posterior$def_rush_td),
    pass_td_diff = c(posterior$att_pass_td - posterior$def_pass_td),
    safeties_diff = c(posterior$att_safeties - posterior$def_safeties),
    two_pointConv_diff = c(posterior$att_two_pointConv - posterior$def_two_pointConv),
    extra_points_diff = c(posterior$att_extra_points - posterior$def_extra_points),
    field_goals_diff = c(posterior$att_field_goals - posterior$def_field_goals))

# Convert data to long format for ggridges
team_strengths_agg_long <- team_strengths_agg %>%
    pivot_longer(cols = c(rush_td_diff, pass_td_diff, safeties_diff, two_pointConv_diff, extra_points_diff, field_goals_diff), names_to = "metric", values_to = "value")

# Simulating future games
# Extract parameters from the fitted model
posterior <- rstan::extract(fit)
# Prepare the parameters for simulation using the full posterior
params <- list(
    att_rush_td = posterior$att_rush_td,
    def_rush_td = posterior$def_rush_td,
    att_pass_td = posterior$att_pass_td,
    def_pass_td = posterior$def_pass_td,
    att_safeties = posterior$att_safeties,
    def_safeties = posterior$def_safeties,
    att_two_pointConv = posterior$att_two_pointConv,
    def_two_pointConv = posterior$def_two_pointConv,
    att_extra_points = posterior$att_extra_points,
    def_extra_points = posterior$def_extra_points,
    att_field_goals = posterior$att_field_goals,
    def_field_goals = posterior$def_field_goals,
    home_advantage = posterior$home_advantage,
    int_td_rush = posterior$int_td_rush,
    int_td_pass = posterior$int_td_pass,
    int_safety = posterior$int_safety,
    int_two_pointConv = posterior$int_two_pointConv,
    int_extra_point = posterior$int_extra_point,
    int_field_goal = posterior$int_field_goal,
    p_extra_point = posterior$p_extra_point,
    p_two_pointConv = posterior$p_two_pointConv)

# Team names and ids
teams <- unique(c(games$home_team, games$away_team))
team_ids <- setNames(1:length(teams), teams)

# Function to map team names to team IDs
get_team_id <- function(team_name, team_ids) {
    return(team_ids[[team_name]])
}

# Function to simulate scores
simulate_scores <- function(home, att, def, home_advantage, int) {
    log_mean <- ifelse(home == 1, att + def + home_advantage + int + 1e-6, att + def + int + 1e-6)
    log_mean[is.na(log_mean)] <- 0 # Handle NA log_mean
    log_mean[!is.finite(log_mean)] <- 0 # Handle infinite log_mean
    mean <- exp(log_mean)
    mean <- ifelse(mean < 0, 0.1, mean) # Ensure mean is non-negative
    mean[is.na(mean)] <- 0.1 # Handle NA mean
    mean[!is.finite(mean)] <- 0.1 # Handle infinite mean
    score <- rpois(1, lambda = mean)

    return(score)
}

# Function to simulate home team scores
simulate_home_scores <- function(home_team_id, away_team_id, params, n_simulations = 10000) {
    idx <- sample(1:length(params$home_advantage), n_simulations, replace = TRUE)
    home_advantage <- params$home_advantage[idx]

    scores <- data.frame(
        rush_td = numeric(n_simulations),
        pass_td = numeric(n_simulations),
        safeties = numeric(n_simulations),
        two_pointConv = numeric(n_simulations),
        extra_points = numeric(n_simulations),
        field_goals = numeric(n_simulations)
    )

    scores$rush_td <- pbsapply(1:n_simulations, function(i) simulate_scores(1, params$att_rush_td[idx[i], home_team_id], params$def_rush_td[idx[i], away_team_id], home_advantage[i], params$int_td_rush[idx[i]]))
    scores$pass_td <- pbsapply(1:n_simulations, function(i) simulate_scores(1, params$att_pass_td[idx[i], home_team_id], params$def_pass_td[idx[i], away_team_id], home_advantage[i], params$int_td_pass[idx[i]]))
    scores$safeties <- pbsapply(1:n_simulations, function(i) simulate_scores(1, params$att_safeties[idx[i], home_team_id], params$def_safeties[idx[i], away_team_id], home_advantage[i], params$int_safety[idx[i]]))
    scores$field_goals <- pbsapply(1:n_simulations, function(i) simulate_scores(1, params$att_field_goals[idx[i], home_team_id], params$def_field_goals[idx[i], away_team_id], home_advantage[i], params$int_field_goal[idx[i]]))

    total_td <- rowSums(scores[, c("rush_td", "pass_td")], na.rm = TRUE)

    scores$extra_points <- pbsapply(1:n_simulations, function(i) {
        td <- total_td[i]
        if (!is.na(td) && td > 0) {
            sum(runif(td) < params$p_extra_point[idx[i]]) * simulate_scores(1, params$att_extra_points[idx[i], home_team_id], params$def_extra_points[idx[i], away_team_id], home_advantage[i], params$int_extra_point[idx[i]])
        } else {
            return(0)
        }
    })

    scores$two_pointConv <- pbsapply(1:n_simulations, function(i) {
        td <- total_td[i]
        if (!is.na(td) && td > 0) {
            sum(runif(td) >= params$p_extra_point[idx[i]]) * simulate_scores(1, params$att_two_pointConv[idx[i], home_team_id], params$def_two_pointConv[idx[i], away_team_id], home_advantage[i], params$int_two_pointConv[idx[i]])
        } else {
            return(0)
        }
    })

    return(scores)
}

# Function to simulate away team scores
simulate_away_scores <- function(home_team_id, away_team_id, params, n_simulations = 10000) {
    idx <- sample(1:length(params$home_advantage), n_simulations, replace = TRUE)

    scores <- data.frame(
        rush_td = numeric(n_simulations),
        pass_td = numeric(n_simulations),
        safeties = numeric(n_simulations),
        two_pointConv = numeric(n_simulations),
        extra_points = numeric(n_simulations),
        field_goals = numeric(n_simulations)
    )

    scores$rush_td <- pbsapply(1:n_simulations, function(i) simulate_scores(0, params$att_rush_td[idx[i], away_team_id], params$def_rush_td[idx[i], home_team_id], 0, params$int_td_rush[idx[i]]))
    scores$pass_td <- pbsapply(1:n_simulations, function(i) simulate_scores(0, params$att_pass_td[idx[i], away_team_id], params$def_pass_td[idx[i], home_team_id], 0, params$int_td_pass[idx[i]]))
    scores$safeties <- pbsapply(1:n_simulations, function(i) simulate_scores(0, params$att_safeties[idx[i], away_team_id], params$def_safeties[idx[i], home_team_id], 0, params$int_safety[idx[i]]))
    scores$field_goals <- pbsapply(1:n_simulations, function(i) simulate_scores(0, params$att_field_goals[idx[i], away_team_id], params$def_field_goals[idx[i], home_team_id], 0, params$int_field_goal[idx[i]]))

    total_td <- rowSums(scores[, c("rush_td", "pass_td")], na.rm = TRUE)

    scores$extra_points <- pbsapply(1:n_simulations, function(i) {
        td <- total_td[i]
        if (!is.na(td) && td > 0) {
            sum(runif(td) < params$p_extra_point[idx[i]]) * simulate_scores(0, params$att_extra_points[idx[i], away_team_id], params$def_extra_points[idx[i], home_team_id], 0, params$int_extra_point[idx[i]])
        } else {
            return(0)
        }
    })

    scores$two_pointConv <- pbsapply(1:n_simulations, function(i) {
        td <- total_td[i]
        if (!is.na(td) && td > 0) {
            sum(runif(td) >= params$p_extra_point[idx[i]]) * simulate_scores(0, params$att_two_pointConv[idx[i], away_team_id], params$def_two_pointConv[idx[i], home_team_id], 0, params$int_two_pointConv[idx[i]])
        } else {
            return(0)
        }
    })

    return(scores)
}

# Function to simulate overtime
simulate_overtime <- function(home_team_id, away_team_id, params) {
    # Scale factor for overtime performance (16.6% of regulation time)
    ot_scale <- 0.166

    # Simulate 50/50 coin toss to determine the winner
    coin_toss_winner <- sample(c("home", "away"), 1, prob = c(0.5, 0.5))

    # Simulate coin toss winner's decision to take first possession with 60% possession probability
    first_possession_team <- if (runif(1) < 0.6) coin_toss_winner else if (coin_toss_winner == "home") "away" else "home"

    # Simulate first possession
    if (first_possession_team == "home") {
        first_possession <- simulate_home_scores(home_team_id, away_team_id, params, n_simulations = 1)
        first_possession_points <- round((first_possession$rush_td * 6 +
                                              first_possession$pass_td * 6 +
                                              first_possession$field_goals * 3) * ot_scale)
        if (first_possession_points >= 6) {
            return(list(result = "home_win", points = c(home = first_possession_points, away = 0)))
        }
    } else {
        first_possession <- simulate_away_scores(home_team_id, away_team_id, params, n_simulations = 1)
        first_possession_points <- round((first_possession$rush_td * 6 +
                                              first_possession$pass_td * 6 +
                                              first_possession$field_goals * 3 ) * ot_scale)
        if (first_possession_points >= 6) {
            return(list(result = "away_win", points = c(home = 0, away = first_possession_points)))
        }
    }

    # Simulate second possession if first possession did not result in a touchdown
    if (first_possession_team == "home") {
        second_possession <- simulate_away_scores(home_team_id, away_team_id, params, n_simulations = 1)
        second_possession_points <- round((second_possession$rush_td * 6 +
                                               second_possession$pass_td * 6 +
                                               second_possession$field_goals * 3) * ot_scale)
    } else {
        second_possession <- simulate_home_scores(home_team_id, away_team_id, params, n_simulations = 1)
        second_possession_points <- round((second_possession$rush_td * 6 +
                                               second_possession$pass_td * 6 +
                                               second_possession$field_goals * 3) * ot_scale)
    }

    # Determine outcome after both possessions
    if (first_possession_team == "home") {
        if (first_possession_points > second_possession_points) {
            return(list(result = "home_win", points = c(home = first_possession_points, away = second_possession_points)))
        } else if (second_possession_points > first_possession_points) {
            return(list(result = "away_win", points = c(home = first_possession_points, away = second_possession_points)))
        } else {
            return(list(result = "tie", points = c(home = first_possession_points, away = second_possession_points)))
        }
    } else {
        if (second_possession_points > first_possession_points) {
            return(list(result = "home_win", points = c(home = second_possession_points, away = first_possession_points)))
        } else if (first_possession_points > second_possession_points) {
            return(list(result = "away_win", points = c(home = second_possession_points, away = first_possession_points)))
        } else {
            return(list(result = "tie", points = c(home = second_possession_points, away = first_possession_points)))
        }
    }
}

# Function to simulate a full matchup including potential overtimes
simulate_matchup <- function(home_team_name, away_team_name, params, team_ids, n_simulations = 10000) {
    home_team_id <- get_team_id(home_team_name, team_ids)
    away_team_id <- get_team_id(away_team_name, team_ids)

    # Simulate regulation time scores
    home_scores <- simulate_home_scores(home_team_id, away_team_id, params, n_simulations)
    away_scores <- simulate_away_scores(home_team_id, away_team_id, params, n_simulations)

    # Calculate points for regulation time
    home_points_reg <- home_scores$rush_td * 6 +
        home_scores$pass_td * 6 +
        home_scores$extra_points * 1 +
        home_scores$two_pointConv * 2 +
        home_scores$field_goals * 3 +
        home_scores$safeties * 2

    away_points_reg <- away_scores$rush_td * 6 +
        away_scores$pass_td * 6 +
        away_scores$extra_points * 1 +
        away_scores$two_pointConv * 2 +
        away_scores$field_goals * 3 +
        away_scores$safeties * 2

    # Initialize overtime points and outcomes
    home_points_ot <- rep(0, n_simulations)
    away_points_ot <- rep(0, n_simulations)
    ot_outcomes <- rep("none", n_simulations)

    # Identify ties and simulate overtime if necessary
    ties <- home_points_reg == away_points_reg

    for (i in which(ties)) {
        ot_result <- simulate_overtime(home_team_id, away_team_id, params)
        home_points_ot[i] <- ot_result$points["home"]
        away_points_ot[i] <- ot_result$points["away"]
        ot_outcomes[i] <- ot_result$result
    }

    # Calculate total points including overtime
    home_points <- home_points_reg + home_points_ot
    away_points <- away_points_reg + away_points_ot

    return(data.frame(home_points, away_points, ot_outcomes))
}

calculate_metrics <- function(results, n_simulations = 10000) {
  # Calculate win counts
  home_win_count <- sum(results$home_points > results$away_points)
  away_win_count <- sum(results$away_points > results$home_points)
  total_games <- nrow(results)
  
  # Calculate adjusted probabilities
  if (total_games > 0) {
    home_win_prob <- round((home_win_count / total_games) * 100, 2)
    away_win_prob <- round((away_win_count / total_games) * 100, 2)
    tie_prob <- round(mean(results$ot_outcomes == "tie") * 100, 2)
  } else {
    home_win_prob <- 0
    away_win_prob <- 0
    tie_prob <- 0
  }
  
  # Ensure that home_win_prob + away_win_prob + tie_prob == 100%
  prob_sum <- home_win_prob + away_win_prob + tie_prob
  if (prob_sum != 100) {
    # Adjust the probabilities to sum up to 100
    adjustment <- 100 - prob_sum
    if (home_win_prob > away_win_prob) {
      home_win_prob <- home_win_prob + adjustment
    } else {
      away_win_prob <- away_win_prob + adjustment
    }
  }
  
  # Calculate other metrics
  home_team_total <- median(results$home_points)
  away_team_total <- median(results$away_points)
  full_game_total <- median(results$home_points + results$away_points)
  home_spread <- median(results$home_points - results$away_points) * (-1)
  away_spread <- median(results$away_points - results$home_points) * (-1)
  
  metrics <- data.frame(
    Metric = c("Home Win Probability", "Away Win Probability", 
               "Tie Probability", "Home Expected Points", "Away Expected Points", 
               "Total Expected Points", "Home Expected Spread", "Away Expected Spread"),
    Result = c(
      sprintf("%.2f%%", home_win_prob), # Format as percentage
      sprintf("%.2f%%", away_win_prob), # Format as percentage
      sprintf("%.2f%%", tie_prob), # Format as percentage
      home_team_total,
      away_team_total,
      full_game_total,
      home_spread,
      away_spread
    )
  )
  
  return(metrics)
}

# Example team names
home_team_name <- "Georgia"
away_team_name <- "Alabama"

# Simulate matchups
simulated_games <- simulate_matchup(home_team_name, away_team_name, params, team_ids, n_simulations = 10000)

# Calculate metrics
metrics <- calculate_metrics(simulated_games, n_simulations)

# Print metrics
print(metrics)