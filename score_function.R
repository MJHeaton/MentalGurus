################
### Function ###
################

### Loading Packages
library(tidyverse)

# #### ----
### Loading Necessary Data
sumRes2 <- read_csv("data/clean.csv") %>% select(-X1)
load("get_match_good2.RData")


### Takes Raw Question Responses, Gives Probability Match with each Personality
get_match <- function(all_scores) {
  # Covariance is calculated by the observations that are matched with at least 90% probability
  if(!require("mvtnorm")) install.packages("mvtnorm") ; library(mvtnorm)
  load("get_match_good2.RData")
  temp_scores <- setNames(all_scores, paste0(rep(c("EXT", "EST", "AGR", "CSN", "OPN"), each = 10), 1:10))
  temp_scores[neg.quest] <- (-1 * temp_scores[neg.quest]) + 6
  scores <- rowSums(matrix(temp_scores, byrow = TRUE, nrow = 5))
  std_scores <- (scores - scaling_means)/scaling_sd
  probs <- c()
  for(i in 1:16) {
    probs <- append(probs, dmvnorm(std_scores, mus[i,], cov.list[[i]]) * prob.weight[i])
  }
  final_probs <- setNames(probs / sum(probs),1:16)
  return(final_probs)
}

### Takes Raw Question Responses, Gives Composite Scores
composite_scores <- function(all_scores) {
  load("get_match_good2.RData")
  temp_scores <- setNames(all_scores, paste0(rep(c("EXT", "EST", "AGR", "CSN", "OPN"), each = 10), 1:10))
  temp_scores[neg.quest] <- (-1 * temp_scores[neg.quest]) + 6
  scores <- rowSums(matrix(temp_scores, byrow = TRUE, nrow = 5))
  return(scores)
}



graph_match <- function(probs) {
  data <- data.frame(
    prob = probs * 100,
    cluster = 1:16,
    grouping = rep(1:4, each = 4))
  
  
  # Set a number of 'empty bar'
  empty_bar <- 0
  
  # Add lines to the initial dataset
  to_add <- matrix(NA, empty_bar, ncol(data))
  colnames(to_add) <- colnames(data)
  data <- rbind(data, to_add)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=prob)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", aes(fill = as.factor(grouping))) +#), fill=alpha("green", 0.3)) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "None",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar(start = 0) + 
    geom_text(data=label_data, aes(x=id, y=prob+10, label=cluster, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    labs(main = "Cluster Assignment Probabilities")
  return(p)
}
