setwd("C:/Users/georg/Desktop/George/FGV/5º Período/Final_Project_SM")

install.packages("tidyverse")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("conflicted")
install.packages("caret")
install.packages("betareg")

library(betareg)
library(caret)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Load the data
data <- read.csv("data/game_info_treated.csv", header = TRUE, sep = ",")
# take off the ratings column
data <- data[, !(names(data) %in% c("rating"))]

data$metacritics_scaled <- data$metacritics / 100
# Introduce a small epsilon value
epsilon <- 0.0001

# Adjust the scaled ratings to avoid 0 and 1
data$rating_scaled <- ifelse(data$rating_scaled == 0, epsilon,
                             ifelse(data$rating_scaled == 1, 1 - epsilon, data$rating_scaled))


# make a glm model with family beta
model <- betareg(rating_scaled ~ platform_Mobile + 
                   platform_Nintendo +
                   platform_Other +
                   platform_PC +
                   platform_PlayStation +
                   platform_Xbox +
                   playtime + 
                   achievements_count + 
                   game_series_count + 
                   esrb_rating +
                   genre_Action +
                   genre_Adventure +
                   genre_Arcade +
                   genre_Board_Games +
                   genre_Card +
                   genre_Casual +
                   genre_Educational +
                   genre_Family +
                   genre_Fighting +
                   genre_Indie +
                   genre_Massively_Multiplayer +
                   genre_Platformer +
                   genre_Puzzle +
                   genre_RPG +
                   genre_Racing +
                   genre_Shooter +
                   genre_Simulation +
                   genre_Sports +
                   genre_Strategy,
                 data = data, link = "logit")
summary(model)





