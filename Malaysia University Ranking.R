#install.packages("repr")

library(dplyr)
library(tidyr)

rank_uni <- read.csv("Malaysia_Uni.csv")
rank_uni  
str(rank_uni)

rank_uni$Institution <- as.character(rank_uni$Institution)
rank_uni$Country <- as.character(rank_uni$Country)
rank_uni$World.Rank <- as.factor(rank_uni$World.Rank)

rank_uni


