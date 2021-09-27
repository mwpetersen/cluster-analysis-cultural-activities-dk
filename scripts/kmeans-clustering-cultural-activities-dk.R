
library(tidyverse)
library(readxl)
library(clusterability)
library(factoextra)
library(NbClust)
library(mapDK)

# Import data -------------------------------------------------------------

df_cultural_activities <- read_excel("data/use-of-cultural-activities-dk-2019.xlsx", skip = 2) %>%
  select(-1)

# Wrangle data ------------------------------------------------------------

df_cultural_activities <- df_cultural_activities %>%
  rename(kommune = `...2`) %>%
  filter(!str_detect(kommune, 'Region|Province'))

df_cultural_activities <- data.frame(df_cultural_activities, row.names = 1)

# K-means clustering ------------------------------------------------------


df_cultural_activities <- as.data.frame(scale(df_cultural_activities))

clusterabilitytest(df_cultural_activities, "dip")

nc <- NbClust(df_cultural_activities, min.nc=2, max.nc = 15, method = "kmeans")

fviz_nbclust(nc)

km_fit <- kmeans(df_cultural_activities, 2, nstart = 25)

test <- km_fit$cluster

df_cultural_activities$cluster <- test

df_cultural_activities %>%
  count(cluster)

df_cultural_activities$kommune <- rownames(df_cultural_activities)  

mapDK(values = "cluster", id = "kommune", data = df_cultural_activities)
  