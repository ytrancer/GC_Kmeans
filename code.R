library(readxl)
library(tidyverse)
library(factoextra)

# Load data

df<- read_excel("Clustering_CaseStudy_28042020.xlsx")

#EDA

str(df)
summary(df)
 # A very small number of NAs.  As the data is categorical we cannot impute NAs,
# therefore, they will be removed.

summary(df$`Service Offering`)

# Select and rename columns for modeling and remove NAs

df <- df %>% select(-`Sales Channel`, -`Sales Sub Channel`, -`Date of Birth`,
                    -`Gender`, -`Device`, -`Service Offering`, -`Age Group`) %>% 
             rename("Age" = `Age Score`, "Experience" = `Trading Experience Score`,
                    "NWScore" = `Net Worth Score`, "Income" = `Income Score`) %>% 
             drop_na()

# Standardise values

df_clean <- df[2:6]
z_df <- as.data.frame(lapply(df_clean, scale))

# Training model

RNGversion("3.5.2") # for reproducibility
set.seed(123)

df_clusters <- kmeans(z_df, 5)

# Examine model

# Function for Elbow Method

wssplot <- function(data, nc = 15, set.seed = 1234){
  wss <- (nrow(data) - 1)*sum(apply(data, 2, var))
  for(i in 2:nc) {
    set.seed(1234)
    wss[i] <- sum(kmeans(x = data, centers = i, nstart = 25)$withinss)
  }
  plot(1:nc, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Group Sum of Square',
       main = 'Elbow Method Plot to Find Optimal Number of Clusters', frame.plot = T,
       col = 'blue', lwd = 1.5)
}

wssplot(z_df)

# fviz(x = z_df, FUNcluster = kmeans, method = 'wss')
fviz_cluster(df_clusters, data = z_df)
