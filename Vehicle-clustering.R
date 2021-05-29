library(tidyverse)

library(readxl)

library(NbClust)

library(knitr)

library(tidymodels)

library(flexclust)

library(funtimes)

theme_set(theme_light())

#setwd("E:\IIT 2nd Year\Data Mining\CW")

# Read in the original excel datafile

vehicles_original <- read_csv("vehicles.csv") %>%
  
  janitor::clean_names() %>%
  
  #https://www.rdocumentation.org/packages/janitor/versions/1.2.0/topics/clean_names
  
  mutate(class = as_factor(class))

# Get a birds eye view of how the dataset looks like

summary(vehicles_original)



vehicles_original %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  #mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'van'")





vehicles_original %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'bus'")





vehicles_original %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: saab")





vehicles_original %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: opel")



vehicles_bus = vehicles_original %>%
  
  filter(class == "bus") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_van = vehicles_original %>%
  
  filter(class == "van") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_opel = vehicles_original %>%
  
  filter(class == "opel") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_saab = vehicles_original %>%
  
  filter(class == "saab") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  
  arrange(samples)



print(combined)


combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers class: 'van'")


combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers class: 'bus'")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: saab")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: opel")



# Remove the sample name and the class name. Both of these will be remove so that only n

#umerical data is left for the algorithm.

vehicles_data_points = combined %>%
  
  select(-samples, -class)

# Now that we have the "vehicles_data_points" dataset, scaling is performed

vehicles_scaled = vehicles_data_points %>%
  
  mutate(across(everything(), scale))

summary(vehicles_scaled)

library(stats)
pc <- princomp(vehicles_scaled)

plot(pc)
plot(pc, type='l')
summary(pc)


pc <- prcomp(vehicles_scaled)
# First for principal components
comp <- data.frame(pc$x[,1:2])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

set.seed(123)

# Perform the kmeans using the NbClust function

# Use Euclidean for distance

cluster_euclidean = NbClust(comp,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

table(cluster_euclidean$Best.partition,vehicles_original$class)

# Use manhattan for distance

cluster_manhattan = NbClust(comp,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")
table(cluster_manhattan$Best.partition,vehicles_original$class)

library(factoextra)
library(NbClust)

fviz_nbclust(comp, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#########################################################

set.seed(1234)
fit.km <- kmeans(vehicles_scaled, centers=4,  nstart=25)
fit.km$size

fit.km$centers
plot(fit.km$centers)

fit.km2 <- kmeans(vehicles_scaled, centers=3,  nstart=25)
fit.km2$size

fit.km2$centers
plot(fit.km2$centers)

fit.km3 <- kmeans(vehicles_scaled, centers=2,  nstart=25)
fit.km3$size

fit.km3$centers
plot(fit.km3$centers)