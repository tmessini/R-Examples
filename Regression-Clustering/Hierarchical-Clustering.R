###########################################
#### Topic 3: Hierarchical Clustering  ####
###########################################

########################
#### [a] Dissimilarity 
########################

### Load mtcars dataset
data(mtcars)

show(mtcars)
### Standardize (z-score) the data
mtcars_scaled <- scale(mtcars)
show(mtcars_scaled)


##################### VALIDATION #####################
#### Validation calculate Z-score manually
# Load the mtcars dataset
data(mtcars)

# Compute z-scores manually for all numeric columns
z_scores_mtcars <- as.data.frame(lapply(mtcars, function(x) (x - mean(x)) / sd(x)))

# Print the first few rows of the transformed dataset
print(head(z_scores_mtcars))
#####################################################


### Compute the Euclidean distance
dist_euclid <- dist(mtcars_scaled, method = "euclidean")
show(dist_euclid)

### Convert 'dist' object to a full matrix for easy indexing
dist_mat <- as.matrix(dist_euclid)
show(dist_mat)

### Extract distances from "Mazda RX4" to each "Merc 450" model
distances <- dist_mat["Mazda RX4", c("Merc 450SE", "Merc 450SL", "Merc 450SLC")]
show(round(distances, 3))

#distances

#######################################
#### [b] Linkage Methods & Dendrograms
#######################################

#Hierarchical clustering: single linkage
hc_single <- hclust(dist_euclid, method = "single")

#Hierarchical clustering: complete linkage
hc_complete <- hclust(dist_euclid, method = "complete")

#Plot the single linkage dendrogram
plot(hc_single, main = "Single Linkage Dendrogram",
     xlab = "", sub = "", cex = 0.8)

plot(hc_single, 
     main = "Single Linkage Dendrogram", 
     xlab = "Car Models", 
     sub = "Data: mtcars (standardized) | Distance: Euclidean | Method: single linkage",
     cex = 0.8)

#Plot the complete linkage dendrogram
plot(hc_complete, 
     main = "Complete Linkage Dendrogram", 
     xlab = "Car Models", 
     sub = "Data: mtcars (standardized) | Distance: Euclidean | Method: complete linkage",
     cex = 0.8)

#######################################
#### [c] Cut the single and complete lingakge dendrograms into 4 clusters and display the number of observations
#### for each cluster
#######################################

#Cut each dendrogram into k=4 clusters
single_clusters   <- cutree(hc_single, k = 4)
complete_clusters <- cutree(hc_complete, k = 4)

#Show the size (number of cars) in each of the 4 clusters
table(single_clusters)
table(complete_clusters)


## Single Linkage
print("Single Linkage Clusters:\n")
# split( ) groups rownames by their single_clusters value
single_list <- split(rownames(mtcars), single_clusters)
show(single_list)

complete_list <- split(rownames(mtcars), complete_clusters)
complete_list

#######################################
#### [d] calculate the means of features 'cyl' and 'hp' of clusters 1 and 3
#######################################

### Calculation for Single Linkage ###
single_mean_cyl_1 <- mean(mtcars[single_clusters == 1, "cyl"])
single_mean_hp_1  <- mean(mtcars[single_clusters == 1, "hp"])

single_mean_cyl_3 <- mean(mtcars[single_clusters == 3, "cyl"])
single_mean_hp_3  <- mean(mtcars[single_clusters == 3, "hp"])

# Display the single linkage results
cat("Single Linkage Cluster 1: mean cyl =", round(single_mean_cyl_1, 3), ", mean hp =", round(single_mean_hp_1, 3), "\n")
cat("Single Linkage Cluster 3: mean cyl =", round(single_mean_cyl_3, 3), ", mean hp =", round(single_mean_hp_3, 3), "\n")



### Calculation for Complete Linkage ###
complete_mean_cyl_1 <- mean(mtcars[complete_clusters == 1, "cyl"])
complete_mean_hp_1  <- mean(mtcars[complete_clusters == 1, "hp"])

complete_mean_cyl_3 <- mean(mtcars[complete_clusters == 3, "cyl"])
complete_mean_hp_3  <- mean(mtcars[complete_clusters == 3, "hp"])

# Display the complete linkage results
cat("Complete Linkage Cluster 1: mean cyl =", round(complete_mean_cyl_1, 3), ", mean hp =", round(complete_mean_hp_1, 3), "\n")
cat("Complete Linkage Cluster 3: mean cyl =", round(complete_mean_cyl_3, 3), ", mean hp =", round(complete_mean_hp_3, 3), "\n")

