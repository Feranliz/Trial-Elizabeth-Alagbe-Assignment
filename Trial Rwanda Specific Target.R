if (!require('ade4')) install.packages("ade4")
if (!require('factoextra')) install.packages("factoextra")
if (!require('NbClust')) install.packages("NbClust")
if (!require('randomForest')) install.packages("randomForest")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")

library(ade4)
library(factoextra)
library(NbClust)
library(randomForest)
library(ggplot2)
library(ggspatial)
library(tidyterra)

# Stack terracing drivers -----------------------------------------------------
stacked = c(slope, prec, sand, pop, travel_time)
stacked_original = stacked

# Transform skewed variables --------------------------------------------------
stacked$pop = log10(stacked$pop + abs(global(stacked$pop, min, na.rm=TRUE)$min) + 1)
stacked$travel_time = log10(stacked$travel_time + 1)

# Scale variables -------------------------------------------------------------
stacked = scale(stacked)

# Sample pixels ---------------------------------------------------------------
set.seed(123)
sr = spatSample(
  stacked,
  min(10000, ncell(stacked)),
  method = "random",
  as.df = TRUE,
  xy = TRUE,
  na.rm = TRUE
)

# PCA -------------------------------------------------------------------------
pca = dudi.pca(sr[, -c(1,2)], scannf = FALSE, nf = 2)
(eig.val = data.frame(eigenvalue = c(pca$eig)))
(loading = pca$co)

fviz_pca_var(pca, col.var = "contrib") +
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 17.5) +
  theme_minimal()

fviz_pca_ind(pca)

# Outlier detection and removal -----------------------------------------------
outliers = subset(pca$li, Axis1 < mean(pca$li$Axis1) - 1.5 * IQR(pca$li$Axis1) | 
                    Axis1 > mean(pca$li$Axis1) + 1.5 * IQR(pca$li$Axis1) | 
                    Axis2 < mean(pca$li$Axis2) - 1.5 * IQR(pca$li$Axis2) | 
                    Axis2 > mean(pca$li$Axis2) + 1.5 * IQR(pca$li$Axis2))

sr$new = row.names(sr)
'%!in%' = function(x,y)!('%in%'(x,y))
sr = subset(sr, new %!in% row.names(outliers))

# Re-run PCA after outlier removal --------------------------------------------
pca = dudi.pca(sr[, -c(1:2, 8)], scannf = FALSE, nf = 2)
(eig.val = data.frame(eigenvalue = c(pca$eig)))

fviz_pca_var(pca, col.var = "contrib") +
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 17) +
  theme_minimal()

fviz_pca_ind(pca)

# Project PCA back to raster --------------------------------------------------
pc_rast = terra::predict(stacked, pca)
names(pc_rast) = c("pc1", "pc2")

plot(pc_rast)

# Clustering ------------------------------------------------------------------
pc_scores = pca$li

hclust_r = hclust(dist(pc_scores), method = "ward.D2")
plot(hclust_r, hang = -1, labels = FALSE)

k = 5
clusters = cutree(hclust_r, k = k)

train = data.frame(
  pc1 = pc_scores[,1],
  pc2 = pc_scores[,2],
  cluster = as.factor(clusters)
)

rf_model = randomForest(cluster ~ pc1 + pc2, data = train)

cluster_rast = terra::predict(pc_rast, rf_model)
names(cluster_rast) = "class"

plot(rwa0)
plot(cluster_rast, add = TRUE)
plot(rwa1, add = TRUE)

# Variable importance analysis ------------------------------------------------
all = c(cluster_rast, stacked_original)
all_df = terra::as.data.frame(all)
all_df = na.omit(all_df)
rf = randomForest(as.factor(class) ~ ., all_df)
varImpPlot(rf)

# Boxplots for each variable by domain ----------------------------------------
cluster_cols = c("1" = "#440154", "2" = "#3b528b", "3" = "#21918c", "4" = "#5ec962", "5" = "#fde725")
all_df$class = factor(all_df$class, levels = c(1, 2, 3, 4, 5))

par(mfrow = c(2,3), xaxs='i', yaxs='i')

boxplot(all_df$slope ~ all_df$class,
        ylab = 'Slope', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$sand ~ all_df$class,
        ylab = 'Sand (%)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$prec ~ all_df$class,
        ylab = 'Annual precipitation (mm)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$pop ~ all_df$class,
        ylab = 'Population (inhabitants)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$travel_time ~ all_df$class,
        ylab = 'Travel time to city (hrs)', xlab = '',
        col = cluster_cols[levels(all_df$class)])

# Enhanced map visualization --------------------------------------------------
cluster_rast_df = as.data.frame(cluster_rast, xy = TRUE)
cluster_rast_df$class = factor(cluster_rast_df$class, levels = c(1, 2, 3, 4, 5))

ggplot() + theme_bw() +
  geom_raster(data = na.omit(cluster_rast_df), aes(x = x, y = y, fill = class)) +
  geom_spatvector(data = rwa1, fill = NA, linewidth = 1, color = "black") +
  scale_fill_viridis_d(option = "inferno") +
  labs(fill = "Domains") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = ggplot2::margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), 
                   text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), 
                         width = unit(1, "cm"), pad_x = unit(0.15, "cm"), 
                         pad_y = unit(0.15, "cm"), 
                         style = north_arrow_fancy_orienteering(text_size = 10))

# Zonal statistics ------------------------------------------------------------
area_tbl = zonal(cellSize(cluster_rast, unit = "km"), cluster_rast, "sum", na.rm = TRUE)
colnames(area_tbl) = c("Domain", "Area_km2")

pop_tbl = zonal(pop, cluster_rast, fun = "sum", na.rm = TRUE)
colnames(pop_tbl) = c("Domain", "Population")

result = merge(area_tbl, pop_tbl, by = "Domain")
result
