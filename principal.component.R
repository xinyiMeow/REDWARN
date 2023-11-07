data <- read.csv("austin_stages.csv", sep = ",", header = TRUE)
data <- data[, -2] #exclude the "body" column which is not used
 ## PCA
data_for_pca <- data[, 8:11]
data_for_pca <- scale(data_for_pca)
pca_result <- prcomp(data_for_pca)
loadings <- pca_result$rotation
print(loadings)
summary(pca_result)

scores_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)

loadings_data <- data.frame(
  Loadings1 = pca_result$rotation[, 1],
  Loadings2 = pca_result$rotation[, 2],
  Variable = rownames(pca_result$rotation)
)
loadings <- pca_result$rotation
loadings_data <- as.data.frame(loadings)
loadings_data$Variable <- rownames(loadings)


load <- ggplot(data = loadings_data, aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_text(aes(label = Variable), vjust = 1, hjust = 1) +
  xlab("PC1 Loadings") +
  ylab("PC2 Loadings") +
  theme_minimal()
ggsave("load.pdf", plot = load)

# Create the biplot
# Assuming each variable should have a different color
loadings_data <- data.frame(
  Loadings1 = pca_result$rotation[, 1],
  Loadings2 = pca_result$rotation[, 2],
  Variable = rownames(pca_result$rotation)
)
# Assuming each variable should have a different color
loadings_data$Color <- factor(loadings_data$Variable)  # Create a factor based on the Variable column

biplot <- ggplot(data = scores_data, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_segment(data = loadings_data,
               aes(x = 0, y = 0, xend = Loadings1, yend = Loadings2, color = Color),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               lineend = "round") +
  geom_text(data = loadings_data, aes(x = Loadings1, y = Loadings2, label = Variable),
            color = "red", vjust = 1.5, hjust = 1.5) +
  xlab("PC1") +
  ylab("PC2") +
  theme_minimal()
ggsave("biplot.pdf", plot = biplot)
## regress
data <- data %>%
  mutate(stage_num = if_else(stage_num == 0, 6, stage_num))
 
