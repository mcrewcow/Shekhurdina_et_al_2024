library(tidyverse)
library(broom)

# Load your data - replace 'path_to_your_data' with the actual path to your dataset
data <- read.table(file = "clipboard", sep = "\t", header=TRUE)


# Reshape data to long format
data_long <- pivot_longer(data, 
                          cols = -Genus, 
                          names_to = 'Material', 
                          values_to = 'Contribution')

# Perform ANOVA for each microorganism
anova_results <- data_long %>% 
  group_by(Genus) %>% 
  do(tidy(aov(Contribution ~ Material, data = .)))

# View the results
print(anova_results)

data <- read.table(file = "clipboard", sep = "\t", header=TRUE) #reversed
cor_matrix <- cor(data[,-1], use="complete.obs")  # Exclude the first column (Genus) for correlation analysis

cor_melted <- reshape2::melt(cor_matrix)

# Plot the heatmap
ggplot(cor_melted, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  labs(x = "", y = "", title = "Correlation Matrix of Material Contributions")

library(Hmisc)

cor_results <- Hmisc::rcorr(as.matrix(data[,-1]))  # Exclude the Genus column for correlation analysis
library(reshape2)
# Melt the correlation matrix
cor_melted <- melt(cor_results$r)  # rcorr's correlation matrix
names(cor_melted) <- c("Var1", "Var2", "value")

# Melt the p-value matrix
p_melted <- melt(cor_results$P)  # rcorr's p-value matrix
names(p_melted) <- c("Var1", "Var2", "p_value")

# Combine the melted correlation and p-value data frames
combined_data <- merge(cor_melted, p_melted, by=c("Var1", "Var2"))

ggplot(combined_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "")), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title = element_blank(),
        legend.position = "right") +
  labs(title = "Correlation Matrix of Material Contributions with Significance")
