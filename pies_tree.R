library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(ape)

my_data1 <- read_excel("C://Users/rodri/Downloads/Network 3rd stage (2).xlsx")

# Convert taxonomic classifications to factors
my_data1$Genus <- factor(my_data1$Genus)
my_data1$Family <- factor(my_data1$Family)
my_data1$Class <- factor(my_data1$Class)
my_data1$Order <- factor(my_data1$Order)
my_data1$Phylum <- factor(my_data1$Phylum)
my_data1$Domain <- factor(my_data1$Domain)

# Create the phylogenetic tree
tree <- as.phylo(~Domain/Phylum/Class/Order/Family/Genus, data = my_data1)

# Use ggtree for plotting
plot(tree)

data <- read_excel("C://Users/rodri/Downloads/Network 3rd stage (2).xlsx")

# Pivot the data
data_long <- data %>%
  pivot_longer(cols = starts_with("C-liq"):starts_with("FM-liq"), names_to = "Component", values_to = "Value")

# Filter data for a specific Genus (you can change this to any genus you want)
genus_data <- filter(data_long, Domain == "Bacteria")

p <- ggplot(genus_data, aes(x = "", y = Value, fill = Component)) +
geom_bar(width = 1, stat = "identity") +
coord_polar("y") +
theme_void() +
labs(fill = NULL, title = paste("Pie Chart for", unique(genus_data$Genus))) +
theme(legend.position = "bottom")  # Adjust legend position as needed
print(p)
legend <- cowplot::get_legend(ggplotGrob(p))
grid.draw(legend)
