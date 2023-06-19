#Install the required packages (if not already installed):
install.packages(c("micropan", "ggplot2"))

#Load the required libraries
library(micropan)
library(ggplot2)

#pan-matrix data
pan_matrix <- read.table("panmatrix.csv", header = TRUE, row.names = 1)

# Calculate the number of new genes
heaps_data <- heaps(panmatrix(pan_matrix))

# Plot the number of new genes
ggplot(heaps_data, aes(x = N, y = n)) +
  geom_line() +
  labs(x = "Number of Genomes", y = "Number of New Genes") +
  theme_minimal()