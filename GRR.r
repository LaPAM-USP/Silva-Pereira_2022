
# Load required libraries
library(ape)
library(phytools)
library(mgcv)

# Function to calculate GRR
calculate_GRR <- function {
  common_genes <- intersect(genome1, genome2)
  GRR <- length(common_genes) / min(length(genome1), length(genome2))
  return(GRR)
}

# Function to calculate patristic distance
calculate_patristic_distance <- function(tree) {
  pd <- sum(tree$edge.length[tree$mrca(node1, node2):node2])
  return(pd)
}

# Example phylogenetic tree
tree <- read.tree("MTBC.tree")
matrixOrtho <- as.matrix("Orthologous.tsv")

# Calculate GRR and patristic distance
GRR <- calculate_GRR(matrixOrtho)
pd <- calculate_patristic_distance(tree)

# Print the results
cat("GRR:", GRR, "\n")
cat("Patristic distance:", pd, "\n")

# Perform association analysis across multiple pairs of genomes
association_data <- data.frame(GRR = numeric(length(genomes)),
                               patristic_distance = numeric(length(genomes)))

for (i in 1:length(genomes)) {
  association_data$GRR[i] <- calculate_GRR(genomes[[i]], genomes[[i+1]])
  association_data$patristic_distance[i] <- distances[i]
}

# Perform correlation analysis
correlation <- cor(association_data$GRR, association_data$patristic_distance)

# Fit GAM for the whole comparison
gam_whole <- gam(GRR ~ s(patristic_distance), data = whole_comparison)

# Fit GAM for the intra comparison
gam_intra <- gam(GRR ~ s(patristic_distance), data = intra_comparison)

# Plot the GAMs
plot(whole_comparison$patristic_distance, whole_comparison$GRR, 
     xlab = "Patristic Distance", ylab = "GRR",
     main = "GAM: Whole Comparison", pch = 16)
lines(whole_comparison$patristic_distance, predict(gam_whole, newdata = whole_comparison),
      col = "black", lty = "dashed", lwd = 2)

plot(intra_st_comparison$patristic_distance, intra_comparison$GRR,
     xlab = "Patristic Distance", ylab = "GRR",
     main = "GAM: Intra Comparison", pch = 16)
lines(intra_st_comparison$patristic_distance, predict(gam_intra, newdata = intra_comparison),
      col = "blue", lty = "solid", lwd = 2)

