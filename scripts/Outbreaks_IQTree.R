## Script name: Outbreaks_IQTree.R
## Purpose: Parse IQ-TREE phylogeny to identify outbreaks, defined as polytomies of 10 or more individuals
## Author: Kim Andrews

# ======================== Load Packages ==========================

require(tidyverse)
require(ape)
require(treeio)

# =================== Parse the IQ-TREE phylogeny =================

# Load the IQ-TREE  phylogeny
tree <- read.tree("tree_raw.nwk")

# Collapse polytomies (branches of length = 0), accounting for the fact that minimum IQ-TREE branch length = 0.000001
tree_collapsed <- di2multi(tree, tol = 0.0000011)

# Filter to retain only University and Community samples that occur in polytomies. 
# All samples except those from the University or Community contain "/" in the sample name in this dataset
Polytomies <- as_tibble(tree_collapsed) %>% filter(!grepl("/", label)) %>% drop_na() %>% filter(branch.length < 0.0000011)

# Calculate the size of each polytomy
summarizePolytomies <- Polytomies %>% group_by(parent) %>% summarize(n=n())

# View a histogram of polytomy sizes
ggplot(summarizePolytomies, aes(n)) + geom_histogram() + theme_bw() +
  xlab("Outbreak Cluster Size") + ylab("Count")

# Get samples in large polytomies (10 or more individuals), defined here as "outbreaks"
Outbreaks <- summarizePolytomies %>% filter(n>=10) %>% left_join(Polytomies) %>% select(label, parent) %>% 
  rename(strain = label, Outbreak = parent)


