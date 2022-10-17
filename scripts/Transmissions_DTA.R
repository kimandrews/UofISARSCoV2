### Author: Kim Andrews
### Purpose: Parse output from TreeTime Discrete Trait Analysis to identify between-population transmission events

# ============================= Packages ==========================

require(tidyverse)
require(ape)

# ============================= Parse the TreeTime Discrete Trait Analysis ==========================

# Load the tree and node data from TreeTime Discrete Trait Analysis performed using the Nextstrain ncov pipeline
tree <- read.tree("tree.nwk")
conf <- read_tsv("NodeConfidence.tsv") 
dates <- read_tsv("TreeNodeDates.tsv")
node_data <- left_join(conf, dates)

# Tip names and numbers
tips <- tree$tip.label %>% as_tibble() %>% mutate(num = 1:nrow(.)) %>% rename(name = value) 

# Internal node names and numbers
nodes <- tree$node.label %>% as_tibble() %>% mutate(num = 1:nrow(.) + nrow(tips)) %>% rename(name = value) 

# Combine tips and internal nodes 
node_numbers <- bind_rows(tips, nodes)

# Branches & associated node data
branches <- tree$edge %>% as_tibble() %>% rename(parent_num = V1, child_num = V2)

branch_info <- branches %>% left_join(node_numbers, by= c("parent_num" = "num")) %>% rename(parent_name = name) %>% 
  left_join(node_numbers, by= c("child_num" = "num")) %>% rename(child_name = name) %>% 
  left_join(node_data, by = c("parent_name" = "node")) %>% 
  rename(parent_population = population, parent_confidence = confidence, parent_date = date) %>% 
  left_join(node_data, by = c("child_name" = "node")) %>% 
  rename(child_population = population, child_confidence = confidence, child_date = date) %>% 
  unite("pops", c("parent_population", "child_population")) 

# Identify transmissions from University to Community (UtoC) and Community to University (CtoU)
UtoC <- branch_info %>% filter(pops == "University_Community")
CtoU <- branch_info %>% filter(pops == "Community_University")

# Identify between-population transmissions that lead to phylogenetic singletons
# These are characterized by a child node that is a tip (not a node with descendants)

UtoC_singletons <- UtoC %>% filter(!grepl("NODE_", child_name))
CtoU_singletons <- CtoU %>% filter(!grepl("NODE_", child_name))

## Filter parent & child nodes with confidence values >= 0.8
branch_info_conf80 <- branch_info %>% filter(parent_confidence >= 0.8) %>% filter(child_confidence >= 0.8)

UtoC_conf80 <- branch_info_conf80 %>% filter(pops == "University_Community")
CtoU_conf80 <- branch_info_conf80 %>% filter(pops == "Community_University")

UtoC_singletons_conf80 <- UtoC_conf80 %>% filter(!grepl("NODE_", child_name))
CtoU_singletons_conf80 <- CtoU_conf80 %>% filter(!grepl("NODE_", child_name))



