library(readxl)
library(dplyr)
library(knitr)
library(lavaan)
library(bootnet)
library(psych)
library(qgraph)
library(NetworkComparisonTest)
library(psychonetrics)

#-------------------------------------------------
# Load data
#-------------------------------------------------

KPT_uagyerekek22_24_comp <- read_excel(
  "C:/Users/Haász Evelin/Desktop/Egyetem/Kognitív Képességek Kutatócsoport/Connections between sensorimotor and cognitive abilities, and school performance/KPT2/KPT_uagyerekek22_24_comp.xlsx"
)

View(KPT_uagyerekek22_24_comp)

colnames(KPT_uagyerekek22_24_comp)
nrow(KPT_uagyerekek22_24_comp)
table(KPT_uagyerekek22_24_comp$nem)


#-------------------------------------------------
# Keep only children with complete data at both waves
#-------------------------------------------------

network_vars <- c(
  "Szamsorozatvisszafele_1",
  "Szamismetles_1",
  "Kezek_1",
  "Raven_1",
  "Egyensuly_nyitottszem_jobb_1",
  "Egyensuly_csukottszem_jobb_1",
  "Egyensuly_csukottszem_bal_1",
  "Egyensuly_nyitottszem_bal_1",
  "Corsi_1",
  "Ujjak_1",
  "Figurak_1",
  
  "Szamsorozatvisszafele_2",
  "Szamismetles_2",
  "Kezek_2",
  "Raven_2",
  "Egyensuly_nyitottszem_jobb_2",
  "Egyensuly_csukottszem_jobb_2",
  "Egyensuly_csukottszem_bal_2",
  "Egyensuly_nyitottszem_bal_2",
  "Corsi_2",
  "Ujjak_2",
  "Figurak_2"
)


KPT_complete <- KPT_uagyerekek22_24_comp %>%
  filter(if_all(all_of(network_vars), ~ !is.na(.)))


# Check sample size
nrow(KPT_complete)


#-------------------------------------------------
# Create 2022 and 2024 datasets from same children
#-------------------------------------------------

data2022 <- KPT_complete %>%
  select(
    anonim_id,
    Szamsorozatvisszafele_1,
    Szamismetles_1,
    Kezek_1,
    Raven_1,
    Egyensuly_nyitottszem_jobb_1,
    Egyensuly_csukottszem_jobb_1,
    Egyensuly_csukottszem_bal_1,
    Egyensuly_nyitottszem_bal_1,
    Corsi_1,
    Ujjak_1,
    Figurak_1
  )


data2024 <- KPT_complete %>%
  select(
    anonim_id,
    Szamsorozatvisszafele_2,
    Szamismetles_2,
    Kezek_2,
    Raven_2,
    Egyensuly_nyitottszem_jobb_2,
    Egyensuly_csukottszem_jobb_2,
    Egyensuly_csukottszem_bal_2,
    Egyensuly_nyitottszem_bal_2,
    Corsi_2,
    Ujjak_2,
    Figurak_2
  )


#-------------------------------------------------
# Rename variables
#-------------------------------------------------

data2022 <- data2022 %>%
  rename(
    id = anonim_id,
    BDS = Szamsorozatvisszafele_1,
    FDS = Szamismetles_1,
    Hands = Kezek_1,
    Raven = Raven_1,
    BOR = Egyensuly_nyitottszem_jobb_1,
    BCR = Egyensuly_csukottszem_jobb_1,
    BCL = Egyensuly_csukottszem_bal_1,
    BOL = Egyensuly_nyitottszem_bal_1,
    Corsi = Corsi_1,
    Fingers = Ujjak_1,
    Figures = Figurak_1
  )


data2024 <- data2024 %>%
  rename(
    id = anonim_id,
    BDS = Szamsorozatvisszafele_2,
    FDS = Szamismetles_2,
    Hands = Kezek_2,
    Raven = Raven_2,
    BOR = Egyensuly_nyitottszem_jobb_2,
    BCR = Egyensuly_csukottszem_jobb_2,
    BCL = Egyensuly_csukottszem_bal_2,
    BOL = Egyensuly_nyitottszem_bal_2,
    Corsi = Corsi_2,
    Fingers = Ujjak_2,
    Figures = Figurak_2
  )


#-------------------------------------------------
# Check longitudinal matching
#-------------------------------------------------

nrow(data2022)
nrow(data2024)

identical(data2022$id, data2024$id)


#-------------------------------------------------
# Remove ID from network variables
#-------------------------------------------------

data2022_r <- data2022 %>%
  select(-id)

data2024_r <- data2024 %>%
  select(-id)


#-------------------------------------------------
# Spearman correlations
#-------------------------------------------------

cor2022 <- corr.test(
  data2022_r,
  method = "spearman",
  adjust = "none"
)

cor2022


cor2024 <- corr.test(
  data2024_r,
  method = "spearman",
  adjust = "none"
)

cor2024


#-------------------------------------------------
# Ability groups from variables
#-------------------------------------------------

groups <- list(
  GWM  = match(c("BDS", "FDS", "Corsi"), colnames(data2022_r)),
  GF   = match(c("Raven", "Figures"), colnames(data2022_r)),
  PSMF = match(c("Fingers", "Hands"), colnames(data2022_r)),
  BF   = match(c("BOR", "BCR", "BCL", "BOL"), colnames(data2022_r))
)


#-------------------------------------------------
# Psychometric networks (EBICglasso)
# Using Spearman correlations
#-------------------------------------------------

Graph2022 <- EBICglasso(
  as.matrix(cor2022$r),
  n = nrow(data2022_r),
  gamma = 0.5,
  lambda.min.ratio = 0.01
)


Graph2024 <- EBICglasso(
  as.matrix(cor2024$r),
  n = nrow(data2024_r),
  gamma = 0.5,
  lambda.min.ratio = 0.01
)


#-------------------------------------------------
# Computing the networks / layouts
#-------------------------------------------------

layout22 <- qgraph(
  Graph2022,
  layout = "spring",
  groups = groups,
  legend = TRUE,
  palette = "colorblind"
)$layout


layout24 <- qgraph(
  Graph2024,
  layout = "spring",
  groups = groups,
  legend = TRUE,
  palette = "colorblind"
)$layout


#-------------------------------------------------
# Colors
#-------------------------------------------------

pretty_colors <- c(
  "#87B9E8", # GWM
  "#73D3C9", # GF
  "#E887A1", # PSMF
  "#A799B7"  # BF
)


#-------------------------------------------------
# Drawing the networks
#-------------------------------------------------

qgraph(
  Graph2022,
  layout = layout22,
  groups = groups,
  legend = TRUE,
  theme = "classic",
  color = pretty_colors,
  edge.color = "black",
  vsize = 9,
  details = TRUE
)


qgraph(
  Graph2024,
  layout = layout24,
  groups = groups,
  legend = TRUE,
  theme = "classic",
  color = pretty_colors,
  edge.color = "black",
  vsize = 9,
  details = TRUE
)


#-------------------------------------------------
# Network fit
#-------------------------------------------------

nPar2022 <- sum(
  Graph2022[upper.tri(Graph2022)] != 0
)

nPar2024 <- sum(
  Graph2024[upper.tri(Graph2024)] != 0
)


fit2022 <- ggmFit(
  Graph2022,
  cor2022$r,
  sampleSize = nrow(data2022_r),
  nPar = nPar2022,
  ebicTuning = 0.5
)


fit2024 <- ggmFit(
  Graph2024,
  cor2024$r,
  sampleSize = nrow(data2024_r),
  nPar = nPar2024,
  ebicTuning = 0.5
)


print(fit2022)
fit2022$fitMeasures

print(fit2024)
fit2024$fitMeasures


#-------------------------------------------------
# Centrality analysis
#-------------------------------------------------

centrality22 <- centrality(Graph2022)
centrality24 <- centrality(Graph2024)


fullcentr22 <- data.frame(
  Node = names(centrality22$OutDegree),
  Strength = centrality22$OutDegree,
  Closeness = centrality22$Closeness,
  Betweenness = centrality22$Betweenness
)

fullcentr22


fullcentr24 <- data.frame(
  Node = names(centrality24$OutDegree),
  Strength = centrality24$OutDegree,
  Closeness = centrality24$Closeness,
  Betweenness = centrality24$Betweenness
)

fullcentr24


#-------------------------------------------------
# Centrality change comparison
#-------------------------------------------------

centr_compare <- fullcentr22 %>%
  rename(
    Strength_2022 = Strength,
    Closeness_2022 = Closeness,
    Betweenness_2022 = Betweenness
  ) %>%
  left_join(
    fullcentr24 %>%
      rename(
        Strength_2024 = Strength,
        Closeness_2024 = Closeness,
        Betweenness_2024 = Betweenness
      ),
    by = "Node"
  )


centr_compare


#-------------------------------------------------
# Nonparametric bootstrap: edge and strength stability
# Spearman correlations
#-------------------------------------------------

boot2022 <- bootnet(
  data = data2022_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor",
  corArgs = list(method = "spearman"),
  statistics = c("edge", "strength"),
  nCores = 1
)


boot2024 <- bootnet(
  data = data2024_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor",
  corArgs = list(method = "spearman"),
  statistics = c("edge", "strength"),
  nCores = 1
)


#-------------------------------------------------
# Calculating the CIs
#-------------------------------------------------

edges22 <- subset(
  boot2022$bootTable,
  type == "edge"
)


edge_summary22 <- edges22 %>%
  group_by(node1, node2) %>%
  summarise(
    mean_edge = mean(value, na.rm = TRUE),
    ci_lower = quantile(value, 0.025, na.rm = TRUE),
    ci_upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    crosses_zero = ci_lower < 0 & ci_upper > 0
  )


top_edges22 <- edge_summary22 %>%
  arrange(desc(abs(mean_edge))) %>%
  head(20)


top_edges22


edges24 <- subset(
  boot2024$bootTable,
  type == "edge"
)


edge_summary24 <- edges24 %>%
  group_by(node1, node2) %>%
  summarise(
    mean_edge = mean(value, na.rm = TRUE),
    ci_lower = quantile(value, 0.025, na.rm = TRUE),
    ci_upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    crosses_zero = ci_lower < 0 & ci_upper > 0
  )


top_edges24 <- edge_summary24 %>%
  arrange(desc(abs(mean_edge))) %>%
  head(20)


top_edges24


#-------------------------------------------------
# Case-dropping bootstrap
# Spearman correlations
#-------------------------------------------------

bootcase22 <- bootnet(
  data = data2022_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor",
  corArgs = list(method = "spearman"),
  statistics = "strength",
  type = "case",
  nCores = 1
)


corStability(bootcase22)


bootcase24 <- bootnet(
  data = data2024_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor",
  corArgs = list(method = "spearman"),
  statistics = "strength",
  type = "case",
  nCores = 1
)


corStability(bootcase24)


#-------------------------------------------------
# Paired longitudinal network comparison
#-------------------------------------------------

set.seed(1234)

# Number of permutations
n_perm <- 5000


#-------------------------------------------------
# Function to estimate EBICglasso network
# using Spearman correlations
#-------------------------------------------------

estimate_network <- function(data) {
  
  cor_matrix <- cor(
    data,
    method = "spearman",
    use = "pairwise.complete.obs"
  )
  
  EBICglasso(
    cor_matrix,
    n = nrow(data),
    gamma = 0.5,
    lambda.min.ratio = 0.01
  )
}


#-------------------------------------------------
# Observed networks
#-------------------------------------------------

network22 <- estimate_network(data2022_r)
network24 <- estimate_network(data2024_r)


#-------------------------------------------------
# Global strength
#-------------------------------------------------

global_strength <- function(network) {
  sum(abs(network[upper.tri(network)]))
}


#-------------------------------------------------
# Network structure difference
#-------------------------------------------------

network_structure_difference <- function(network1, network2) {
  sum(abs(network1 - network2))
}


observed_global_strength_diff <-
  abs(
    global_strength(network22) -
      global_strength(network24)
  )


observed_structure_diff <-
  network_structure_difference(
    network22,
    network24
  )


observed_global_strength_diff
observed_structure_diff


#-------------------------------------------------
# Paired permutation test
#-------------------------------------------------

perm_global_strength <- numeric(n_perm)
perm_structure <- numeric(n_perm)


for (i in 1:n_perm) {
  
  # Randomly decide, separately for each child,
  # whether to swap their 2022 and 2024 measurements
  
  swap <- sample(
    c(TRUE, FALSE),
    size = nrow(data2022_r),
    replace = TRUE
  )
  
  
  perm22 <- data2022_r
  perm24 <- data2024_r
  
  
  # Swap complete rows within child
  perm22[swap, ] <- data2024_r[swap, ]
  perm24[swap, ] <- data2022_r[swap, ]
  
  
  # Estimate networks
  perm_network22 <- estimate_network(perm22)
  perm_network24 <- estimate_network(perm24)
  
  
  # Calculate global strength difference
  perm_global_strength[i] <-
    abs(
      global_strength(perm_network22) -
        global_strength(perm_network24)
    )
  
  
  # Calculate network structure difference
  perm_structure[i] <-
    network_structure_difference(
      perm_network22,
      perm_network24
    )
}


#-------------------------------------------------
# P-values
#-------------------------------------------------

p_global_strength <-
  (
    sum(
      perm_global_strength >= observed_global_strength_diff
    ) + 1
  ) /
  (n_perm + 1)


p_network_structure <-
  (
    sum(
      perm_structure >= observed_structure_diff
    ) + 1
  ) /
  (n_perm + 1)


#-------------------------------------------------
# Results
#-------------------------------------------------

cat(
  "Global strength difference:",
  observed_global_strength_diff,
  "\n"
)


cat(
  "Global strength permutation p-value:",
  p_global_strength,
  "\n\n"
)


cat(
  "Network structure difference:",
  observed_structure_diff,
  "\n"
)


cat(
  "Network structure permutation p-value:",
  p_network_structure,
  "\n"
)


#RESULTS: Global strength: 
#not statistically different between 2022 and 2024.
#Network structure: statistically different between 2022 and 2024.
#--------------------------------------

#....chekcking density....
sum(network22[upper.tri(network22)] != 0)
sum(network24[upper.tri(network24)] != 0)

round(
  sum(network22[upper.tri(network22)] != 0) / 55,
  3
)

round(
  sum(network24[upper.tri(network24)] != 0) / 55,
  3
)

#fdr
p_values <- c(
  global_strength = p_global_strength,
  network_structure = p_network_structure
)

p.adjust(p_values, method = "BH")

# ============================================
# PAIRED EDGE DIFFERENCES + FDR
# ============================================

set.seed(1234)
n_perm <- 5000

# 1. Observed edge differences
observed_edges_22 <- network22[upper.tri(network22)]
observed_edges_24 <- network24[upper.tri(network24)]

observed_edge_diff <- observed_edges_24 - observed_edges_22

# 2. Store permutation differences
perm_edge_diff <- matrix(
  NA,
  nrow = n_perm,
  ncol = length(observed_edge_diff)
)

# 3. Paired permutations
for (i in 1:n_perm) {
  
  swap <- sample(
    c(TRUE, FALSE),
    size = nrow(data2022_r),
    replace = TRUE
  )
  
  perm22 <- data2022_r
  perm24 <- data2024_r
  
  perm22[swap, ] <- data2024_r[swap, ]
  perm24[swap, ] <- data2022_r[swap, ]
  
  # Estimate networks
  perm_network22 <- estimate_network(perm22)
  perm_network24 <- estimate_network(perm24)
  
  # Signed difference: 2024 - 2022
  perm_edge_diff[i, ] <-
    perm_network24[upper.tri(perm_network24)] -
    perm_network22[upper.tri(perm_network22)]
}

# 4. Two-sided paired permutation p-value for each edge
p_edge <- numeric(length(observed_edge_diff))

for (j in 1:length(observed_edge_diff)) {
  
  p_edge[j] <- (
    sum(
      abs(perm_edge_diff[, j]) >=
        abs(observed_edge_diff[j])
    ) + 1
  ) / (n_perm + 1)
}

# 5. FDR correction across all 55 edges
p_edge_fdr <- p.adjust(
  p_edge,
  method = "BH"
)

# 6. Edge names
edge_idx <- which(
  upper.tri(network22),
  arr.ind = TRUE
)

edge_names <- paste(
  rownames(network22)[edge_idx[, 1]],
  "--",
  colnames(network22)[edge_idx[, 2]]
)

# 7. Final results table
edge_results <- data.frame(
  edge = edge_names,
  edge_2022 = observed_edges_22,
  edge_2024 = observed_edges_24,
  difference = observed_edge_diff,
  p = p_edge,
  p_fdr = p_edge_fdr
)

# Sort by FDR-adjusted p-value
edge_results <- edge_results[
  order(edge_results$p_fdr),
]

rownames(edge_results) <- NULL

edge_results

#The strength of the BCR–BCL, BOR–BOL, and Raven–Figures edges increased significantly from 2022 to 2024, whereas the FDS–BOR edge decreased significantly. 
#All four differences remained significant after FDR correction.

#-------------------------------
#longitudinal model
#-------------------------------
#A 2022-es képességek mennyire jósolják meg ugyanazon gyermek 2024-es képességeit?

# ============================================
# LONGITUDINAL DATASET
# ============================================


long_data <- cbind(
  setNames(data2022_r, paste0(names(data2022_r), "_22")),
  setNames(data2024_r, paste0(names(data2024_r), "_24"))
)

vars_long <- names(data2022_r)

vars_design <- cbind(
  paste0(vars_long, "_22"),
  paste0(vars_long, "_24")
)

rownames(vars_design) <- vars_long
colnames(vars_design) <- c("2022", "2024")


# ============================================
# BETA MATRIX
# All 2022 -> 2024 temporal paths
# ============================================

beta_matrix <- matrix(
  1,
  nrow = length(vars_long),
  ncol = length(vars_long),
  dimnames = list(vars_long, vars_long)
)


# ============================================
# LONGITUDINAL MODEL
# ============================================

model_long2 <- dlvm1(
  data = long_data,
  vars = vars_design,
  standardize = "z_per_wave",
  beta = beta_matrix,
  sigma_zeta_within = "diag",
  sigma_zeta_between = "diag"
)

model_long2 <- model_long2 %>%
  runmodel()


# ============================================
# RESULTS
# ============================================

model_long2

fit(model_long2)

parameters(model_long2)
