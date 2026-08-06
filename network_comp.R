library(readxl)
library(dplyr)
library(knitr)
library(lavaan)
library(bootnet)
library(psych)
library(qgraph)

KPT_uagyerekek22_24_comp <- read_excel("C:/Users/Haász Evelin/Desktop/Egyetem/Kognitív Képességek Kutatócsoport/Connections between sensorimotor and cognitive abilities, and school performance/KPT2/KPT_uagyerekek22_24_comp.xlsx")
View(KPT_uagyerekek22_24_comp)
colnames(KPT_uagyerekek22_24_comp)

data2022 <- KPT_uagyerekek22_24_comp %>%
                select(anonim_id,
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
                       Figurak_1)

data2024 <- KPT_uagyerekek22_24_comp %>%
  select(anonim_id,
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
         Figurak_2)

data2022 <- data2022 %>%
  rename( id = anonim_id,
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
          Figures = Figurak_1)


data2024 <- data2024 %>%
  rename( id = anonim_id,
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
          Figures = Figurak_2) 

#-----------------------
# - ID from the network
#-----------------------

data2022_r <- data2022 %>%
  select(-id)

data2024_r <- data2024 %>%
  select(-id)


#----------------------
#corr 
#----------------------

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

#----------------------
# ability groups from variables
#----------------------
groups <- list(
  GWM  = match(c("BDS", "FDS", "Corsi"), colnames(data2022_r)),
  GF   = match(c("Raven", "Figures"), colnames(data2022_r)),
  PSMF = match(c("Fingers", "Hands"), colnames(data2022_r)),
  BF   = match(c("BOR", "BCR", "BCL", "BOL"), colnames(data2022_r))
)

#-------------------------------------
# Psychometric network(EBICglasso)
#-------------------------------------

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


#------------------------------
#Computing the networks
#------------------------------

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

#-----------------------------------------------------------
# adding colors to the layout
#-----------------------------------------------------------
pretty_colors <- c(
  "#87B9E8", # GWM
  "#73D3C9", #GF
  "#E887A1", # PSMF
  "#A799B7" #BF
 )

#-------------------------------
# drawing the networks
#-------------------------------

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


#----------------
#network fit
#----------------

nPar2022 <- sum(Graph2022[upper.tri(Graph2022)] !=0)
nPar2024 <- sum(Graph2024[upper.tri(Graph2024)] !=0)

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


#-----------------------------
# centrality analysis
#-----------------------------
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

#-----------------------------
# centrality change comparison
#-----------------------------

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

#----------------------
#bootstrap stability
#----------------------

boot2022 <- bootnet(
  data = data2022_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor_auto",
  statistics = c("edge", "strength"),
  nCores = 1
)


boot2024 <- bootnet(
  data = data2024_r,
  nBoots = 500,
  default = "EBICglasso",
  corMethod = "cor_auto",
  statistics = c("edge", "strength"),
  nCores = 1
)

#-------------------------
#calculating the CIs
#-------------------------

edges22 <- subset(boot2022$bootTable, type == "edge")

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


edges24 <- subset(boot2024$bootTable, type == "edge")

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

#--------------------------------------------------------
# case-dropping bootstrap
#--------------------------------------------------------

