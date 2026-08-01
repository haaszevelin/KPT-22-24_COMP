library(readxl)
library(dplyr)
library(knitr)
library(lavaan)

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
    
