library(readxl)
library(dplyr)
library(knitr)

KPT_uagyerekek22_24_comp <- read_excel("C:/Users/Haász Evelin/Desktop/Egyetem/Kognitív Képességek Kutatócsoport/Connections between sensorimotor and cognitive abilities, and school performance/KPT2/KPT_uagyerekek22_24_comp.xlsx")
View(KPT_uagyerekek22_24_comp)

KPT_uagyerekek22_24_comp$nem <- factor(KPT_uagyerekek22_24_comp$nem,
                      levels = c(1,2),
                      labels = c("Men", "Women"))

table(KPT_uagyerekek22_24_comp$nem)

KPT_uagyerekek22_24_comp <- subset(KPT_uagyerekek22_24_comp,
               !is.na(nem))
table(KPT_uagyerekek22_24_comp$nem)

colnames(KPT_uagyerekek22_24_comp)


Complete_cases_KPT2224 <- KPT_uagyerekek22_24_comp %>%
  dplyr::filter(if_all(1:28, ~ !is.na(.)))
View

colnames(Complete_cases_KPT2224)

#-----------------------
# CFA for KPT1
#-----------------------
overall.model_1 <- 'Gf =~ Raven_1 + Figurak_1; GWM =~ Corsi_1 + Szamismetles_1 + Szamsorozatvisszafele_1;
 PSMF =~ Ujjak_1 + Kezek_1; BOF =~ Egyensuly_nyitottszem_jobb_1 + Egyensuly_nyitottszem_bal_1;
 BCF =~ Egyensuly_csukottszem_jobb_1 + Egyensuly_csukottszem_bal_1'

overall.fit1<- cfa(model = overall.model_1,
                   data = Complete_cases_KPT2224,
                   meanstructure = TRUE)

summary(overall.fit1,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)

table_fit <- matrix(NA, nrow=7, ncol=9)
colnames(table_fit) = c("Model", "X2", "df", "CFI","TLI", "RMSEA", "SRMR", "AIC", "BIC")
table_fit[1, ] <- c("Overall Model1", round(fitmeasures(overall.fit1,
                                                       c("chisq", "df", "cfi", "tli",
                                                         "rmsea", "srmr", "aic", "bic")),3))

kable(table_fit)

#------------------------
#CFA for KPT2
#------------------------
overall.model_2 <- 'Gf =~ Raven_2 + Figurak_2; GWM =~ Corsi_2 + Szamismetles_2 + Szamsorozatvisszafele_2;
PSMF =~ Ujjak_2 + Kezek_2; BOF =~ Egyensuly_nyitottszem_jobb_2 + Egyensuly_nyitottszem_bal_2;
BCF =~ Egyensuly_csukottszem_jobb_2 + Egyensuly_csukottszem_bal_2'

overall.fit2<- cfa(model = overall.model_2,
                   data = Complete_cases_KPT2224,
                   meanstructure = TRUE)

summary(overall.fit2,
        standardized = TRUE,
        rsquare = TRUE,
        fit.measure = TRUE)

table_fit[2, ] <- c("Overall Model2 ", round(fitmeasures(overall.fit2,
                                                       c("chisq", "df", "cfi", "tli",
                                                         "rmsea", "srmr", "aic", "bic")),3))

kable(table_fit)

#------------------------
#longitudinal CFA
#------------------------
