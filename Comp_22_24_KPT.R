library(readxl)
library(dplyr)
library(knitr)
library(lavaan)

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
table(Complete_cases_KPT2224$nem)

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
#longitudinal MGCFA
#------------------------
# ============================================================
# LONGITUDINAL CFA + MEASUREMENT INVARIANCE
# ============================================================

library(semTools)

# ============================================================
# 1. LONGITUDINAL CONFIGURAL MODEL
# ============================================================

long_model_configural <- '

# ------------------------------------------------
# TIME 1 (2022)
# ------------------------------------------------

Gf_1 =~ Raven_1 + Figurak_1

GWM_1 =~ Corsi_1 +
         Szamismetles_1 +
         Szamsorozatvisszafele_1

PSMF_1 =~ Ujjak_1 + Kezek_1

BOF_1 =~ Egyensuly_nyitottszem_jobb_1 +
         Egyensuly_nyitottszem_bal_1

BCF_1 =~ Egyensuly_csukottszem_jobb_1 +
         Egyensuly_csukottszem_bal_1


# ------------------------------------------------
# TIME 2 (2024)
# ------------------------------------------------

Gf_2 =~ Raven_2 + Figurak_2

GWM_2 =~ Corsi_2 +
         Szamismetles_2 +
         Szamsorozatvisszafele_2

PSMF_2 =~ Ujjak_2 + Kezek_2

BOF_2 =~ Egyensuly_nyitottszem_jobb_2 +
         Egyensuly_nyitottszem_bal_2

BCF_2 =~ Egyensuly_csukottszem_jobb_2 +
         Egyensuly_csukottszem_bal_2


# ------------------------------------------------
# LATENT STABILITY
# ------------------------------------------------

Gf_1 ~~ Gf_2
GWM_1 ~~ GWM_2
PSMF_1 ~~ PSMF_2
BOF_1 ~~ BOF_2
BCF_1 ~~ BCF_2


# ------------------------------------------------
# WITHIN-TIME FACTOR CORRELATIONS
# ------------------------------------------------

Gf_1 ~~ GWM_1 + PSMF_1 + BOF_1 + BCF_1
GWM_1 ~~ PSMF_1 + BOF_1 + BCF_1
PSMF_1 ~~ BOF_1 + BCF_1
BOF_1 ~~ BCF_1

Gf_2 ~~ GWM_2 + PSMF_2 + BOF_2 + BCF_2
GWM_2 ~~ PSMF_2 + BOF_2 + BCF_2
PSMF_2 ~~ BOF_2 + BCF_2
BOF_2 ~~ BCF_2


# ------------------------------------------------
# CORRELATED UNIQUENESSES
# ------------------------------------------------

Raven_1 ~~ Raven_2
Figurak_1 ~~ Figurak_2

Corsi_1 ~~ Corsi_2
Szamismetles_1 ~~ Szamismetles_2
Szamsorozatvisszafele_1 ~~ Szamsorozatvisszafele_2

Ujjak_1 ~~ Ujjak_2
Kezek_1 ~~ Kezek_2

Egyensuly_nyitottszem_jobb_1 ~~ Egyensuly_nyitottszem_jobb_2
Egyensuly_nyitottszem_bal_1 ~~ Egyensuly_nyitottszem_bal_2

Egyensuly_csukottszem_jobb_1 ~~ Egyensuly_csukottszem_jobb_2
Egyensuly_csukottszem_bal_1 ~~ Egyensuly_csukottszem_bal_2

'

# ============================================================
# 2. CONFIGURAL INVARIANCE
# ============================================================

fit_configural <- cfa(
  model = long_model_configural,
  data = Complete_cases_KPT2224,
  std.lv = TRUE,
  meanstructure = TRUE,
  estimator = "MLR"
)

summary(
  fit_configural,
  standardized = TRUE,
  fit.measures = TRUE
)

fitMeasures(
  fit_configural,
  c("chisq","df","cfi","tli","rmsea","srmr","aic","bic")
)

# ============================================================
# 3. METRIC INVARIANCE
# ============================================================

long_model_metric <- '

# ------------------------------------------------
# TIME 1
# ------------------------------------------------

Gf_1 =~ a1*Raven_1 + a2*Figurak_1

GWM_1 =~ b1*Corsi_1 +
         b2*Szamismetles_1 +
         b3*Szamsorozatvisszafele_1

PSMF_1 =~ c1*Ujjak_1 + c2*Kezek_1

BOF_1 =~ d1*Egyensuly_nyitottszem_jobb_1 +
         d2*Egyensuly_nyitottszem_bal_1

BCF_1 =~ e1*Egyensuly_csukottszem_jobb_1 +
         e2*Egyensuly_csukottszem_bal_1


# ------------------------------------------------
# TIME 2
# ------------------------------------------------

Gf_2 =~ a1*Raven_2 + a2*Figurak_2

GWM_2 =~ b1*Corsi_2 +
         b2*Szamismetles_2 +
         b3*Szamsorozatvisszafele_2

PSMF_2 =~ c1*Ujjak_2 + c2*Kezek_2

BOF_2 =~ d1*Egyensuly_nyitottszem_jobb_2 +
         d2*Egyensuly_nyitottszem_bal_2

BCF_2 =~ e1*Egyensuly_csukottszem_jobb_2 +
         e2*Egyensuly_csukottszem_bal_2


# ------------------------------------------------
# LATENT STABILITY
# ------------------------------------------------

Gf_1 ~~ Gf_2
GWM_1 ~~ GWM_2
PSMF_1 ~~ PSMF_2
BOF_1 ~~ BOF_2
BCF_1 ~~ BCF_2


# ------------------------------------------------
# WITHIN-TIME FACTOR CORRELATIONS
# ------------------------------------------------

Gf_1 ~~ GWM_1 + PSMF_1 + BOF_1 + BCF_1
GWM_1 ~~ PSMF_1 + BOF_1 + BCF_1
PSMF_1 ~~ BOF_1 + BCF_1
BOF_1 ~~ BCF_1

Gf_2 ~~ GWM_2 + PSMF_2 + BOF_2 + BCF_2
GWM_2 ~~ PSMF_2 + BOF_2 + BCF_2
PSMF_2 ~~ BOF_2 + BCF_2
BOF_2 ~~ BCF_2


# ------------------------------------------------
# CORRELATED UNIQUENESSES
# ------------------------------------------------

Raven_1 ~~ Raven_2
Figurak_1 ~~ Figurak_2

Corsi_1 ~~ Corsi_2
Szamismetles_1 ~~ Szamismetles_2
Szamsorozatvisszafele_1 ~~ Szamsorozatvisszafele_2

Ujjak_1 ~~ Ujjak_2
Kezek_1 ~~ Kezek_2

Egyensuly_nyitottszem_jobb_1 ~~ Egyensuly_nyitottszem_jobb_2
Egyensuly_nyitottszem_bal_1 ~~ Egyensuly_nyitottszem_bal_2

Egyensuly_csukottszem_jobb_1 ~~ Egyensuly_csukottszem_jobb_2
Egyensuly_csukottszem_bal_1 ~~ Egyensuly_csukottszem_bal_2

'

fit_metric <- cfa(
  model = long_model_metric,
  data = Complete_cases_KPT2224,
  std.lv = TRUE,
  meanstructure = TRUE,
  estimator = "MLR"
)

fitMeasures(
  fit_metric,
  c("chisq","df","cfi","tli","rmsea","srmr","aic","bic")
)

# ============================================================
# 4. MODEL COMPARISON
# ============================================================

anova(fit_configural, fit_metric)

# ============================================================
# Problems :
# - first we excluded incomplete cases --> 485 cases left,w kids who took part in both
# data collecting occasions
# - it seems previously data were not adequately filtered which lead to no effect, but
# on the filtered data there was no metric invariance
# - large differences in variance between variables (eg. 0-10 vs 0-9000 )
# - 2 manifest variables on most of the factors
# my next move should be a network + invariance tests
# ============================================================



