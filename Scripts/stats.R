## Project: DaMN project

## Title: Nanoplastics modulate the outcome of a zooplankton-microparasite 
## interaction.

## Script purpose: Plots the graphs of the paper.

## Date: 15-Oct-2022

## Authors: Stylianos Mavrianos, 
##          Florent Manzi, 
##          Ramsy Agha, 
##          Noemi Azoubib, 
##          Charlotte Schampera, 
##          Justyna Wolinska1

## Corresponding author: florent.vmanzi@gmail.com

#_______________________________________________________________________________

# Importing Libraries
library(car)
library(ggpubr)
library(survival)
library(survminer)

# Importing Dependencies
source("Final_Scripts/Utils.R")
source("Final_Scripts/Preprocessing.R")

# 1. Parasite Fitness -----------------------------------------------------
## a. Host Viability ----


binom_HV <- subset(Mets,  Retrieved == "0" | Retrieved == "1")

bn_HV <- glm(formula = Retrieved ~ NP_treatment,
             family = binomial(link = "logit"),
             data = binom_HV)

summary(bn_HV)
Anova(bn_HV, type = 2)

## b.Parasite reproduction ----

model_PR <- aov(Spore_yield ~ NP_treatment, data = InfH)

#summary(model_PR)
#plot(model_PR)
#hist(resid(model_PR))

Anova(model_PR,  type = 2)

TukeyHSD(model_PR)

# % Variance explained 

1840926463 + 2881274756
(1840926463 / 4722201219)*100
(2881274756 / 4722201219)*100

## c. Prevalence of Infection ----

binom_PI <- subset(Inf_after_day8, Inf_mets == "1" | Inf_mets == "0")

bn_PI <- glm(formula = Inf_mets ~ NP_treatment,
             family = binomial(link = "logit"), 
             data = binom_PI)

summary(bn_PI)

Anova(bn_PI, type = 2)

##d. Net Parasite Output ----

model_II <- aov(Spore_yield ~ NP_treatment, data = Mets)

# % Variance explained 

1365352187 + 7416390000 
(1365352187 / 8781742187)*100
(7416390000 / 8781742187)*100

anova(model_II)

TukeyHSD(model_II)

# 2. Host Fitness --------------------------------------------------
## a.Host Lifespan ----

model_HL <- aov(Age_death ~ NP_treatment * Inf_treatment, data = d)

Anova(model_HL, type = 2)


##  % Variance explained
412.2 + 3769.7 + 59.6 + 3052.2
(412.2 / 7293.7)*100 # NPs
(3769.7 / 7293.7)*100 # Inf
(59.6 / 7293.7)*100 # Interaction
(3052.2 / 7293.7)*100 # Res

TukeyHSD(model_HL)

# hist(resid(model_HL))
# plot(model_HL)

## b.Host that reproduced ----


# HRM stands for Host Reached Maturity

binom_HRM <- subset(d,  maturity == "0" | maturity == "1")


bn_HRM <- glm(formula = binom_HRM$maturity ~ Inf_treatment * NP_treatment, 
              family = binomial(link = "logit"), 
              data = binom_HRM )
summary(bn_HRM)


Anova(bn_HRM, type="III")

## c.Host Fecundity ----

# Only the ones that reproduced at least once

model_HF <- aov(Total_juv ~ NP_treatment * Inf_treatment, data = Reproduced_at_least1)

summary(model_HF)

Anova(model_HF, type = 3)

#  % Variance explained

170.32 + 191.19   + 71.03 + 683.16
(170.32 / 1115.7)*100 #NP
(191.19   / 1115.7)*100 #Inf
(71.03 / 1115.7)*100 #Interaction
(683.16 / 1115.7)*100 #Res

TukeyHSD(model_HF)

#hist(resid(model_HF))
#plot(model_HF)



## Getting Confidence interval for Total_juv

np_treatments <- c('Zero', 'Low', 'High')
inf_treatments <- c('Infected', 'Control')

for (status in inf_treatments) {
  for (np in np_treatments) {
      print(paste(np, '-', status) )
    
      print(get_CI(data = Reproduced_at_least1,
                    var = Reproduced_at_least1$Total_juv,
                    inf = status, 
                    np = np
                    )
          )

}
}

for (status in inf_treatments) {
  print(paste(status) )
  
  print(get_CI(data = Reproduced_at_least1,
               var = Reproduced_at_least1$Total_juv,
               inf = status, 
               inplace = T
  )
  )
  
}

