## Project: DaMN project

## Title: Nanoplastics modulate the outcome of a zooplankton-microparasite 
## interaction.

## Script purpose: Performs Survival analysis of the data.

## Date: 15-Oct-2022

## Authors: Stylianos Mavrianos, 
##          Florent Manzi, 
##          Ramsy Agha, 
##          Noemi Azoubib, 
##          Charlotte Schampera, 
##          Justyna Wolinska1

## Corresponding author: florent.vmanzi@gmail.com

#_______________________________________________________________________________
getwd()

# Set working dir
setwd('/Volumes/NO NAME/Erasmus/Erasmus_Placement/Project_DaMN/Analysis/R_analysis')

# Importing Libraries 
library(ggplot2)
library(dplyr)
library(survival)

# Importing Dependencies
source("Final_Scripts/Utils.R") # Required functions
source("Final_Scripts/Preprocessing.R") # Data



Daily_suv <-     Data_1%>%
  group_by(Treatment)%>% 
  arrange(Age_death) %>%
  mutate(cumsuv = array(length(Age_death):1)) %>%
  mutate(cumsuv = cumsuv -1 ) %>%
  mutate(cumsuv = (cumsuv / 25 )) %>%
  select(cumsuv,  NP_treatment, 
         Inf_treatment, Treatment, 
         Age_death) %>%
  ungroup()

Daily_suv$Inf_treatment <- factor(Daily_suv$Inf_treatment,
                                  levels = c("Control","Infected"),
                                  labels = c("Control", "Parasite-inoculated"))  

Daily_suv$status <- (Daily_suv$Age_death != 29) 

Daily_suv$status <- as.numeric(Daily_suv$status,
                               levels = c('FALSE', 'TRUE'),
                               labels = c(1, 0))

Daily_suv$Infection_status <- as.numeric(Daily_suv$Inf_treatment,
                                         levels = c('Control', 'Parasite-inoculated'),
                                         labels = c(0, 1))

Daily_suv$NP_status <- as.numeric(Daily_suv$NP_treatment,
                                  levels = c('Zero', 'Low', 'High'),
                                  labels = c('0', '1', '2'))

# Survival analysis ----


coxph(Surv(Age_death, status) ~ Infection_status , data = Daily_suv)
coxph(Surv(Age_death, status) ~ NP_status , data = Daily_suv)

coxph(Surv(Age_death, status) ~ NP_status+Infection_status , data = Daily_suv)


s1 <- survfit(Surv(Daily_suv$Age_death, Daily_suv$status) ~ Daily_suv$NP_status + Daily_suv$Infection_status)


# Summarize
res <- surv_summary(s1, Daily_suv)


# Daily survival ----


ggplot(Daily_suv, aes( x = Age_death, y = cumsuv , group = Treatment, color = Inf_treatment )) +
  scale_y_continuous(breaks=seq(	0, 2, 0.1)) +
  scale_x_continuous(breaks=seq(	1, 30, 2)) +
  labs( x = "\nDays",
        y = "\nCumulative Survival",
        title = "\nDaily survival") +
  facet_wrap(~ NP_treatment) +
  geom_vline(xintercept = 11, linetype="dashed", color = "red") +
  geom_vline(xintercept = 3, linetype="dashed", color = "red") +
  geom_step() +
  scale_colour_manual(values = c("blue", "red")) +
  guides(col=guide_legend("Infection treatment"),
         shape =guide_legend("Exposed")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(),
        plot.title = element_text(hjust = 0.5, size = 15))


# Survival probability ----
ggplot(res, aes( x = time, y = surv ,group =Infection_status, fill = Infection_status )) +
  facet_wrap(~NP_status ) + 
  scale_y_continuous(breaks=seq(	0, 1, 0.15)) +
  scale_x_continuous(breaks=seq(	1, 30, 5)) +
  labs( x = "\nDays",
        y = "\nsurvival probability",
        title = "\nSurvival") +
  
  geom_ribbon(aes(ymin= lower, ymax = upper), alpha= 0.3, ) + 
  geom_line() +
  
  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(),
        plot.title = element_text(hjust = 0.5, size = 15))