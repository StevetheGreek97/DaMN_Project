## Project: DaMN project

## Title: Nanoplastics modulate the outcome of a zooplankton-microparasite 
## interaction.

## Script purpose: Cleaning, reformatting and creating subset of the data.

## Date: 15-Oct-2022

## Authors: Stylianos Mavrianos, 
##          Florent Manzi, 
##          Ramsy Agha, 
##          Noemi Azoubib, 
##          Charlotte Schampera, 
##          Justyna Wolinska1

## Corresponding author: florent.vmanzi@gmail.com

#_______________________________________________________________________________


# Importing Data

Raw_Data <- read.table("../Data/DaMN_Datafile.csv", header = T, sep = ',')

# Factorize data----

# Change the structure of the data from "chr" to factors.
# Change the names too.
Raw_Data$NP_treatment <- factor(Raw_Data$NP_treatment, 
                                levels = c("ZERO", "LOW", "HIGH"),
                                labels = c("Zero", "Low", "High"))

# Changes the labels to numeric values

#Data$NP_treatment <- factor(Data$NP_treatment, 
#                            levels = c("ZERO", "LOW", "HIGH"),
#                            labels = c("0", "5", "20"))

Raw_Data$Treatment <- factor(Raw_Data$Treatment,
                             levels = c("A_ZERO_METS" , "A_ZERO____" , "A_LOW_METS" ,
                                        "A_LOW____" , "A_HIGH_METS" , "A_HIGH____" ),
                             labels = c("Zero_Mets", "Zero", "Low_Mets", 
                                        "Low", "High_Mets", "High"))

# Infection treatment = Inf_treatment. 
#Individuals are divided according to their infection status: Control vs Infected
Raw_Data$Inf_treatment <- factor(Raw_Data$Inf_treatment,
                                 levels = c("NO_METS","METS"),
                                 labels = c("Control", "Infected"))



Raw_Data$ID.No. <- as.factor(Raw_Data$ID.No.)
Raw_Data$Rep. <- as.factor(Raw_Data$Rep.)
Raw_Data$Inf_treatment <- as.factor(Raw_Data$Inf_treatment)
Raw_Data$H_errors <- as.factor(Raw_Data$H_errors)

# Data Manipulation ----

#Remove Handling errors
Data_2 <- subset(Raw_Data, H_errors == 0 )

#Remove Background mortality. Main data!!!
Data_1 <- subset(Data_2, Age_death > 3) 

# Subsets of the main Data

# Only the exposed to the parasite treatment.
Mets <- subset(Data_1, Inf_treatment == "Infected")

# Only the parasite-free treatments.
Nomets <- subset(Data_1, Inf_treatment == "Control") 

#The ones that died before day 8 post exposure
Died_bf_day8 <- subset(Mets, Retrieved == 0) 

# Only the exposed and infected from the parasite individuals.
InfH <- subset(Mets, Spore_yield > 0)

# Combine the the three datasets.

#Individuals that died after day-8 and not infected were excluded + Control. 
d <- rbind(Died_bf_day8, InfH, Nomets)

#Only the ones that survived day-8 and were successfully infected + Control
Successfully_inf <- rbind(InfH, Nomets)

#From the infection treatment only the ones that survived after day 8.
Inf_after_day8 <- subset(Mets, Retrieved == 1) 

# Create new column; 0 | 1 did not reached maturity | reached maturity.
# maturity = The individuals that reached maturity

d$maturity <-  (d$Age_at_Mat > 0)
d$maturity <- as.numeric(d$maturity,
                         levels = c("TRUE","FALSE"),
                         labels = c("1", "0"))

# Only the ones that reproduced at least once
Reproduced_at_least1 <- subset(d, Total_juv > 0) 

# Create new column; 0 | 1 did not reached maturity | reached maturity.
# maturity = The individuals that reached maturity
Successfully_inf$maturity <-  (Successfully_inf$Age_at_Mat > 0)
Successfully_inf$maturity <- as.numeric(Successfully_inf$maturity,
                                        levels = c("TRUE","FALSE"),
                                        labels = c("1", "0"))
#Only the ones that reproduced at least once
Sup_Reproduced_at_least1 <- subset(Successfully_inf, Total_juv > 0) 
Sup_binom_HRM <- subset(Successfully_inf,  maturity == "0" | maturity == "1")

binom_PI <- subset(Inf_after_day8, Inf_mets == "1" | Inf_mets == "0")
binom_HV <- subset(Mets,  Retrieved == "0" | Retrieved == "1")
binom_HRM <- subset(d,  maturity == "0" | maturity == "1")





