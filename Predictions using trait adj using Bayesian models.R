#Using our own dataset for Example 8
#BLUP datasets for QAM panel- Grain yield

#Set working directory
setwd("C:/Users/den.lozada/Desktop/IBCF")

#Install IBCF package
install.packages("IBCF.MTME")

#Load library
library(IBCF.MTME)

#Load Data
myY  <- read.table("Grain yield.txt", head = TRUE)
myG <- read.table("Genotype data.txt", head = TRUE)

#Install BGLR package
install.packages("BGLR")

#Load library 
library(BGLR)

#Transform phenotypic dataset Y into Tidy format
myY <- data.frame(rownames(myY), myY)
colnames(myY) <- c('GID', 'T1_', 'T2_', 'T3_','T4_', 'T5_', 'T6_', 'T7_', 'T8_', 'T9_')
Data_test <- getTidyForm(myY)

#Transform phenotypic dataset Y into Tidy format
myY <- data.frame(rownames(myY), myY)
colnames(myY) <- c('GID', 'T1_', 'T2_', 'T3_','T4_', 'T5_', 'T6_', 'T7_', 'T8_', 'T9_','T10_', 'T11_', 'T12_','T13_', 'T14_', 'T15_', 'T16_', 'T17_', 'T18_', 'T19_', 'T20_')
Data_test <- getTidyForm(myY)

#Adjust phenotypic data for markers via a simple regression model; for first four traits T1-T4 
ETA <- list(myG = list(X = myG, model = 'BRR'))
FM_T1 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T1'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T2 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T2'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T3 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T3'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T4 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T4'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

#For traits T5-T9
ETA <- list(myG = list(X = myG, model = 'BRR'))
FM_T5 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T5'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T6 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T6'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T7 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T7'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T8 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T8'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

FM_T9 <- BGLR(Data_test$Response[Data_test$Trait
                                 == 'T9'], ETA = ETA, nIter = 20000, burnIn = 15000,
              verbose = FALSE)

#For traits 10-15

FM_T10 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T10'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T11 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T11'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T12 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T12'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T13 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T13'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T14 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T14'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T15 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T15'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

#For traits 16 to 20
FM_T16 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T16'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T17 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T17'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T18 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T18'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T19 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T19'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

FM_T20 <- BGLR(Data_test$Response[Data_test$Trait
                                  == 'T20'], ETA = ETA, nIter = 20000, burnIn = 15000,
               verbose = FALSE)

#Get the breeding values
BreedingValue <- data.frame(GID = rownames(myY),
                            T1_ = FM_T1$yHat, T2_ = FM_T2$yHat, T3_ = FM_T3$yHat,
                            T4_ = FM_T4$yHat)

BreedingValue <- data.frame(GID = rownames(myY),
                            T5_ = FM_T5$yHat, T6_ = FM_T6$yHat, T7_ = FM_T7$yHat,
                            T8_ = FM_T8$yHat, T9_ = FM_T9$yHat)


BreedingValue <- data.frame(GID = rownames(myY),
                            T10_ = FM_T10$yHat, T11_ = FM_T11$yHat, T12_ = FM_T12$yHat,
                            T13_ = FM_T13$yHat, T14_ = FM_T14$yHat, T15_ = FM_T15$yHat)

BreedingValue <- data.frame(GID = rownames(myY),
                            T16_ = FM_T16$yHat, T17_ = FM_T17$yHat, T18_ = FM_T18$yHat,
                            T19_ = FM_T19$yHat, T20_ = FM_T20$yHat)


#Transform breeding values into Tidy format
library(IBCF.MTME)
Breeding_DS <- getTidyForm(BreedingValue)

##Cross validation for 10 random partitions, 20% TST and 80% TRN; with both markers and phenotypic information used#
Breeding_DS_Missing <- CV.RandomPart(Breeding_DS,
                                     NPartitions = 10, PTesting = 0.20)
PM_Breeding <- IBCF(Breeding_DS_Missing)
summary(PM_Breeding)

#Compare prediction accuracies without markers (only phenotypes)
Response_DS_Missing <- CV.RandomPart(Data_Wheat, NPartitions = 10, PTesting = 0.20)
PM_Response <- IBCF(Response_DS_Missing)
summary(PM_Response)

#Compare prediction accuracies without markers (only phenotypes)
Response_DS_Missing <- CV.RandomPart(myY, NPartitions = 10, PTesting = 0.20)
PM_Response <- IBCF(Response_DS_Missing)
summary(PM_Response)