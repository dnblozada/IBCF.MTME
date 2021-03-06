#Using SRI to predict GY in an IBCF approach 

#Load libraries
library(IBCF.MTME)

#Set working directory
setwd("C:/MARCH 2020 Analyses/IBCF/SRI")

#All SRI to predict grain yield
SRI <- read.csv("Pheno_MTME_GY_SRI_3.csv", header = TRUE)
CrossV <- CV.RandomPart(SRI, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm1 <- IBCF(CrossV)
summary(pm1)
write.csv(summary(pm1), "Summary_SRI_Predict_GY.csv")

#NDVI to predict GY
NDVI <- read.csv("NDVI_GY.csv", header = TRUE)
CrossV <- CV.RandomPart(NDVI, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm2<- IBCF(CrossV)
summary(pm2)
write.csv(summary(pm2), "Summary_NDVI_GY.csv")

#NWI to predict GY
NWI <- read.csv("NWI_GY.csv", header = TRUE)
CrossV <- CV.RandomPart(NWI, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
CrossV <- CV.RandomPart(NWI, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm3 <- IBCF(CrossV)
summary(pm3)
write.csv(summary(pm3), "Summary_NWI_GY.csv")

#SR to predict GY
SR <- read.csv("SR_GY.csv", header = TRUE)
CrossV <- CV.RandomPart(SR, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm4 <- IBCF(CrossV)
summary(pm4)
write.csv(summary(pm4), "Summary_SR_GY.csv")

#NDVI-NWI to predict GY
NDVI_NWI <- read.csv("NDVI-NWI.csv", header = TRUE)
CrossV <- CV.RandomPart(NDVI_NWI, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm5 <- IBCF(CrossV)
summary(pm5)
write.csv(summary(pm5), "Summary_NDVI-NWI.csv")

#NDVI-SR to predict GY
NDVI_SR <- read.csv("NDVI-SR.csv", header = TRUE)
CrossV <- CV.RandomPart(SR, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm6 <- IBCF(CrossV)
summary(pm6)
write.csv(summary(pm6), "Summary_NDVI-SR.csv")

#NWI-SR to predict GY
NWI-SR <- read.csv("NWI-SR.csv", header = TRUE)
CrossV <- CV.RandomPart(SR, Traits.testing = c("GY"), NPartitions = 10,
                        PTesting = 0.25, Set_seed = 123)
pm4 <- IBCF(CrossV)
summary(pm4)
write.csv(summary(pm4), "Summary_NWI_SR.csv")






