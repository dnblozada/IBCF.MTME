#Predictions using across multiple traits and environments using an IBCF approach

#Set working directory
setwd("C:/MARCH 2020 Analyses/IBCF/Multi trait and multiple env")

#Install package
install.packages("IBCF.MTME")

#Load library
library(IBCF.MTME)

#Load data
Traits <- read.csv("PUL_MTME.csv", header = TRUE)

#Perform CV with 75% training and 25% testing
CrossV <- CV.RandomPart(Traits, NPartitions = 10,
                        PTesting = 0.25, Set_seed = 5) #
pm1 <- IBCF(CrossV)
summary(pm1)

#Write results
write.csv(summary(pm1), "Summary_MTME_Pm1.csv")



