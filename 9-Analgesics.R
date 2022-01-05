
# Clear Everything
rm(list = ls())

# Set working directory
setwd("G:/My Drive/0) Current Courses/Data710-Everson/HW-Assignments")

# Get working directory
getwd()

# Get data
data <- read.csv("Analgesics.csv")

# Examine data
head(data)
tail(data)
str(data)
summary(data)
colnames(data)

# library
library(tidyverse)
library(hmisc)
library(Hmisc)


# Select only needed variables
data <- select(data, Pain, Drug)





## AGGREGATE FUNCTION: calculating means by group ================================================

# Calculate means by group

# The aggregate command is one way to calculate means 
# of a continuous variable (y) by categories of a grouped variable (x).

# With this we see the average number of children to each group of education

by_means <- aggregate(x = data$Pain,                      # Specify continuous variable
          by = list(data$Drug),               # Specify grouping variable, which must be a list type
          FUN = mean,                          # Specify function (i.e. mean)
          na.rm=TRUE)                          # Skips over any NA or missing values.

by_medians <- aggregate(x = data$Pain,                      # Specify continuous variable
          by = list(data$Drug),               # Specify grouping variable, which must be a list type
          FUN = median,                          # Specify function (i.e. mean)
          na.rm=TRUE)                          # Skips over any NA or missing values.

write.csv(by_means,"9-Final/analgesics_by_means.csv", row.names = FALSE)
write.csv(by_medians,"9-Final/analgesics_by_medians.csv", row.names = FALSE)



#########################################################################
# Problem: Is there a significant relationship between Pain and Drug type?


# Conduct Anova test ======================
one.way <- aov(Pain ~ Drug, 
               data = data)
summary(one.way)



######################################################################
# Problem: Which drug resulted in the lowest levels of pain?


# boxplot method--------------------------------

boxplot <- ggplot(data = data, 
                  aes(x = Drug, y = Pain)) +
          geom_jitter(aes(color=Drug),
                      size=4,
                      width=.25,
                      height=0) +
          geom_boxplot(color="black", 
                       size=1.2, 
                       alpha=0.5,
                       outlier.shape = NA) + 
  xlab("Drug Type") +
  ylab("Pain Rating") +
  ggtitle("Drug Type vs Pain Rating") +
  theme_light()
        
boxplot

# find mean, median, e.t.c. for each drug's pain---
A_drug_only <- data[data$Drug == "A",]
B_drug_only <- data[data$Drug == "B",]
C_drug_only <- data[data$Drug == "C",]

summary(A_drug_only)
summary(B_drug_only)
summary(C_drug_only)



# correlation matrix method---------------------

# Create three new dummy variables
data <- mutate(data,
               isDrug_A = ifelse(Drug == "A", 1, 0),
               isDrug_B = ifelse(Drug == "B", 1, 0),
               isDrug_C = ifelse(Drug == "C", 1, 0))
head(data)
tail(data)
str(data)
summary(data)
colnames(data)

# Select only needed variables
data <- select(data, Pain, isDrug_A, isDrug_B, isDrug_C)

# Get correlation matrix (r-statistic) with p-values===============================

# standard correlation matrix
corMatrix <- cor(data)
corMatrix

# correlation matrix with p vals
R_corr_matrix <- rcorr(as.matrix(data))
R_corr_matrix

# Write matrices into csv files
write.csv(corMatrix,"9-Final/Analgesics_correlationMatrix.csv", row.names = FALSE)
write.csv(R_corr_matrix$P,"9-Final/Analgesics_p-values.csv", row.names = FALSE)



