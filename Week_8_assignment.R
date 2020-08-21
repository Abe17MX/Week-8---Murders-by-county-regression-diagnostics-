library(tidyverse)
library(here)
library(haven)
library(dplyr)
library(car)
library(ggplot2)


base <- read.csv("murders.csv")

View(base)

#Q1

summary(base)
# To see the ranges of the variables

#Q2

# matriz = <- cor(x, method = c())

matriz <- cor(base)
View(matriz)

cor1 <- cor(base$arrests, base$murders)
cor1

cor2 <- cor(base$popul, base$arrests)
cor2

cor3 <- cor(base$murders, base$popul)
cor3

cor4 <- cor(base$murdrate, base$arrestrate)
cor4

cor5 <- cor(base$percblack, base$murders)
cor5

#Q3 and Q#4

Reg1 = lm(formula = murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = base)
summary(Reg1)

#Q5

# Residuals graphs to evaluate normality assumption of residuals
histreg1 <- hist(residuals(Reg1))
plot1 <- plot(lm(murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = base))

#Q6

Reg2 = lm(formula = murdrate ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = base)
summary(Reg2)

histreg2 <- hist(residuals(Reg2))
plot2 <- plot(lm(murdrate ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc, data = base))
              



