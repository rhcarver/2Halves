# Game of Halves.R  -- code to produce output from 
# Game of Two Halves case
# Demonstrates multinomial logit and Decision Tree

setwd("~/Dropbox/MyRWorkDSC201/Game2Halves")


#Read file: Betting.csv contains case data
mydata<-read.csv("Betting.csv",header=T)

# Summarize the outcomes of all observed matches
table(mydata$Match_O)

# First the multinomial Logit model
# package nnet contains relevant command 'multinom'

install.packages("nnet")
library(nnet)

# First, create target variable 'result' based on factor 
#     Match Outcome where "Loss" is the reference level
#     of the factor

mydata$result<-relevel(mydata$Match_O, ref="Loss")
# show that "result" just re-orders "Match_O"
table(mydata$Match_O, mydata$result)
# Synatx of multinomial command includes target ~ x1 + x2 +
#     etc., data = dataframe
test<-multinom(result~HTGD+RED.H+RED.A+POINTS_H+POINTS_A+
               TOTAL_H_P+TOTAL_A_P+FGS.0+FGS.1, 
               data=mydata)
summary(test)  # display estimation results

#2-tailed z test
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Exponentiate
exp(coef(test))

#Misclassification error -- Confustion matrix and misclassification rate
tab1<-table(predict(test), mydata$result)
print(tab1)
1-sum(diag(tab1))/sum(tab1)

###########################################
# Decision tree with party
# First install and invoke package party

# Party performs recursive partitioning, an alternative to 
#    regression models
install.packages("party")
library(party)


# Create a decision tree using same variables as in Exhibit 9 of case
# create an object containing the target and 4 explanatory variables

mymodel <-result ~ HTGD + TOTAL_H_P + TOTAL_A_P + FGS.0 + FGS.1 

partytree <- ctree(mymodel, data=mydata, controls=ctree_control(mincriterion=0.90,
      minsplit=250, maxdepth=3)) # specify some criteria for splitting
# Note that this tree does not perfectly match the one in the case

print(partytree)  #  print the decision rules

plot(partytree)  # display graphically


#Misclassification error -- Confustion matrix and misclassification rate

tab<-table(predict(partytree), mydata$result) 
print(tab)
1-sum(diag(tab))/sum(tab)

