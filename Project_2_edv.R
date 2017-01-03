                        # PROJECT 2 #
                        ############               
============================================================================================================
# First we read in and study the data 

bank<-read.csv("C:/Documents and Settings/faculty/Desktop/Edvancer_Certified_R_Prog/Datasets/bank-full.csv")

dim(bank)
                        
names(bank)

str(bank)
                        
contrasts(bank$y)                        
=============================================================================================================
# Shows there are no missing values in the data
  
sapply(bank,function(x) sum(is.na(x)))

=============================================================================================================
# Run the Logistic-Regression model check summary and multicollinearity
  
model1<-glm(y~.,family=binomial(logit),data=bank)

summary(model1)
 
library(car)                        
                        
vif(model1)                        

# In the summary we see that age, previous and pdays are not significant

# The coefficients indicate that 

# For every one unit change in age, the log odds of subscribed yes 

# (versus no) increases by 0.0001127. etc

# deviance statistic to assess model fit. From the summary

# The larger the devience worse the fit

# In our model the devience with only intercept term(Null devience)

# decreases as the variable gets added and the Residual devience 

# smaller than the null devience 

# Coefficients of independent variables in the linear equation are given above.

# Wald's z-test is perfromed to bring out which of the coefficients are

# significantly different from zero. 

# Those coefficients which have a dot or a star at the end are significantly different,

# others are not. Thus 'pdays' and 'age' can be safely removed

# from the model. Further note that among three poutcome(s),  

# two of them, poutcomeunknown and poutcomeother, are not significant, 

# only poutcomesuccess is. Overall, is poutcome significant? 

# This can be gauged using Wald's test available in 'aod' package.

# Conduct wald's test on poutcome at serial number 41 to 43. Coeff (Terms),

# 41 to 43 in the coeff table are jointly tested

library(aod)

wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 41:43)

# With a p value of 0.0, poutcome is significant.
==========================================================================================
# Lets compute the confidence interval and exponentiated C.I
                          
# confidence interval                          

confint(model1)

exp(confint(model1))

===========================================================================================

# predict the probablity values for the dependant variable y

prob<-predict(model1,type="response")
                        
# Testing predictions for few rows
                        
prob[1:10]
============================================================================================
# Predict the errors
  
res<-residuals(model1,type="deviance")

# Residual deviance is the di???erence in G^2 = ???2 logL 
# between a maximal model that has a separate
# parameter for each cell in the model and the built model. 
# Changes in the deviance (the di???erence in the quqntity -2logL)
# for two models which can be nested in a reduction will be approximately 
# chi square -distributed with dof equal to the change in the number 
# of estimated parameters. Thus the di???erence in deviances can be
# tested against the chi^2 distribution for signi???cance.                                            

==============================================================================================
# The ANOVA test tries adding the factors only in 
                          
# the order given in the model formula (left to right)  
                        
# it sequentially compares the smaller model with the 
                        
# next more complex model by adding one variable in each step. 
                        
# Each of those comparisons is done via a likelihood ratio test        
           
anova(model1,test="Chisq")                    
                        
===============================================================================================
# Plotting the fitted value for the model
  
plot(model1$fitted)
===================================================================================
#load Few important packages
 
library(MASS)

library(plyr)
==========================================================================================                        
#After dropping those two variables which are not significant we get
                          
# model 2 
                          
model2<-glm(y~poutcome+campaign+duration+month+day+contact+loan+housing+balance+default+education+marital+job,data=bank,family=binomial(logit))
           
summary(model2)

vif(model2)
==========================================================================================================================================================                        
# Divide the dataser in to train set and test set.
                          
# There are several ways to divide data set into train and test set
                          
# We use the following.
                          
# For reproducibility                          
                          
set.seed(420) 
                        
#80-20 training/test 
                        
train<-sample(dim(bank)[1],floor(.8*dim(bank)[1])) 
                        
traindata<-bank[train,]
                        
testdata <- bank[-train,]     
                            

============================================================================================================================================================                                           
# We run the model2 on train set
                          
model3<-glm(y~poutcome+campaign+duration+month
            
            +day+contact+loan+housing+balance+default
            
                +education+marital+job,
            
                   data=traindata,family=binomial(logit))
                    
                         
summary(model3)
                        
vif(model3)
                        
anova(model3,test="Chisq")
                        
prob.train<-predict(model3,type="response")
                        
prob.train[1:10]
                        
res<-residuals(model1,type="deviance")
                        
plot(model3$fitted)   
                        
confint(model1)
                        
exp(confint(model1))                        
============================================================================================================================================================                          
# predicting model2 on test set                          
                        
prob.test<-predict(model3,testdata,type="response")
                        
prob.test[1:10]                        
============================================================================================================================================================
# Check the performance of model2  on train set

# Confusion Matrix, ROC curve, Area under ROC curve , crossvalidation
                          
for (T in seq(0.1,0.9,0.1)){
                            
class1<-ifelse(prob.train>T,1,0)
                        
# Confusion Matrix
                        
cat("Threshhold = ", T,"\n")
                        
confusion = table(class1,traindata$y)
                        
cat("Confusion Matrix :" , "\n" )
                        
print(confusion)                      
                      
pctcorrect<-100*(confusion[1,1]+confusion[2,2])/sum(confusion)
                                                
False.Positive.Rate<-confusion[[3]]/(confusion[[1]]+confusion[[3]])
                        
False.Negative.Rate<-confusion[[2]]/(confusion[[2]]+confusion[[4]])

# accuracy(confusion)

cat("% correct =",round(pctcorrect,1),"\n")

print("------------------------")

cat("False Positive Rate =", round(False.Positive.Rate,2),"\n")

print("------------------------")

cat("False Negative Rate =", round(False.Positive.Rate,2),"\n")

print("------------------------")

}
                        
library(pROC)
                        
g<-roc(y~prob.train,data=traindata)
                        
plot(g)
                       

============================================================================================================================================================                          
# Check the performance of model 2 on test set
                          
for (T in seq(0.1,0.9,0.1)){
                            
class1<-ifelse(prob.test>T,1,0)
                            
# Confusion Matrix
                            
cat("Threshhold = ", T,"\n")
                            
confusion = table(class1,testdata$y)
                            
cat("Confusion Matrix :" , "\n" )
                            
print(confusion)                      
                            
pctcorrect<-100*(confusion[1,1]+confusion[2,2])/sum(confusion)
                            
False.Positive.Rate<-confusion[[3]]/(confusion[[1]]+confusion[[3]])
                            
False.Negative.Rate<-confusion[[2]]/(confusion[[2]]+confusion[[4]])
                            
cat("% correct =",round(pctcorrect,1),"\n")
                            
print("------------------------")
                            
cat("False Positive Rate =", round(False.Positive.Rate,2),"\n")
                            
print("------------------------")
                            
cat("False Negative Rate =", round(False.Positive.Rate,2),"\n")
                            
print("------------------------")
                            
}          
                

library(pROC)
                        
g<-roc(y~prob.test,data=testdata)
                        
plot(g) 

============================================================================================================================================================
# Performing cross-validation
                          
library(DAAG)
                        
CVbinary(model2)
                        
#Cross-validation estimate of accuracy = 0.903                       
                        
=====================================================================================================================================================                          





                

