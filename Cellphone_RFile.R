# Dataset imported using Import Dataset function

# For EDA will save the file in new name
CellEDA <- Cellphone
class(CellEDA)
dim(CellEDA)
# thereare 3333 observations with 11 variables

# Check for missing value 
colSums(is.na(CellEDA))
# There is no missing value 


str(CellEDA)
# All vriables type is number which is okay for Logistic regression, however to perform EDA we need toc covert few variables to factor
summary( CellEDA)

CellEDA$Churn<-as.factor(CellEDA$Churn)
CellEDA$ContractRenewal<-as.factor(CellEDA$ContractRenewal)
CellEDA$DataPlan<-as.factor(CellEDA$DataPlan)

# Check for Outliers in the dataset
par(mfrow=c(2,4))

bp1=boxplot(CellEDA$AccountWeeks,horizontal = FALSE,main="Duration-Account Weeks", col="lightblue")
length(bp1$out)
bp1$out

bp2=boxplot(CellEDA$DataUsage,horizontal = FALSE,main="Data Usage", col="lightblue")
length(bp2$out)
bp1$out


bp3=boxplot(CellEDA$DayMins,horizontal = FALSE,main="Day Mins", col="lightblue")
length(bp3$out)
bp3$out

bp4=boxplot(CellEDA$DayCalls,horizontal = FALSE,main="Day Calls", col="lightblue")
length(bp4$out)
bp4$out

bp5=boxplot(CellEDA$MonthlyCharge,horizontal = FALSE,main="Monthly Charges", col="lightblue")
length(bp5$out)
bp5$out

bp6=boxplot(CellEDA$OverageFee,horizontal = FALSE,main="Overage Fee", col="lightblue")
length(bp6$out)
bp6$out

bp7=boxplot(CellEDA$RoamMins,horizontal = FALSE,main="Roaming Mins", col="lightblue")
length(bp7$out)
bp7$out

# There are some outliers in each variables. since we are creating logit model, hence not treating outliers  


# How Many customers churn
ggplot(CellEDA, aes(x=Churn))+ geom_histogram(stat = "count",fill = c("darkgreen","darkred"))
# In dataset there are 483 churned customers which is 14.50% of the given sample

# Plot histogram to see the distribution of the variables
# Install Package DataExplorer
plot_histogram(CellEDA)
# Histograms looks like a normal distributions
# For majority of the customers data usage is o or less than 0.50
# For few customers DayCalls & DayMins are 0. Will check via scatterplot if these are the same customer who have day calls 0 as well as day mins as 0


# Lets create scatter Plots for all possible combination to identify any pattern in churning of the customers
# Pair-wise scatter plots to visualize relationship
pairs(CellEDA[,-c(1,3,4,6)], col="darkblue", cex.labels = 1)
# same customers have day calls & day mins as 0
# Day Mins & Monthly charges have two separate clusters
# Data usage scatter with all possible variables have two cluster


# Each variable impact on churning
# Visualize impact of various variables on customer churn
# Library ( Data Explorer)
plot_boxplot(CellEDA,by="Churn",ggtheme =theme_dark())
# Obdervation 1 - Customers calling more frequently on service desk, are more likely to chrun
# Observation 2 - Data usage is less for the churned customers

# Plotting churing table according to the customer service calls
table(CellEDA$Churn,CellEDA$CustServCalls)

# Lets check the customer churning on Data Plan
variable.names(CellEDA)
variable<-list('DataPlan')
plotG<-list()
for (i in variable) { plotG<-ggplot(CellEDA,aes_string(x=i,fill=CellEDA$Churn))+geom_bar(position = "stack")+scale_fill_discrete(name="Churn")
print(plotG)}
# Customers with no data plan are most likely to chrun


# Lets check data plan impact on other variables
plot_boxplot(CellEDA[,-c(1,3,6)],by="DataPlan", ggtheme = theme_dark())
# .	Customers with no data plan having less Data usage, which is correct
# Customers with no data plan having less monthly charges comparitively

# Lets check the customer churning on Contract Renewal
variable<-list('ContractRenewal')
plotG<-list()
for (i in variable) { plotG<-ggplot(CellEDA,aes_string(x=i,fill=CellEDA$Churn))+geom_bar(position = "stack")+scale_fill_discrete(name="Churn")
print(plotG)}
# Ther are 42% churned out of the customers who not renewed the plan. 

# Lets check who all are the customers not going to renew the contract
plot_boxplot(CellEDA,by="ContractRenewal",ggtheme = theme_dark())
#There is no signifiaction observation coming out

# Identify What kind of customers calling more frquently on customer service desk
CellEDA$CustServCalls<-as.factor(CellEDA$CustServCalls)
plot_boxplot(CellEDA,by="CustServCalls",ggtheme = theme_dark())
# Customer with more day calls , day mins, monthly charges & Roaming mins are more frequently calling to cutomer service desk

# check if Data usage have impact on monthly charges
plot_boxplot(CellEDA[,c(5,9)],by="DataUsage",ggtheme = theme_dark())
qplot(CellEDA$DataUsage, CellEDA$MonthlyCharge, colour = CellEDA$Churn)

# With boxplot it is clear that if customer is using more data, then monthly charges are higher
# With Scatterplot it is showing two groups. First group is no impact on monthly charges if data usage is between 0-.05
# Second observation is there is linear relationship between data usage & monthly charges if data usage is greater than or equals to 1

plot_boxplot(CellEDA[,c(7,9)],by="DayMins",ggtheme = theme_dark())
qplot(CellEDA$DayMins,CellEDA$MonthlyCharge, colour = CellEDA$Churn)
# again there is a linear relationship between daymins and monthly charges
# Since monthly charges is impated by Day Mins & Data usage, might cause multicolinierity


# Correlation Matrix
corrplot::corrplot(cor(CellEDA[,-c(1,3,4,6)]), method = "number", type = c("lower"))
# There is a correlation between Data usage and monthly charges, however not causing multicolinierity problem

# Key takeaway from EDA...
# 1) There are 483 churned customers which is 14.50% of the given sample. Need to build a model to identify churning
# 2) Customer with less data usage are most likely to chrun. For majority of the customers data usage is 0 or less than 0.50 which can be concerning for the CellEDA company
# 3) Customers calling more frequently on service desk, are more likely to chrun
# 4) Customers with no data plan are most likely to chrun
# 5) Did not find significance impact of account weeks & Day Calls
# 5) Customer with more day calls , day mins, monthly charges & Roaming mins are more frequently calling to cutomer service desk
# 6) Customers not renewing the contract are leaning towards Chrun. 42% of the customers ( 20% of ovearall churned customers) who not renewed the contract, churned
# 7) Monthly charges is having linear relationship with Day Mins & Data usage, which might cause multicolinierity

# *********************Lets Start with Logistic Model***************************


summary(Cellphone)

# Creating full model without splitting the data between train & test
FullModelData<-Cellphone
str(FullModelData)

Logit.Full1<-glm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls
                +MonthlyCharge+OverageFee+RoamMins, data=FullModelData, family = "binomial")


vif(Logit.Full1)
summary(Logit.Full1)
# There is high VIF for Data Plan, Data Usage, DayMins, Monthly Charges & Overage Fee
# Lets decide between Data Plan & Data Usage - As per EDA, customer dont have data plan or consuming less data
# between 0 - 0.5 are most likely to churn, however there are some customers who dont have a data plan, still
# having some data usage. Hence decided to us drop Data Plan 
# We also learnt in EDA there is linear relationship of monthly charges with Day Mins & Data usage. Lets drop Monthly charges as well

# Logit modle on entire data set wihtout data plan & Monthly Charges variable
Logit.Full2<-glm(Churn~AccountWeeks+ContractRenewal+DataUsage+CustServCalls+DayMins+DayCalls
                +OverageFee+RoamMins, data=FullModelData, family = "binomial")
vif(Logit.Full2)
summary(Logit.Full2)
# VIF is good for rest of the variables
# Most of the variables are significant except AccountWeeks & Day Calls. 
# In EDA we also identified that Accounts Weeks & Day Calls are not playing major role

# Split data in Train & Test
set.seed(680)
split<-sample(2,nrow(Cellphone),replace=TRUE, prob=c(0.7,0.3))
Train<-Cellphone[split==1,]
Test<-Cellphone[split==2,]

# Check Train & Test data string
str(Train)
str(Test)

# Check how many churned customer are in Trained & Test Data
sum(Train$Churn)
sum(Test$Churn)
# Overall data had 14.50% churned customers
# Train Data has 328 churned customer out of 2335 which is 14.05%
# Test Data has 155 churned customer out of 998 which is 15%
# Sampling is balanced. 

# Model -1 with all variables 
# Like full modle created on entire dataset, will create model on Train dataset with all variable to see if we still see the same problem of multicolinierity
Logit.Eq1 <- Churn ~ AccountWeeks + ContractRenewal + DataPlan + DataUsage + CustServCalls + DayMins + DayCalls + 
  MonthlyCharge + OverageFee + RoamMins
write.csv(file = "LogitTest.csv", Test)

Logit1 <- glm(Logit.Eq1  , Train, family = binomial)
vif(Logit1)
summary(Logit1)
# Like dumb model on full dataset, there is again high VIF for Data Plan, Data Usage, DayMins, Monthly Charges & Overage Fee

# Model2 - Dropping Data Plan & Monthly Charges 
Logit.Eq2 <- Churn ~ AccountWeeks + ContractRenewal +DataUsage+CustServCalls + DayMins + DayCalls + 
  OverageFee + RoamMins
Logit2 <- glm(Logit.Eq2  , Train, family = binomial)
vif(Logit2)
summary(Logit2)
# As observed earlier, we have good VIF for all rest variables after dropping Data Plan & Monthly charges
# Again Account Weeks and Day Calls are not significant variables. We will drop them further 

# Model 3 - Final logit model after retaining significant variables
Logit.Eq.Final <- Churn ~  ContractRenewal  + DataUsage + CustServCalls + DayMins  + 
   OverageFee + RoamMins
Logit.Final <- glm(Logit.Eq.Final, Train, family = binomial)
vif(Logit.Final)
summary(Logit.Final)
# All variables are significant and there is no problem of multicolinierity in the model


varImp(Logit.Final)
# Cust Service call & contract Renewal are very important factor

# Apply final model on test data and predicting implied probability
Pred.Logit <- predict.glm(Logit.Final, newdata=Test, type="response")
Pred.Logit

Test$pred<-predict.glm(Logit.Final, newdata=Test, type="response")
str(Test)
# Type is selected as 'Response' as it is going to match up with 1 or 0

# Create logit table with cutoff .50
Logit.Table<-table(Test$Churn,Pred.Logit>.5)
Logit.Table

# check accuracy
Accuracy <- sum(diag(Logit.Table))/sum(Logit.Table)
Accuracy
# Per confusion matrix overall accuracy is 85%
# To predict accuracy, we don't have to look for the overall accuracy. Needs to see the accuracy in business context. 
# In this confusion matrix there are two type of errors.. 
# i) out of (823+127)= 950 predicted cases, 127 got wrong. Which means out of 950 predicted good customers acutally 127 (13%) customers churned
# ii) out of (20+28) = 48 predicated Churned, 20 (42%) are actually good. 
# iii) Since our business objective is to predict churned customers and only 28 (18%) customers predicted correctly out of 155 churned customers
# iv) Sensivity i.e. PREDICTION OF TRUE POSITIVES is 28/(127+28) = 18%

# conclusion is our model is not good enough to predict Churned customers

# Lets create ROC curve to decide optimal threashold value to increase the sensitivity
# Library - ROCR
Pred.ROC<-predict(Logit.Final,Test, type='response')
Pred.ROC<-prediction(Pred.ROC, Test$Churn)
ROC.Perf<-performance(Pred.ROC,"tpr", "tnr")
plot(ROC.Perf)

# If we take the lower thrashold value, it is will increase the sinsitivity.
# Lets use cutoff .20
Logit.Table<-table(Test$Churn,Pred.Logit>.2)
Logit.Table

# check accuracy
Accuracy <- sum(diag(Logit.Table))/sum(Logit.Table)
Accuracy

# Overall accuracy is reduced to 80%
# True positive i.e. sensitivity is increased to 100/(100+55) = 65% 
