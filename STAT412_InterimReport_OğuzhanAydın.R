library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(ggmosaic)
library(factoextra)
library(sqldf)
library(rpart.plot)
library(InformationValue)
library(randomForest)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(forcats)
library(Boruta)
library(ggpubr)
library(naniar)
library(misty)
library(GGally)
library(e1071)
library(forecast)
library(caret)
library(pROC)
library(vcd)
library(mice)
library(VIM)
library(rcompanion)
library(visdat)
library(rstatix)
library(nortest)
library(AID)
library(usdm)
set.seed(412)
data <- read.csv("crx.csv", header = F)
head(data)
dim(data)

# Data Cleaning
colnames(data) <- c("Gender","Age","Debt","Married","BankCustomer","Industry",
                    "Ethnicity","YearsEmployed","PriorDefault","Employed","CreditScore",
                    "DriversLicense","Citizen","ZipCode","Income","Approved")
str(data)
data %>% count(Gender)
data$Gender <- str_replace(data$Gender,"a", "Female") # 0 = Female
data$Gender <- str_replace(data$Gender,"b", "Male") # 1 = Male
data$Gender <- gsub("?",NA,data$Gender, fixed = TRUE)
data$Gender <- as.factor(data$Gender)

data %>% count(Age)
data$Age <- as.numeric(data$Age)

data %>% count(Debt)
data$Debt <- as.numeric(data$Debt)

data %>% count(Married)
data$Married <- str_replace(data$Married,"u", "Married") # 1 = Married
data$Married <- str_replace(data$Married,"y", "Not Married")
data$Married <- str_replace(data$Married,"l", "Not Married") # 0 = Single/Divorced/etc.
data$Married <- gsub("?",NA,data$Married, fixed = TRUE)
data$Married <- as.factor(data$Married)

data %>% count(BankCustomer)
data$BankCustomer <- str_replace(data$BankCustomer,"p", "0") # 0 = Do not have a bank account
data$BankCustomer <- str_replace(data$BankCustomer,"g", "1")
data$BankCustomer <- str_replace(data$BankCustomer,"1g", "1") # 1 = Have a bank account
data$BankCustomer <- gsub("?",NA,data$BankCustomer, fixed = TRUE)
data$BankCustomer <- as.factor(data$BankCustomer)

data %>% count(Industry)
data <- data %>% mutate(Industry = recode(Industry, "aa"="ConsumerStaples","c"="Energy",
                                          "cc"="InformationTechnology","d"="Real Estate","e"="Education",
                                          "ff"="Healthcare","i"="ConsumerDiscretionary",
                                          "j"="Research","k"="Financials","m"="CommunicationServices",
                                          "q"="Materials","r"="Transport","w"="Industrials","x"="Utilities"))
data$Industry <- gsub("?",NA,data$Industry, fixed = TRUE)
data$Industry <- as.factor(data$Industry)

data %>% count(Ethnicity)
data <- data %>% mutate(Ethnicity = recode(Ethnicity, "bb"="Asian", "ff"="Latino","h"="Black","v"="White",
                                           "dd"="Other","j"="Other","n"="Other","o"="Other","z"="Other"))
data$Ethnicity <- gsub("?",NA,data$Ethnicity, fixed = TRUE)
data$Ethnicity <- as.factor(data$Ethnicity)

data %>% count(YearsEmployed)

data %>% count(PriorDefault)
data <- data %>% mutate(PriorDefault = recode(PriorDefault, "f"="0", "t"="1")) # 1=prior default, 0=No prior default
data$PriorDefault <- as.factor(data$PriorDefault)

data %>% count(Employed)
data <- data %>% mutate(Employed = recode(Employed, "f"="Employed", "t"="Not Employed")) # 1= employed, 0=Not employed
data$Employed <- as.factor(data$Employed)

data %>% count(CreditScore)

data %>% count(DriversLicense)
data <- data %>% mutate(DriversLicense = recode(DriversLicense, "f"="Has License", "t"="Not Has License")) # 1=Has license, 0=No license
data$DriversLicense <- as.factor(data$DriversLicense)

data %>% count(Citizen)
data <- data %>% mutate(Citizen = recode(Citizen, "g"="ByBirth", "p"="Temporary", "s"="ByOtherMeans"))
data$Citizen <- as.factor(data$Citizen)

data %>% count(Income)

data %>% count(Approved)
data <- data %>% mutate(Approved = recode(Approved, "-"="Not Approved", "+"="Approved")) # 1 = Approved, 0 = Not Approved
data$Approved <- as.factor(data$Approved)

head(data)
data <- data %>% dplyr::select(-ZipCode)

#### Exploratory Data Analysis

summary(data)
colSums(is.na(data))
prop.table(table(data$Approved)) # Balanced data set


# Research Question 2: Is there an association between gender and the likelihood of being approved for Credit
contingency_table <- table(data$Gender, data$Approved)
contingency_table
odds_ratio <- oddsratio(contingency_table)
odds_ratio
chi2_test <- chisq.test(contingency_table)
library(questionr)
oddsratio(contingency_table)
chi2_test
ggplot(na.omit(data)) +
  geom_mosaic(aes(x = product(Gender), fill = Approved)) + labs(title = "Credit Approval by Gender")


# Research Question 3: Does the citizenship status of individuals have an impact on their debt?
ggplot(data, aes(x = Citizen, y = Debt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Citizenship", y = "Debt") +
  theme_bw()
shapiro.test((data$Debt))
question3 <- data[,c(3,13)]
bestNormalize::bestNormalize(data$Debt)
question3$Debt <- bestNormalize::orderNorm(data$Debt)$x.
shapiro.test(question3$Debt)
bartlett.test(Debt ~ Citizen, question3) 
kruskal.test(Debt ~ Citizen, data = question3)
kruskal.test(Debt ~ Citizen, data)
rq3 <- aov(Debt ~ Citizen, data = question3)
summary(rq3)

#After transformation, normal. Null hypothesis for Kruskal test is rejected.  there is a statistically significant difference in the debt levels among the different citizenship groups. 
#the interpretation is that citizenship status has a significant impact on debt levels, as evidenced by the results of the Kruskal-Wallis test.



# Research Question 4: Which 5 industry has the largest number of approval and largest probability of being approved
data_approved <- subset(data, Approved=="Approved") %>% group_by(Industry) %>% mutate(count_name_occurr = n())
data_notapproved <- subset(data, Approved=="Not Approved")
df <- as.data.frame(data_approved)
df <- sqldf("SELECT Industry, COUNT(*) AS Freq FROM df GROUP BY Industry ORDER BY Freq DESC LIMIT 5")
p1 <- ggplot(df, aes(y = fct_reorder(Industry,Freq), x= Freq, fill = Industry)) + geom_bar(stat = "identity") + 
  theme(legend.position = "none") + 
  geom_text(aes(label=Freq), hjust=0.25, fontface="bold", position = position_dodge(0.9))+
  xlab("Number of approval") + 
  ylab("Industries") +
  ggtitle("Number of Approval by Industries") +
  xlim(0,70)

ggplot(na.omit(data_approved), aes(y = fct_reorder(Industry,count_name_occurr), fill = Industry)) + geom_bar(stat = "count") + 
  theme(legend.position = "none") + 
  geom_text(aes(label=..count..), stat = "count", hjust=0.25, fontface="bold", position = position_dodge(0.9))+
  xlab("Number of approval") + 
  ylab("Industries") +
  ggtitle("Number of Approval by Industries")

app_freq <- sqldf("SELECT Industry, COUNT(*) AS ApprovedFreq FROM data_approved GROUP BY Industry, Approved")
notapp_freq <- sqldf("SELECT Industry, COUNT(*) AS NotApproved FROM data_notapproved GROUP BY Industry, Approved")
freq <- merge(app_freq, notapp_freq, by="Industry")
freq <- freq %>% mutate(Prob = round(ApprovedFreq/(ApprovedFreq+NotApproved),2))
freq <- sqldf("SELECT Industry, Prob FROM freq ORDER BY Prob DESC LIMIT 5")
p2 <- ggplot(na.omit(freq), aes(x = Prob, y = fct_reorder(Industry, Prob), fill = Industry)) +
  geom_bar(stat = "identity") +
  labs(title = "Probability of Approval by Industries",
       x = "Probability of Approval",
       y = "Industries") +
  theme(legend.position = "none") +
  geom_text(aes(label=Prob),hjust=0.25, fontface="bold", position = position_dodge(0.9)) +
  xlim(0,0.9)
gridExtra::grid.arrange(p1,p2, ncol=1)


# Research Question 5: Is there a significant relationship between debt and Age
ggplot(data, aes(Age, Debt)) + geom_point(color="darkblue") + theme() +
  ggtitle("Debt vs Age")
#It seems there is an increasing trend. Also, variation is high. There might be a heteroscedasticity.
#Two Continuous Variable
cor.test(data$Age, data$Debt) #Significant.


# Research Question 7: Does gender and having drivers license have an interaction effect on years of employing?
shapiro.test((data$YearsEmployed))
ggplot(data, aes(sample = YearsEmployed))+ stat_qq() + stat_qq_line() #right skewed
question7 <- data %>% dplyr::select(YearsEmployed, DriversLicense, Gender)
question7$YearsEmployed <- bestNormalize::orderNorm(data$YearsEmployed)$x.
shapiro.test(question7$YearsEmployed)
ks.test(question7$YearsEmployed, "pnorm") #Kolmogorov-Smirnov test for normality
bartlett.test(YearsEmployed~interaction(Gender,DriversLicense), question7) #Satisfied.  the variances of the groups or samples being compared are equal. 

model7 <- lm(YearsEmployed~Gender*DriversLicense, data = question7)
summary(model7)

medians <- aggregate(YearsEmployed ~ Gender + DriversLicense, question7, median)
ggplot(medians, aes(x = Gender, y = YearsEmployed, color = DriversLicense, group = DriversLicense)) +
  geom_line(linetype = "solid", size = 2) +
  geom_point(size = 4) +
  labs(x = "Gender", y = "Years Employed", color = "Drivers License", title = "Interaction Plot")
anova(model7, test = "Chisq")
#Interaction plot shows that gender and having drivers license does not have interaction effect on years of employing.
#both gender and having a driver's license show significant effects on years of employment individually. However, there is no significant interaction effect between gender and having a driver's license on years of employment.




### Missingness and Imputation
md.pattern(data)
marginplot(data[c(1,2)])
vis_miss(data) #MAR
na.test(data) # Little's Missing Completely at Random (MCAR) Test. Reject it is MCAR
mcar_test(data)

# There are total 54 missing observations.

mice_plot <- aggr(data, col=c('darkred','orange'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_Data <- mice(data, m=5, maxit = 40, seed = 500)
complete_data<-mice::complete(imputed_Data,5)
anyNA(complete_data) 

# Puprple color indicates the missing values and red ones are non missing variables. Bar plot shows the percentage of missing values. The table shows the percentage of the NA values for the variables. Gender and variables have the highest percentage of NA value together. They are followed by Industry, Ethnicity, Married and Bank Customer. No other variable has NA value.

Age_withNA<- data$Age
Age_withoutNA<- complete_data$Age
value<-c(Age_withNA,Age_withoutNA)
label <-c(rep("WithNA",length(Age_withNA)),rep("WithoutNA",length(Age_withoutNA)))
df_for_plot<-data.frame(value,label)
ggplot(df_for_plot,aes(x=value,colour= label))+geom_density(size=1)
#As seen from the density plot, the imputation for Age does not cause a considerable change in the distribution of the variable.



### One Hot Encoding
Approved <- complete_data$Approved
complete_data_use <- complete_data
variables_to_encode <- c("Industry", "Ethnicity", "Citizen")
variables_not_encode <- complete_data_use %>% dplyr::select(-c("Industry", "Ethnicity", "Citizen"))
# Perform one-hot encoding
encoded_data <- predict(dummyVars("~.", data = complete_data_use[, variables_to_encode]), newdata = complete_data_use)
final_df <- cbind(variables_not_encode,encoded_data)
dim(final_df)

#Scaling
num_variable <- final_df %>% dplyr::select(c(Income, CreditScore, YearsEmployed, Debt, Age))
notnum_variable <- final_df %>% dplyr::select(-c(Income, CreditScore, YearsEmployed, Debt, Age))
scale_compdata_num <- scale(num_variable)
scaled_data <- cbind(notnum_variable, scale_compdata_num)


boruta_data <- Boruta(Approved~., data = scaled_data, doTrace = 2)
print(boruta_data)
#The dimension of the data is increased from 15 to 34. Now, lets use Boruta to select the important variables.
boruta.finaldata <- TentativeRoughFix(boruta_data)
print(boruta.finaldata)
SelectedCols <- getSelectedAttributes(boruta.finaldata, withTentative = F)
complete_data_use <- subset(scaled_data, select = SelectedCols)
complete_data_use$Approved <- Approved
dim(complete_data_use)
# Now we have 18 variables



### Cross Validation
training.samples <- complete_data_use$Approved %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- complete_data_use[training.samples, ]
test.data <- complete_data_use[-training.samples, ]

#Validation Set Approach
model_validationset <- glm(Approved ~., data = train.data, family = "binomial")
summary(model_validationset)
predictions_validationset <- model_validationset %>% predict(test.data, type = "response")
prediction <- predict(model_validationset, type = "response",newdata = test.data)
opt <- InformationValue::optimalCutoff(test.data$Approved, prediction)[1]
predicted_class_validationset <- ifelse(predictions_validationset >= opt, "Approved", "Not Approved")
accuracy_validationset <- mean(predicted_class_validationset == test.data$Approved)
accuracy_validationset #0.43 Accuracy 


#Leave One Out
train.control_LOOCV <- trainControl(method = "LOOCV")
modelLOO_LOOCV <- train(Approved ~., data = train.data, method = "glm", family="binomial", trControl = train.control_LOOCV)
summary(modelLOO_LOOCV)
print(modelLOO_LOOCV) #0.8535 Accuracy
predictions <- predict(modelLOO_LOOCV, newdata = train.data)
confusion_matrix <- confusionMatrix(predictions, train.data$Approved)
confusion_matrix

#10 Fold Cross Validation
train.control_10f <- trainControl(method = "cv", number = 10)
model_kfold_10f <- train(Approved ~., data = train.data, method = "glm", family = "binomial",
               trControl = train.control_10f)
summary(model_kfold_10f)
model_kfold_10f #0.8606 Accuracy 
predictions <- predict(model_kfold_10f, newdata = train.data)
confusion_matrix <- confusionMatrix(predictions, train.data$Approved)
confusion_matrix

#Repeated 10 Fold Cross Validation
train.control_rep10 <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 10)
model_repkfold_rep10 <- train(Approved ~., data = train.data, method = "glm", family="binomial",
               trControl = train.control_rep10)
summary(model_repkfold_rep10)
model_repkfold_rep10 #0.8625 Accuracy
predictions <- predict(model_repkfold_rep10, newdata = train.data)
confusion_matrix <- confusionMatrix(predictions, train.data$Approved)
confusion_matrix
# Since Repeated 10 Fold Cross Validation Approach has the highest accuracy, I prefer to use Repeated 10 Fold Cross Validation Approach. It is followed by 10-Fold CV, LOOCV and Validation Set Approach.




### Modelling

num_comp_data <- complete_data_use %>% dplyr::select(c("Age","Debt","YearsEmployed","CreditScore","Income"))
ggpairs(num_comp_data, title="correlogram with ggpairs()")
#The correlation between variables are not high. There seems no multicollinearity problem between numerical variables.

#Logistic Model
logisticmodel <- train(Approved ~ ., data = train.data, method = "glm", family = "binomial", trControl = train.control_rep10)
summary(logisticmodel)
#It is seen that the overall model is significant.

logisticmodel_pred = predict(logisticmodel)

train_tab <- table(predicted = logisticmodel_pred, actual = train.data$Approved)
train_con_mat = caret::confusionMatrix(train_tab, positive = "Approved")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"]) #0.87,0.89 0.85

#sensitivity is the proportion of true positives that are correctly identified by a diagnostic test. It shows how good the test is at detecting a disease.
#Specificity is the proportion of the true negatives correctly identified by a diagnostic test. It suggests how good the test is at identifying normal (negative) condition.
#Accuracy is the proportion of true results, either true positive or true negative, in a population. It measures the degree of veracity of a diagnostic test on a condition.
#We wish to have a model with high sensitivity, specificity and accuracy. (greater than 0.8) However, this rarely occurs. So, select a model having two high metrics.

model_glm_pred_test= predict(logisticmodel, newdata = test.data)
test_tab = table(predicted = model_glm_pred_test, actual = test.data$Approved)
test_con_mat = caret::confusionMatrix(test_tab)
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"]) #0.81 0.90 0.75

# There might be overfitting or multicollinearity. We encounter with some aliased term.
logisticmodel2 <- train(Approved ~ .-Citizen.Temporary, data = train.data, method = "glm", family = "binomial", trControl = train.control_rep10)
car::vif(logisticmodel2$finalModel)


#Stepwise Regression
stepmodel <- train(Approved~., train.data, method="glmStepAIC", family="binomial", trControl=train.control_rep10, trace=F)
stepmodel_pred = predict(stepmodel)
car::vif(stepmodel$finalModel)
train_tab <- table(predicted = stepmodel_pred, actual = train.data$Approved)
train_con_mat = caret::confusionMatrix(train_tab, positive = "Approved")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"]) #0.87 - 0.93 - 0.82

#sensitivity is the proportion of true positives that are correctly identified by a diagnostic test. It shows how good the test is at detecting a disease.
#Specificity is the proportion of the true negatives correctly identified by a diagnostic test. It suggests how good the test is at identifying normal (negative) condition.
#Accuracy is the proportion of true results, either true positive or true negative, in a population. It measures the degree of veracity of a diagnostic test on a condition.
#We wish to have a model with high sensitivity, specificity and accuracy. (greater than 0.8) However, this rarely occurs. So, select a model having two high metrics.

model_glm_pred_test= predict(stepmodel, newdata = test.data)
test_tab = table(predicted = model_glm_pred_test, actual = test.data$Approved)
test_con_mat = caret::confusionMatrix(test_tab, positive = "Approved")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"]) #0.83 - 0.93 - 0.75
summary(stepmodel$finalModel)

### ROC/AUC Curve
test_prob=predict(stepmodel, newdata = test.data)
test_prob <- ifelse(test_prob=="Approved",1,0)
aucdata <- ifelse(test.data$Approved=="Approved",1,0)
test_roc = roc(aucdata ~ test_prob, plot = TRUE, print.auc = TRUE)

#Lasso Regression
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100), alpha=1)
lasso_model <- train(Approved ~ ., data = train.data, method = "glmnet", family = "binomial",
                     preProcess = c("center", "scale"), trControl = train.control_rep10, tuneGrid = lambdaGrid)
lasso_model
lasso_model$bestTune
lassomodel_pred = predict(lasso_model)
train_tab <- table(predicted = lassomodel_pred, actual = train.data$Approved)
train_con_mat = caret::confusionMatrix(train_tab, positive = "Approved")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"]) #0.86 - 0.90 - 0.827

model_glm_pred_test= predict(lasso_model, newdata = test.data)
test_tab = table(predicted = model_glm_pred_test, actual = test.data$Approved)
test_con_mat = caret::confusionMatrix(test_tab, positive = "Approved")
c(test_con_mat$overall["Accuracy"], 
  test_con_mat$byClass["Sensitivity"], 
  test_con_mat$byClass["Specificity"]) #0.89 - 0.885 - 0.89

# Neural Network
nnetGrid <- expand.grid(size = seq(1,20,4),decay = seq(0.1,0.5,0.1))
nn_model <-train(Approved ~.,  data =train.data, method = "nnet", trControl = train.control_rep10, family="binomial", tuneGrid = nnetGrid)
nn_model
nn_model$bestTune #gives the optimal parameter values.
plot.nnet(nn_model$finalModel)
nnpred_train = predict(nn_model)
train_tab <- table(predicted = nnpred_train, actual = train.data$Approved)
train_con_mat = caret::confusionMatrix(train_tab, positive = "Approved")
nn_predict <- predict(nn_model,test.data)
test_tab = table(predicted = nn_predict, actual = test.data$Approved)
confusionMatrix(test_tab)

#SVM
svm_model <-train(Approved~.,  data =train.data, method = "svmRadial", preProc =  c("center", "scale"),trControl = train.control_rep10, family="binomial")
svm_model
param_grid <- expand.grid(
  sigma = c(0.1, 1, 10),     # Different values for the cost parameter
  C = c(0.01, 0.1, 1)   # Different values for the gamma parameter
)
tuned_model <- train(
  Approved ~ .,
  data = train.data,
  method = "svmRadial",
  preProc = c("center", "scale"),
  trControl = train.control_rep10,
  family = "binomial",
  tuneGrid = param_grid
)
svm_var_imp<-varImp(svm_model, scale = FALSE)
plot(svm_var_imp)
svm_var_imp2<-varImp(tuned_model, scale = FALSE)
plot(svm_var_imp2)
svmpred_train = predict(svm_model)
svmpred_train2 = predict(tuned_model)
train_tab <- table(predicted = svmpred_train, actual = train.data$Approved)
train_tab2 <- table(predicted = svmpred_train2, actual = train.data$Approved)
caret::confusionMatrix(train_tab, positive = "Approved", mode = "everything")
caret::confusionMatrix(train_tab2, positive = "Approved", mode = "everything")
svm_test_predict<-predict(svm_model, newdata= test.data)
svm_test_predict2<-predict(tuned_model, newdata= test.data)
head(svm_test_predict)
confusionMatrix(data = svm_test_predict,reference = test.data$Approved, mode = "everything") #0.8613 Accuracy, 0.91 Sensitivity, 0.85 F1
confusionMatrix(data = svm_test_predict2,reference = test.data$Approved, mode = "everything") #0.8613 Accuracy, 0.91 Sensitivity, 0.85 F1


# Random Forest
mtry_grid <- expand.grid(.mtry = seq(100,120,2)) 
rf_model <-train(Approved ~.,  data =train.data, method = "rf", trControl = train.control_rep10,tunegrid = mtry_grid,
                 family="binomial")
rf_model
plot(rf_model)
rf_var_imp<-varImp(rf_model, scale = FALSE)
plot(rf_var_imp)
rfpred_train = predict(rf_model)
train_tab <- table(predicted = rfpred_train, actual = train.data$Approved)
caret::confusionMatrix(train_tab, positive = "Approved", mode = "everything")
rf_test_predict<-predict(rf_model, newdata= test.data)
confusionMatrix(data = rf_test_predict,reference = test.data$Approved, mode = "everything") #0.8613 Accuracy, 0.9016 Sensitivity, 0.8527 F1

# xG Boost
tune.gridxgb <- expand.grid(eta = c(0.05,0.3, 0.075), # 3
                            nrounds = c(50, 75, 100),  # 3
                            max_depth = 4:7,  # 4
                            min_child_weight = c(2.0, 2.25), #2
                            colsample_bytree = c(0.3, 0.4, 0.5), # 3
                            gamma = 0, #1
                            subsample = 1)  # 1
train.control_xg <- trainControl(method = "repeatedcv", 
                                    number = 10, repeats = 10)
xgb_model <-train(make.names(Approved) ~.,  data =train.data, method = "xgbTree",preProc =  c("center", "scale"),trControl = train.control_xg,tunegrid = tune.gridxgb)
xgb_var_imp<-varImp(xgb_model, scale = FALSE)
plot(xgb_var_imp)
xgpred_train = predict(xgb_model)
xgpred_train <- ifelse(xgpred_train=="Approved","Approved","Not Approved")
train_tab <- table(predicted = xgpred_train, actual = train.data$Approved)
caret::confusionMatrix(train_tab, positive = "Approved", mode = "everything")
xgb_test_predict<-predict(xgb_model, newdata= test.data)
xgb_test_predict <- ifelse(xgb_test_predict=="Approved","Approved","Not Approved")
xgb_test_predict <- as.factor(xgb_test_predict)
confusionMatrix(data = xgb_test_predict,reference = test.data$Approved)
xgb_model$bestTune
