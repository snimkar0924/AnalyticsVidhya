

#read data 
loandata<-read.csv('./data/train_u6lujuX.csv')
summary(loandata)

# there are missing values in the data
# note: missing values for:
# 1. loan amount
# 2. loan amount term
# 3. credit history check done or not
# let us first ignore these variables for our model (part A)
# and consider these later (part B)

#### A ######
# the data file has been transformed to add dummy variables for regression
# and saved as train.csv - use this for part A
loandata_A<-read.csv('./data/train.csv')
summary(loandata_A)
colnames(loandata_A)

##split the training dataset into training and validation data
## 
N<-nrow(loandata_A)
R<-round(N*0.65)
N-R
# trngData<-loandata_A[1:R,]
# valdtnData<-loandata_A[(R+1):N,]
trngData<-loandata_A[(N-R+1):N,]
valdtnData<-loandata_A[1:(N-R),]


##check for missing data
#check training set
md.pattern(trngData)
mice_plot1 <- aggr(trngData, col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(trngData), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))

#check validation set
md.pattern(valdtnData)
mice_plot2 <- aggr(valdtnData, col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(valdtnData), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))

model1<-glm(formula = trngData$d_loan_status
                     ~trngData$d_gender
                     +trngData$d_married
                     +trngData$d_dependents
                     +trngData$d_education
                     +trngData$d_self_employed
                     +trngData$ApplicantIncome
                     +trngData$CoapplicantIncome
                     +trngData$d_urban
                     +trngData$d_rural-1,
                      data=trngData, family="binomial")
summary(model1)

model2<-glm(formula = trngData$d_loan_status
                    ~trngData$d_married
                    +trngData$d_education
                    +trngData$d_urban-1,
                    data=trngData, family="binomial")
summary(model2)

model3<-glm(formula = trngData$d_loan_status
            ~trngData$d_married
            +trngData$d_education
            +trngData$d_urban
            +trngData$d_rural-1,
            data=trngData, family="binomial")
summary(model3)


###choose model3
##build logit model
cfs<-coef(model3)

bmodel<-1/(1+exp(-1*(cfs[1]*valdtnData$d_married
                     +cfs[2]*valdtnData$d_education
                     +cfs[2]*valdtnData$d_urban
                     +cfs[3]*valdtnData$d_rural)))

length(bmodel)
head(bmodel, n=20)
plot(bmodel)

actual<-valdtnData$d_loan_status
length(actual[actual==1])
length(actual[actual==0])
head(actual, n=20)
plot(actual)

threshold<-0.6
predicted<-c()
predicted[bmodel>threshold]<-1
predicted[bmodel<=threshold]<-0
length(predicted[predicted==1])
length(predicted[predicted==0])
head(predicted, n=20)
plot(predicted, col='blue')
points(actual, col='red')

df<-cbind.data.frame(actual, predicted)
colnames(df)<-c('survived','pred')
head(df)
write.csv(df,'./data/bmodel.csv')


library(pROC)
auc(actual, predicted)


roc <- calculate_roc(df, 1, 2, n = 100)
head(roc)
plot_roc(roc, 0.6, 1, 2)


# ROC & AUC for model evaluation
# http://www.r-bloggers.com/illustrated-guide-to-roc-and-auc/








#### B ######
##impute the missing data
library(mice)
library(VIM)

md.pattern(loandata)
#get rid of credit_history - causing imputation issues
loandata<-loandata[,-11]
summary(loandata)

mice_plot <- aggr(loandata, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(loandata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_loan_data <- mice(loandata, m=5, maxit = 50, 
                     method = 'pmm', seed = 500)
summary(imputed_loan_data)
str(imputed_loan_data)

#ERROR!!!!
# iter imp variable
# 1   1  LoanAmount  Loan_Amount_Term  Credit_History
# 1   2  LoanAmount  Loan_Amount_Term  Credit_History
# 1   3  LoanAmount  Loan_Amount_Term  Credit_History
# 1   4  LoanAmount  Loan_Amount_Term  Credit_History
# 1   5  LoanAmount
# Error in solve.default(xtx + diag(pen)) : 
#   system is computationally singular: 
#   reciprocal condition number = 1.36303e-16

# write.table(imputed_loan_data,"./data/train_imputed.txt")






