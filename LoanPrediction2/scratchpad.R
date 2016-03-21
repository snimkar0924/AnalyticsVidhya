
library(missForest)

#load data
data <- iris

#Get summary
summary(iris)
colnames(iris)

#Generate 10% missing values at Random 
iris.mis <- prodNA(iris, noNA = 0.1)

#Check missing values introduced in the data
summary(iris.mis)

#remove categorical variables
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

#mice package has a function known as md.pattern().  
#It returns a tabular form of missing value present in 
#each variable in a data set.
md.pattern(iris.mis)

mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_Data <- mice(iris.mis, m=5, maxit = 50, 
                       method = 'pmm', seed = 500)
summary(imputed_Data)
