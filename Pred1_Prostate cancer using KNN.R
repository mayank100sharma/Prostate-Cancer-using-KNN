#Prostate_cancer

#Reading the new dataset file
PC <- read.csv("prostate_cancer_new.csv", stringsAsFactors = FALSE)
str(PC)

#Removing the ID column
PC <- PC[-1]
str(PC)

#Table function shows the no. of entries with B and M levels
table(PC$diagnosis_result)

#Changing the names of the levels and checking their percentage level through prop()
PC$diagnosis_result<- factor(PC$diagnosis_result, levels = c("B", "M"),
                             labels = c("Benign", "Malignant"))

round(prop.table(table(PC$diagnosis_result)) * 100, digits = 1)

#Checking the summary for rest of the features
summary(PC[c("radius", "smoothness", "area")])

#Normalizing the numeric data
normalize11 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize11(c(1, 2, 3, 4, 5))
normalize11(c(10, 20, 30, 40, 50))

#Apply normalize () to data set and Lappy () to apply specified function to each list element
PC_n <- as.data.frame(lapply(PC[2:9], normalize11))
summary(PC_n$radius)

#Splitting the data into train and test data
PC_train <- PC_n[1:65,]
PC_test <- PC_n[66:100,]

#Storing classes into factor vectors
PC_train_labels <- PC[1:65, 1]
PC_test_labels <- PC[66:100, 1]

#Applying K-NN to classify the data
PC_test_pred <- knn(train = PC_train, test = PC_test,cl = PC_train_labels, k=10)

#Removing unneccessary Chi square value
CrossTable(x = PC_test_labels, y = PC_test_pred,
           prop.chisq=FALSE)

#Standardizing the data through Scale()
PC_z <- as.data.frame(scale(PC[-1]))
summary(PC_z$radius)

PC_train <- PC_z[1:65, ]
PC_test <- PC_z[65:100, ]
PC_train_labels <- PC[1:65, 1]
PC_test_labels <- PC[65:100, 1]
PC_test_pred <- knn(train = PC_train, test = PC_test,
                    cl = PC_train_labels, k = 10)

#Comparing the predicted labels with the actual label
CrossTable(x = PC_test_labels, y = PC_test_pred,
           prop.chisq = FALSE)

#Taking the value of K = 4
PC_test_pred <- knn(train = PC_train, test = PC_test,
                    cl = PC_train_labels, k = 4)
CrossTable(x = PC_test_labels, y = PC_test_pred,
           prop.chisq=FALSE)


#Taking the value of K = 15
PC_test_pred <- knn(train = PC_train, test = PC_test,
                    cl = PC_train_labels, k = 15)
CrossTable(x = PC_test_labels, y = PC_test_pred,
           prop.chisq=FALSE)


#Taking the value of K = 21
PC_test_pred <- knn(train = PC_train, test = PC_test,
                    cl = PC_train_labels, k = 21)
CrossTable(x = PC_test_labels, y = PC_test_pred,
           prop.chisq=FALSE)

