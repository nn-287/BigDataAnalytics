#Read csv file
#_________________
data<-read.csv(file="C:/Users/Nancy/Desktop/BigData project/titanic.csv", header = TRUE, sep = ",")
head(data)

#0 not surviving, 1 surviving




#Transform it to dataframe
#__________________________

ship_df = data.frame(data$Name,data$Pclass,data$Sex,data$Age,data$SibSp,data$Parch,data$Ticket,data$Fare,data$Cabin,data$Embarked,data$Survived)
head(ship_df,10)

null_columns <- colnames(ship_df)[apply(is.na(ship_df), 2, any)]
null_sum <- colSums(is.na(ship_df))

print(null_columns)
print(null_sum)


# Plot the bar chart
barplot(null_sum, main = "Null Value Counts", xlab = "Columns", ylab = "Null Value Count", col = "blue")





#Modify <Features engineering>
#______________________________

ship_df <- subset(ship_df, select = -c(data.Name, data.SibSp, data.Parch, data.Ticket,data.Cabin,data.Embarked))
head(ship_df)
dim(ship_df)




#target_df <- subset(ship_df, select = data.Survived)
#colnames(target_df)[1] = "Class"
#head(target_df)
#ship_df<-ship_df[,-5]
#print(names(ship_df))
#ship_df <- na.omit(ship_df)
#colnames(ship_df)[5] = "Class"


ship_df$data.Sex <- ifelse(ship_df$data.Sex == "female", 1, 0)#0=>Male, 1=>Female


mean_age <- mean(ship_df$data.Age, na.rm = TRUE)


ship_df$data.Age[is.na(ship_df$data.Age)] <- mean_age

head(ship_df,10)
dim(ship_df)








#Draw insights for numerical data
#_________________________________


#Mean
#_____

mean_values <- colMeans(ship_df)
print(mean_values)
barplot(mean_values, main = "Mean Values", xlab = "Columns", ylab = "Mean", col = "blue")


#STD
#____

std_values <- apply(ship_df, 2, sd)
print(std_values)
barplot(std_values, main = "std Values", xlab = "Columns", ylab = "Mean", col = "green")


#HeatMap
#________
cor_matrix <- cor(ship_df)
heatmap(cor_matrix, col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Heatmap", xlab = "Columns", ylab = "Columns")









install.packages("caTools") 

install.packages("caret") 


library(caTools) 

library(caret)  



split <- sample.split(ship_df, SplitRatio = 0.8)

train_data <- subset(ship_df, split == "TRUE") 

test_data <- subset(ship_df, split == "FALSE") 


str(train_data)




train_scale <- scale(train_data[, 1:4]) 

test_scale <- scale(test_data[, 1:4]) 



dim(train_data)


dim(test_data)


Y<-as.factor(train_data$data.Survived)


install.packages("naivebayes")
library(naivebayes)

classifier_naive <- naive_bayes(Y~ ., data = train_data) 

classifier_naive





y_pred <- predict(classifier_naive, newdata = test_data) 



conf_mat <- table(test_data$data.Survived,y_pred) 

print(conf_mat) 


install.packages("e1071")

library(e1071)



confusionMatrix(conf_mat) 




# plot: HeatMap, Confusion matrix data
#_______________________________________

conf_mat_matrix <- as.matrix(conf_mat)


heatmap(conf_mat_matrix, col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Confusion Matrix Heatmap")





# Bar chart plot for confusion matrix:
#_______________________________________



values <- c(conf_mat[1, 1], conf_mat[1, 2], conf_mat[2, 1], conf_mat[2, 2])


bar_names <- c("True Negative", "False Positive", "False Negative", "True Positive")

barplot(values, names.arg = bar_names, xlab = "Prediction", ylab = "Count", col = "blue", main = "Confusion Matrix")


# plot :
#________




# Assuming the confusion matrix is stored in the 'conf_mat' variable

# Extract the values for the 'Survived' class from the confusion matrix
survived <- c(conf_mat[1, 1], conf_mat[2, 1])

# Create names for the bars
bar_names <- c("Did Not Survive", "Survived")

# Plot the bar chart
barplot(survived, names.arg = bar_names, xlab = "Survival", ylab = "Count", col = "lightblue", main = "Survival Bar Chart")


#plot
#_______





# Assuming the confusion matrix is stored in the 'conf_mat' variable

# Extract the values for the '0' and '1' classes from the confusion matrix
class_0 <- conf_mat[1, 1]
class_1 <- conf_mat[2, 1]

# Create x-values for the sine wave
x <- seq(0, 2 * pi, length.out = 100)

# Calculate y-values for the sine wave based on the '0' and '1' values
y <- sin(class_0 * x) + sin(class_1 * x)

# Plot the sine wave
plot(x, y, type = "l", xlab = "X", ylab = "Y", main = "Sine Wave Chart")









#logistic regression


library(stats)



train_data$data.Sex <- as.integer(train_data$data.Sex)

train_data$data.Age <- as.integer(train_data$data.Age)

train_data$data.Fare <- as.integer(train_data$data.Fare)


options(scipen=999)




model <- glm(Y~train_data$data.Pclass+train_data$data.Sex+train_data$data.Age,train_data$data.Fare, family="binomial", data=train_data)

summary(model)


test_data$data.Sex <- as.integer(test_data$data.Sex)

test_data$data.Age <- as.integer(test_data$data.Age)

test_data$data.Fare <- as.integer(test_data$data.Fare)

Y<-as.factor(test_data$data.Survived)



predictions <- predict(model, newdata = test_data, type = "response")




