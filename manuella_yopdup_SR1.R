# Homework 1 (Statistical Regression & R)
#By Manuella NAKAM

#install packages
install.packages("ggplot2")
install.packages("corrplot")

# list all packages here
library(ggplot2) # plots
library(corrplot) # Correlation plots

#Load the dataset and print the first lines of the dataset
data_behaviour <- read.csv( "/home/manuemk/Documents/AIMS/Stat_REg/data_purchase_behaviour.csv" )
head(data_behaviour)


# 1) Perform a descriptive analysis of the variables in this dataset. Feel free to
#use appropriate graphics to spicy your description.

## print the dimension of the dataset
size <- dim(data_behaviour)
cat("The number of rows is :", size[1], "\n")
cat("The number of columns is :", size[2])

## Overview of the dataset
summary(data_behaviour)

## Analysis of ages

### Histogram of the ages

ggplot(data_behaviour, aes(x = Age_num)) +  
  geom_histogram(color = "turquoise", bins = 10) +
  labs(title = "Histogram of Ages",
       x = "Age",
       y = "Frequency") +
  theme_minimal()


## Analysis of gender

### Plot the barplot of gender
gender <- table(data_behaviour$Gender)
barplot(gender,
        main = "Histogram gender",
        xlab = "Gender",
        ylab = "Frequence",
        col = gender,  
        border = "black")  



## Analysis of city category

### Plot the barplot of city category
category <- table(data_behaviour$City_Category)
percentages <- round(100 * category / sum(category), 1)
barplot(category,
        main = "Histogramme des Fréquences",
        xlab = "Category",
        ylab = "Frequence",
        col = category,  
        border = "black") 

## Analysis of Marital status

### Plot the barplot of Marital status
marital <- table(data_behaviour$Marital_Status)
percentages <- round(100 * marital / sum(marital), 1)
barplot(marital,
        main = "Histogramme des Fréquences",
        xlab = "Marital status",
        ylab = "Frequence",
        col = marital,  
        border = "black")  

## Analysis of City years

### Plot the barplot of City years
City_Years <- table(data_behaviour$Stay_In_Current_City_Years)
percentages <- round(100 * City_Years / sum(City_Years), 1)
barplot(City_Years,
        main = "Histogram of City years",
        xlab = "City_Years",
        ylab = "Frequence",
        col = marital,  
        border = "black")  


## Analysis of Purchase

### Plot the boxplot of Purchase and gender

ggplot(data_behaviour, aes(x = Gender, y = Purchase)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", alpha = 0.7) +
  labs(title = "Boxplot of purchase function of gender",
       x = "Gender",
       y = "Purchase") +
  theme_minimal()

### Plot the boxplot of Purchase and City_Category

ggplot(data_behaviour, aes(x = City_Category, y = Purchase)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", alpha = 0.7) +
  labs(title = "Boxplot of purchase function of the category city",
       x = "City_Category",
       y = "Purchase") +
  theme_minimal()

### Plot the boxplot of Purchase and City_Category

#### Convert into categorical
data_behaviour$Marital_Status <- factor(data_behaviour$Marital_Status, 
                                       levels = c(0, 1), 
                                       labels = c(" 0", " 1"))
####plot
ggplot(data_behaviour, aes(x = Marital_Status, y = Purchase)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", alpha = 0.7) +
  labs(title = "Boxplot of purchase function of marital status",
       x = "Marital_Status",
       y = "Purchase") +
  theme_minimal()

### Plot the boxplot of Purchase and Stay_In_Current_City_Years

#### Convert into categorical
data_behaviour$Stay_In_Current_City_Years <- factor(data_behaviour$Stay_In_Current_City_Years, 
                                        levels = c(0, 1, 2, 3,4), 
                                        labels = c(" 0", " 1", "2", "3", "4"))

####plot
ggplot(data_behaviour, aes(x = Stay_In_Current_City_Years, y = Purchase)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", alpha = 0.7) +
  labs(title = "Boxplot of purchase function of Stay_In_Current_City_Years",
       x = "Stay_In_Current_City_Years",
       y = "Purchase") +
  theme_minimal()

### Plot the scatterplot of Purchase and Age
ggplot(data_behaviour, aes(x = Age_num, y = Purchase)) +
  geom_point() +  
  labs(title = "Scatterplot between purchase and age",
       x = "Age",
       y = "Purchase") 

# Correlation matrix
data_behaviour_ <- data_behaviour[,-1]
df_numerics <- data_behaviour_[sapply(data_behaviour_, is.numeric)] # Choose numerical features
corr_matrix <- cor(df_numerics)

corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix", 
         addCoef.col = "black")



#2) Use a simple linear regression model to investigate the dependence of the purchase amount on Age:

(Age = data_behaviour[,6])
(Purchase = data_behaviour[,7])  


# a) Fit the model by estimating the intercept, the slope, and the underlying uncertainties.  

## Compute the correlation coefficient
my_cor <- function(x, y){
  
  n <- length(x)
  x_bar <- mean(x)
  y_bar <- mean(y)
  var_x <- sum( (x-x_bar)^2 ) / (n-1)
  var_y <- sum( (y-y_bar)^2 ) / (n-1)
  covar_x_y <- sum( (x-x_bar)*(y-y_bar) ) / (n-1)
  cor_x_y <- covar_x_y / ( var_x*var_y )^0.5
  return(cor_x_y)
}

my_cor(Age, Purchase)

# Compute the slope

slope <- function(x, y){
  x_bar <- mean(x)
  a <- sum((x - x_bar)*y)
  b <- sum((x - x_bar)^2)
  return(a/b)
}


(beta_1 <- slope(Age, Purchase))

# Compute the intercept

intercept <- function(x, y, z){
  x_bar <- mean(x)
  y_bar <- mean(y)
  return(y_bar - z*x_bar )
}

(beta_0 <- intercept(Age, Purchase, beta_1))

# Use the slope and intercept to predict

predict_y <- function(x) {
  beta_1 * x + beta_0
}

#Compute the error on  all the ages

(error = Purchase - predict_y(Age))

# Compute the mean of error

(error_bar <- mean(error))

#Compute the variance of error
n <- length(Age)
(var_error <- sum( (error- error_bar)^2 ) / (n-1))

# Compute the mean of age
(Age_bar <- mean(Age))

#Compute the sum of squares
(ssum <- (sum((Age - Age_bar)^2)))

#Compute the variance of slope
(var_beta1 <- var_error/ssum)

# Compute the variance of intercept
(var_beta0 <- (sum(Age^2) * var_error )/(n * ssum))

#Compute the covariance of slope and intercept
(cov_beta1_beta0 <- (- Age_bar *var_error )/ssum)

#Compute the covariance matrix of slope and intercept
cov_matrix_betas <- matrix(c(var_beta0, cov_beta1_beta0, cov_beta1_beta0, var_beta1), nrow = 2) 
cov_matrix_betas


# Calculate the test statistic Z
(Z <- beta_1 / sqrt(var_beta1))

# Define significance level
alpha <- 0.05

# Calculate critical value for a two-tailed test
(z_alpha <- qnorm(1 - alpha / 2))

# print results
cat("Test statistic Z:", Z, "\n")
cat("Critical value z_alpha:", z_alpha, "\n")

# Hypothesis test decision
if (abs(Z) > z_alpha) {
  cat("Reject  the null hypothesis (β1 ≠ 0).\n")
} else {
  cat("Fail to reject the null hypothesis: (β1 = 0).\n")
}


#3) How would you go about investigating the association between the purchase
# amount and gender?

# Divide the data by gender

male_data <- data_behaviour$Purchase[data_behaviour$Gender == "M"]
female_data <- data_behaviour$Purchase[data_behaviour$Gender == "F"]

#Compute the means
mean_male <- mean(male_data)
mean_female <- mean(female_data)

#Compute the variance
var_male <- var(male_data)
var_female <- var(female_data)

#Compute the length
(n_male <- length(male_data))
(n_female <- length(female_data))

# Compute the t-test
(t_stat <- (mean_male - mean_female) / sqrt((var_male / n_male) + (var_female / n_female)))

# Compute the degree of freedom
(df <- n_male + n_female - 2)

# Compute the p-value
(p_value <- 2 * pt(-abs(t_stat), df))

# print results
cat("t-statistique :", t_stat, "\n")
cat("Degrés de liberté :", df, "\n")
cat("p-value :", p_value, "\n")






