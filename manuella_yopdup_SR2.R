# Homework 2 (Statistical Regression & R)
#By Manuella NAKAM

#install packages
install.packages("ggplot2")
install.packages("corrplot")
install.packages("lattice")

# list all packages here
library(ggplot2) # plots
library(corrplot) # Correlation plots
library(lattice) # plots

#Load the dataset and print the first lines of the dataset
data_lending <- read.csv("~/Documents/AIMS/Stat_REg/lending_dataset1_aggr.csv")
head(data_lending)

#1) Undertake a descriptive analysis of this dataset.

## print the dimension of the dataset
size <- dim(data_lending)
cat("The number of rows is :", size[1], "\n")
cat("The number of columns is :", size[2])

## Overview of the dataset
summary(data_lending)

##The number on na
sum(is.na(data_lending))

### Comment: THe dataset content 304 observations and 12 features( 5 categorical and 7 numerical); 
#there is no missing values in the data 

## PLots the fatures by pairs
data_lending_numerics <- data_lending[sapply(data_lending, is.numeric)]
pairs(data_lending_numerics)

## Correlation matrix between numerical values
plot.new()
dev.off()
corr_matrix <- cor(data_lending_numerics, use = "complete.obs")
corrplot(corr_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         title = "Correlation Matrix", 
         addCoef.col = "black")

### We observe that, obs_rate is strong correlate with BadPastRecords and Defaults
### also, the earnings is more related to experience;  defaults is correlate with


# Categorical variables

##Gender vs Residence
plot.new()
dev.off()
mosaicplot(table(data_lending$Gender, data_lending$Residence),  
           las = 1, 
           col = c("salmon", "lightgreen"))
mtext("Residence", side = 2, line = 3)  
mtext("Gender", side = 1, line = 3)  

## Barplot Car Owner by gender
plot.new()
dev.off()
data_table <- table(data_lending$Gender, data_lending$CarOwner)
colors <- c("lightblue", "lightgreen")  
barplot(data_table, 
        beside = TRUE, 
        col = colors,  
        legend = rownames(data_table),  
        main = "Car Owner by gender",
        xlab = "CarOwner",
        ylab = "Frequency")

# Barplot House Owner by gender
plot.new()
dev.off()
data_table <- table(data_lending$Gender, data_lending$HomeOwner)
colors <- c("lightblue", "lightgreen")  
barplot(data_table, 
        beside = TRUE, 
        col = colors,  
        legend = rownames(data_table),  
        main = "Home Owner by gender",
        xlab = "HomeOwner",
        ylab = "Frequency")

# Barplot Land Owner by gender
plot.new()
dev.off()
data_table <- table(data_lending$Gender, data_lending$LandOwner)
colors <- c("lightblue", "lightgreen")  
barplot(data_table, 
        beside = TRUE, 
        col = colors,  
        legend = rownames(data_table),  
        main = "Land Owner by gender",
        xlab = "LandOwner",
        ylab = "Frequency")     


# Numerical variables

###Histogram of age
plot.new()
dev.off()
(ggplot(data_lending, aes(x = Age)) +
    geom_histogram(bins = 20, 
                   fill = "lightblue",        
                   color = "black",          
                   alpha = 0.7) +            
    labs(title = "Histogram of age",
         x = "Age",
         y = "Frequency") +
    theme_minimal())

## Histogram of Obs_rate
plot.new()
dev.off()
(ggplot(data_lending, aes(x = ObsRate)) +
    geom_histogram(bins = 20, 
                   fill = "lightblue",        
                   color = "black",          
                   alpha = 0.7) +            
    labs(title = "Histogram of ObsRate",
         x = "ObsRate",
         y = "Frequency") +
    theme_minimal())

## Histogram of Earnings
plot.new()
dev.off()
(ggplot(data_lending, aes(x = Earnings)) +
    geom_histogram(bins = 20, 
                   fill = "lightblue",        
                   color = "black",          
                   alpha = 0.7) +            
    labs(title = "Histogram of Earnings",
         x = "Earnings",
         y = "Frequency") +
    theme_minimal())

## plot experience~gender and earnings
plot.new()
dev.off()
ggplot(data_lending, aes(x = Experience, y = Earnings, color = Gender)) +
  geom_point() +
  labs(title = "Experience/Earnings",
       x = "Experience",
       y = "Earnings") 

## plot age~gender and earnings
plot.new()
dev.off()
ggplot(data_lending, aes(x = Age, y = Earnings, color = Gender)) +
  geom_point() +
  labs(title = "Age/Earnings",
       x = "Age",
       y = "Earnings") 


# The density of Earnings
plot.new()
dev.off()
plot(density(data_lending$Earnings), col= "skyblue")

# The density of ObsRate
plot.new()
dev.off()
plot(density(data_lending$ObsRate), col= "skyblue")

# dotplot residence~earnings~carowner
plot.new()
dev.off()
ggplot(data_lending) +
  aes(x = Residence, y = Earnings, fill = CarOwner) +  
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, alpha = 0.7) +  
  labs(title = "Dotplot earnings by residence",
       x = "Residence",
       y = "Earnings (CFA)") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set3")   

# dotplot residence~earnings~gender
plot.new()
dev.off()
ggplot(data_lending) +
  aes(x = Residence, y = Earnings, fill = Gender) +  
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, alpha = 0.7) +  
  labs(title = "Dotplot earnings by residence",
       x = "Residence",
       y = "Earnings (CFA)") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set3")  

# dotplot residence~ObsRate~gender
plot.new()
dev.off()
ggplot(data_lending) +
  aes(x = Residence, y = ObsRate, fill = Gender) +  
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, alpha = 0.7) +  
  labs(title = "Dotplot earnings by residence",
       x = "Residence",
       y = "Earnings (CFA)") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set3")  

### Plot the barplot of BadPastRecords
plot.new()
dev.off()
BadPastRecords <- table(data_lending$BadPastRecords)
percentages <- round(100 * City_Years / sum(City_Years), 1)
barplot(BadPastRecords,
        main = "Histogram of City years",
        xlab = "City_Years",
        ylab = "Frequence",
        col = colors,  
        border = "black")  

# plot ObsRate by all the variables
plot.new()
dev.off()
plot(data_lending$ObsRate ~ data_lending$BadPastRecords, , col = "red")
plot(data_lending$ObsRate ~ data_lending$Earnings, , col = "red")
plot(data_lending$ObsRate ~ data_lending$Experience, , col = "red")
plot(data_lending$ObsRate ~ data_lending$Defaults, , col = "red")
plot(data_lending$ObsRate ~ data_lending$Age, , col = "red")
plot(data_lending$ObsRate ~ data_lending$Accounts, , col = "red")


#2) Use this dataset to build a regression model to predict the default rate.
#Justify all the choices you make to arrive at your final model.

#Fit a model with all the variables
model1 <- glm(cbind(data_lending$Defaults ,data_lending$Accounts - data_lending$Defaults)  ~ data_lending$Earnings + data_lending$Age + 
                data_lending$Experience + data_lending$BadPastRecords + data_lending$Accounts + 
                data_lending$Gender + data_lending$Residence + data_lending$HomeOwner + 
                data_lending$LandOwner + data_lending$BadPastRecords + data_lending$CarOwner, family = binomial())
summary(model1)

#Fit a model only with  the significants variables
model2 <- glm(cbind(data_lending$Defaults ,data_lending$Accounts - data_lending$Defaults)  ~ data_lending$Earnings + data_lending$Experience  + data_lending$BadPastRecords , family = binomial())
summary(model2)

#Comparison of the models
AIC(model1,model2)


#3) Interpret your final model.(Report)

summary(model2)

#In summary, earnings and experience decrease the likelihood of default, 
#while bad credit histories increase the likelihood of default. 
#These results can help lenders better understand the risk factors associated 
#with defaults and make informed decisions regarding credit issuance.


#4) Are all the assumptions about your model met?

# Assumption 1: binary response
unique(data_lending$Defaults)

#Assumption 5: There is a Linear Relationship Between Explanatory Variables 
#and the Logit of the Response Variable
plot.new()
dev.off()
par(mar=c(3,3,4,4))
plot(data_lending$Earnings, logit, 
     xlab="Earnings", ylab="Logit")
lines(lowess(data_lending$Earnings, logit), col="red")

plot(data_lending$Experience, logit,
     xlab="Experience", ylab="Logit")
lines(lowess(data_lending$Experience, logit), col="red")


#Assumption 4 : There are No Extreme Outliers
plot.new()
dev.off()
(plot(model, which = 4))  # Cook's distance

#Assumption 2 : Independence of observations 
plot.new()
dev.off()
plot(model, which = 1)  # Residuals vs Fitted

#Assumption 6: Sufficiently large sample size
dim(data_lending)


#5) Discuss possible ways of improving your model.(Report)























