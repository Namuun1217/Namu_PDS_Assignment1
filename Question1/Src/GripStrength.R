library(datasets)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,arules, kernlab, readr, ggpubr, psych, tidyverse)


#Stage 1 -Data collection
## prepared data,saved as CVS file, and imported
raw_data<-import("C:/Users/namuu/OneDrive/Documents/Namuun/UMKC/5530/CS5530_Assignment1_Q2/Question 1/data_raw/raw_data.csv")


#Stage 2 -Data processing
## ensuring that the data is in the correct format before performing any analysis 
raw_data <- as.data.frame(raw_data)
raw_data_summary <- summary(raw_data)
head(raw_data_summary)

colnames(raw_data) <- c("height","weight","age","grip_strength","frailty")
head(raw_data)

##converting character to numeric values 0, 1 since only frailty has non-numeric values
raw_data$frailty <-ifelse(raw_data$frailty=="N", 0, 1)

#creating another data set to use further analysis
clean_data <- raw_data
head(clean_data)
write.csv(clean_data, "clean_data")

sink(file = "test_result.txt")

#Stage 3 -Data analysis
#Making analysis by using data visualization

#Analysis 1 - to check if there is any correlation between weight and height
cor(clean_data$height, clean_data$weight)

###checking correlation between height & weight by using visualization
plot(clean_data$height, clean_data$weight, main="Correlation between heigh & weight",
     xlab = "height", ylab="weight", pch=19)
abline(lm(clean_data$height~clean_data$weight),col="red")
lines(lowess(clean_data$height, clean_data$weight), col="blue")

###checking correlation between height & grip strength by using visualization
plot(clean_data$height, clean_data$grip_strength, main="Correlation between height & grip strength",
     xlab = "height", ylab="grip strength", pch=19)
abline(lm(clean_data$height~clean_data$grip_strength),col="red")
lines(lowess(clean_data$height, clean_data$grip_strength), col="blue")

###checking correlation between weight & grip strength by using visualization
plot(clean_data$weight, clean_data$grip_strength, main="Correlation between weight & grip strength",
     xlab = "weight", ylab="grip strength", pch=19)
abline(lm(clean_data$weight~clean_data$grip_strength),col="red")
lines(lowess(clean_data$weight, clean_data$grip_strength), col="blue")

##Analysis 2 - creating correlation matrix between all given variables
cor(clean_data)

summary(clean_data)

###outputing result to txt file
sink(file = NULL)