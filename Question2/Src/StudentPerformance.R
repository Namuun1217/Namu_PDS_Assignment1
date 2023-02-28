library(datasets)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr,arules, kernlab, readr, ggpubr, psych, tidyverse)

#Stage 1 -Data collection
##importing data from local directory
StudentsPerformance <- import("C:/Users/namuu/OneDrive/Documents/Namuun/UMKC/5530/CS5530_Assignment1_Q2/Question 2/data_raw/StudentsPerformance.csv")


#Stage 2 -Data processing
## ensuring that the data is in the correct format before performing any analysis 
StudentsPerformance <- as.data.frame(StudentsPerformance)
StudentsPerformance_summary <- summary(StudentsPerformance)
head(StudentsPerformance_summary)
colnames(StudentsPerformance) <- c("gender","race_ethnicity_parental","level_of_education","lunch","test_preparation_course","math_score","reading_score","writing_score")
head(StudentsPerformance)

##checking if there is any missing data in raw data
sum(is.na(StudentsPerformance))


## since there is no any missing value, i changed available non-numeric value testprep to numeric value
clean_data<-StudentsPerformance
clean_data$test_preparation_course <- ifelse(clean_data$test_preparation_course=="completed",1,0)
write.csv(clean_data, "clean_data.csv")


plot(StudentsPerformance)
par(mfrow = c(2,2))


## creating pdf file to output the result of analysis
pdf(file="C:/Users/namuu/OneDrive/Documents/Namuun/UMKC/5530/CS5530_Assignment1_Q2/Question 2/src/result.pdf",
    width =4,
    height = 4)


#Stage 3 -Data analysis
#Data visualization 1 -Histogram
### to determing if there is different score tendency for each gender

###Math score by Gender plot
x1 <- ggplot(StudentsPerformance, aes(math_score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
x1 <- x1 + xlab("Math Scores") + ylab("Gender") + ggtitle("Math Scores by Gender")
x1

###Reading score by Gender plot
x2 <- ggplot(StudentsPerformance, aes(reading_score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
x2 <- x2 + xlab("Reading Scores") + ylab("Gender") + ggtitle("Reading Scores by Gender")
x2

###Writing score by Gender plot
x3 <- ggplot(StudentsPerformance, aes(writing_score)) + geom_histogram(binwidth=5, color="white", aes(fill=gender))
x3 <- x3 + xlab("Writing Scores") + ylab("Gender") + ggtitle("Writing Scores by Gender")
x3


#Data visualization 2 Scatter Plot
## to determine if there is correlation between those three scores including math, reading and writing

girl_data <-StudentsPerformance%>%filter(gender=='female')
boy_data <-StudentsPerformance%>%filter(gender=='male')
ggplot()+
  geom_point(girl_data, mapping=aes(`math_score`,`reading_score`,color='female'))+
  geom_point(boy_data, mapping=aes(`math_score`,`reading_score`,color='male'))+labs(title='Math and Reading score')+
  geom_smooth(data = girl_data, aes(x = math_score, y = reading_score), method = 'lm', se = FALSE, color = 'indianred') +
  geom_smooth(data = boy_data, aes(x = math_score, y = reading_score), method = 'lm', se = FALSE, color = 'steelblue')
ggplot()+
  geom_point(girl_data, mapping=aes(`math_score`,`writing_score`,color='female'))+
  geom_point(boy_data, mapping = aes(`math_score`,`writing_score`,color='male'))+labs(title='Math and Writing score')+
  geom_smooth(data = girl_data, aes(x = math_score, y = reading_score), method = 'lm', se = FALSE, color = 'indianred') +
  geom_smooth(data = boy_data, aes(x = math_score, y = reading_score), method = 'lm', se = FALSE, color = 'steelblue')
ggplot()+
  geom_point(girl_data, mapping=aes(`reading_score`,`writing_score`,color='female'))+
  geom_point(boy_data, mapping = aes(reading_score,writing_score, color='male'))+labs(title='Reading and Writing score')


#Data visualization 3 -Box plot
## to determine if students who completed the test preparation had better score in tests by distinguishing gender

### Math score with test prep class by Gender
b1 <- ggplot(StudentsPerformance, aes(gender, math_score, color = test_preparation_course))
b1 <- b1 + geom_boxplot()
b1 <- b1 + ggtitle("Math scores by Gender Boxplot")
b1 <- b1 + xlab("Gender") + ylab("Math Scores")
b1

### Reading score with test prep class by Gender
b2 <- ggplot(StudentsPerformance, aes(gender, reading_score, color = test_preparation_course))
b2 <- b2 + geom_boxplot()
b2 <- b2 + ggtitle("Reading scores by Gender Boxplot")
b2 <- b2 + xlab("Gender") + ylab("Reading Scores")
b2

### Writing score with test prep class by Gender
b3 <- ggplot(StudentsPerformance, aes(gender, writing_score, color = test_preparation_course))
b3 <- b3 + geom_boxplot()
b3 <- b3 + ggtitle("Writing scores by Gender Boxplot")
b3 <- b3 + xlab("Gender") + ylab("Writing Scores")
b3


#Data visualization 4 -Bar chart
## to determine which level of education students of each group have.

ggplot(StudentsPerformance, mapping=aes(x = race_ethnicity_parental, fill=level_of_education)) + 
  geom_bar() +
  scale_y_continuous(limits=c(0,400),breaks = seq(0,400,50))+
  labs(x = "Race Group", y ="Level of Education",
       title = "Level of education by Race group")+
  theme_bw()

ggplot(StudentsPerformance, aes(x = gender, fill = lunch)) + 
  geom_bar() + 
  labs(x = "gender", y = "lunch", fill = "lunch")

#Data visualization 5 -Pie chart
## to see the proportion of education levels among the students

ggplot(StudentsPerformance, mapping=aes(x="", fill=level_of_education)) + 
geom_bar(width=1) +
coord_polar(theta = "y") +
labs(fill = "Level of Education",
 title = "Proportion of Education Level",
x = NULL, y = NULL) +
theme_void()

ggplot(StudentsPerformance, mapping=aes(x="", fill=race_ethnicity_parental)) +
  geom_bar(width=1) +
  coord_polar(theta = "y") +
  labs(fill = "Race",
       title = "Proportion of Race ethnicity",
       x = NULL, y = NULL) +
  theme_void()

ggplot(StudentsPerformance, mapping=aes(x="", fill=lunch)) +
  geom_bar(width=1) +
  coord_polar(theta = "y") +
  labs(fill = "Lunch",
       title = "Lunch by Gender",
       x = NULL, y = NULL) +
  theme_void()+
 facet_wrap(~gender)

dev.off()

