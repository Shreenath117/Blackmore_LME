# Case Study Solutions : Blackmore.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('blackmore.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:5]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 945 rows and 4 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is :
# Model to determine the typical trajectory of exercise over time and does it differ b/w eating disordered and control subject groups

# The response variable : exercise
# The data collected in the given excel file is grouped under which data class : "Longitudinal or Panel Data"

#-------------------------------------------------------------------------------------------------

# Soln. to Question 3:

#Summarising the dataset : 
summary(data)

# Observations :
# The median age of the subject is 12 years with min being 8 out of the 138 teenage girls
# Out of the total observations : 359 belongs to control group and 586 in the patient

# Check the datatypes
str(data)

sapply(data, class)

# Here : subject and group are factor variables whereas the rest are numeric

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

hist(data$age,xlab = "Age",col = "red", xlim = c(8,20),breaks = 5)
table(data$age)

# Observations :
# Clearly seen, Age 8 is the min age to be kept as baseline in the model has max observations 
# Age group : 8-12 hasthe maximum observations

#-------------------------------------------------------------------------------------------------
# Soln. to Question 5:
install.packages("ggplot2")
library(ggplot2)
ggplot(data = data, aes(x = age , y = exercise)) +
  geom_point(aes(color = group)) + labs(titles = 'Exercise Histories of Eating Disordered and Control Subjects', x = 'Age of Subjects', y = 'Hours of Exercise Per Week')

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# The scatterplot above shows an obvious peak in hours of exercise for both study groups at around age 15. 
# These results may occur because of the start of high school around this age, 
# which can push these young women to care more about their appearance to fit the societal perception of beauty. 
# Their exercise hours increase as a result which endangers those who have already been diagnosed with an eating disorder.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

data <- 
  data %>%
  mutate(
    age = floor(age)
  )

glimpse(data)
#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

last_age <- 
  data %>%
  group_by(
    subject
  ) %>%
  filter(
    age == max(age)
  ) %>%
  ungroup()
mean_last_age_exercise <-
  last_age %>%
  group_by(group, age) %>%
  summarize(mean = mean(exercise))

mean_last_age_exercise %>%
  as.data.frame() 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

mean_last_age_exercise <-
  last_age %>%
  group_by(group, age) %>%
  summarize(mean = mean(exercise)) %>%
  spread(key = group, value = mean)

mean_last_age_exercise %>%
  as.data.frame() 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:
mean_last_age_exercise <- 
  last_age %>%
  group_by(
    group,
    age
  ) %>%
  summarise(
    mean = mean(exercise),
    number = n()
  ) %>%
  gather(key = stat, value = value, mean, number) %>%
  unite(stat_name, sep = "-", group, stat) %>%
  spread(key = stat_name, value = value)

mean_last_age_exercise %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

blackmore_group_size <- 
  last_age %>%
  group_by(
    group,
    age
  ) %>%
  summarise(
    count = n()
  ) %>%
  ungroup()

ggplot(blackmore_group_size, aes(x = count)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# Computing Mean exercise by group

install.packages("tidyverse")
library(tidyverse)
mean_exercise <- 
  data %>%
  group_by(
    group
  ) %>%
  summarize(mean = mean(exercise))

mean_exercise %>%
  as.data.frame()

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

plot(exercise ~ group, data = data, main = 'Exercise Histories of Eating Disordered and Control Subjects', xlab = 'Group', ylab = 'Hours of Exercise Per Week')

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# The hours of exercise per week for the anorexic patients vary from 0 to 30, with many results in the higher range. 
# The hours of exercise per week for the control group vary from 0 to 12 with many results clumped in the 0 to 5 range. 
# As shown in the above plot : 
# the mean of the patients' hours of exercise is 3.075887 while the control group's is 1.640641.

# From this data we can conclude: 
# the hours of exercise for anorexic patients vary at much higher degrees than the hours for the control group and the hours of exercise peak at high school ages.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:
data %>% dplyr :: select(subject,group,exercise) %>%
group_by(subject,group) %>% summarise(exercise=mean(exercise))->df

df

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

data %>%
  group_by(exercise) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))

# There are 112 rows having 0 value in exercise column

data$exercise_log <- log(data$exercise + 5/60, 2)

data
#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

plot(exercise_log ~ group, data = data, main = 'Exercise Histories of Eating Disordered and Control Subjects', xlab = 'Group', ylab = 'Log Hours of Exercise Per Week')

# Observations
# Lot of outliers : were observed in the previous plot of exercise vs group box plot compared to the plot above there isn't any as we normalised the exercise variable by log
# Here : as we see median of patient is higher than that of the control group

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# The observations per subject is small.
# Hence, we should not be expecting very good estimates of the within subject regression coefficients by OLS  method.  
# That's why we go for the mixed models as they can provide the improved estimates of the within subjects ( Groups ) coefficients 
# and the random effects by pooling information across subjects .

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

install.packages("lme4")
library(lme4)

LMM_model <- lme(exercise_log ~ group * I(age - 8), random= ~I(age - 8) | subject,data=data)

summary(LMM_model)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# There is a small, and marginally statistically significant, average age trend in the control group
# (represented by the fixed-effect coefficient for age - 8), and a highly significant interaction of
# age with group, reflecting a much steeper average trend in the patient group. The small and
# non-significant coefficient for group indicates similar age-8 intercepts for the two groups

#-------------------------------------------------------------------------------------------------

