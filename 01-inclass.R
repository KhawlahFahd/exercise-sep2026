#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

names(yrbss)

yrbss$Grade <- yrbss$grade
yrbss$Gender <- yrbss$gender
# checking the categories 
unique(yrbss$Grade)
unique(yrbss$Gender)
# change to increasing order
yrbss$Grade <- factor(yrbss$Grade,
levels =c("9", "10", "11", "12", "other"),
labels =c("9", "10", "11", "12", "Other"))
# change to capital letters
yrbss$Gender <- str_to_title(yrbss$Gender)
z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

# creating table of the average number of physically active days withen each grade and gender
mean_active <- aggregate(physically_active_7d ~ Grade + Gender,
data = yrbss,
FUN = mean)
ft_2 <- as_flextable(mean_active)
ft_2

# creating the plot 
mean_active |> 
  ggplot(aes(
    x = Grade,
    y = physically_active_7d,
    color = Gender, 
    group = Gender )) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Physical Activity by Grade and Gender",
    x = "Grade",
    y = "Mean Number of Physically Active Days",
    color = "Gender"
  ) +
  theme_minimal()
# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

# creating the bmi
yrbss <- yrbss|>
  mutate(bmi = weight/(height)^2)

summary(yrbss$bmi)
female_12 <- yrbss |>
  filter(Grade == "12", Gender == "Female")

female_12 |> 
  filter(!is.na(physically_active_7d), !is.na(bmi))|>
  ggplot(aes(x = factor(physically_active_7d), y = bmi)) +
  geom_boxplot(fill = "lightblue", color = "steelblue")+
  labs(
    title = "Physical Activity and BMI Among Grade 12 Females", 
    x = "Number of Physically Active Days",
    y = "BMI ( Weight in kg/ Height in m^2 )"
  ) +
  theme_minimal()

# Push your completed code to your GitHub repository
