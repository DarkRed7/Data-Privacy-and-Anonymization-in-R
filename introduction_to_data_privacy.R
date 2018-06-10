setwd("~/GitHub/Data-Privacy-and-Anonymization-in-R")

# libraries
library(dplyr)

# load data (RDS format)
whitehouse <- readRDS("whitehouse.RDS")
fertility <- readRDS("fertility.RDS")

# Set seed
set.seed(42)

# 1. Removing names
# Replace names with random numbers from 1 to 1000
whitehouse_no_names <- whitehouse %>%
    mutate(Name = sample(1000, 469))

whitehouse_no_names

# 2. Rounding salaries
# Rounding Salary to the nearest ten thousand
whitehouse_no_identifiers <- whitehouse_no_names %>%
  mutate(Salary = round(Salary,-4))

whitehouse_no_identifiers

# 3. Genarlization - continuous to categorical
# Convert the salaries into three categories
whitehouse.gen <- whitehouse %>%
  mutate(Salary = ifelse(Salary < 50000, 0, 
                         ifelse(Salary >= 50000 & Salary < 100000, 1, 2)))

whitehouse.gen

# 4. Bottom Coding
whitehouse.bottom <- whitehouse %>%
  mutate(Salary = ifelse(Salary <= 45000, 45000, Salary))

# Filter Results	
whitehouse.bottom %>%
  filter (Salary <=45000)

# 5. Synthetic data

# View fertility data 
fertility

# Get distributions
# Number of participants with Surgical_Intervention and Diagnosis
fertility %>%
  summarise_at(vars(Surgical_Intervention , Diagnosis ), sum)

# Mean and Standard Deviation of Age
fertility %>%
  summarise_at(vars(Age), funs(mean, sd))

# Counts of the Groups in High_Fevers	
fertility %>%
  count(High_Fevers)

# Counts of the Groups in Child_Disease	and Accident_Trauma	
fertility %>%
  count(Child_Disease,Accident_Trauma)

# Binomial distribution
# Find proportions
fertility %>%
  summarise_at(vars(Accident_Trauma, Surgical_Intervention), mean)

# Normal distribution
# Generate Synthetic data
accident <- rbinom(100, 1, 0.44)
surgical <- rbinom(100, 1, 0.51)

# Square root Transformation of Salary
whitehouse.salary <- whitehouse %>%
  mutate(Salary = sqrt(Salary))

# Calculate the mean and standard deviation
stats <- whitehouse.salary %>%
  summarise_at(vars(Salary),funs(mean,sd))

stats

# Generate Synthetic data
salary_transformed <- rnorm(nrow(whitehouse), 279, 71.8)

# Power transformation
salary_original <- salary_transformed^2

# Hard bound
salary <- ifelse(salary_original < 0, 0, salary_original)