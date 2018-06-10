# Set seed
set.seed(42)

# Replace names with random numbers from 1 to 1000
whitehouse_no_names <- whitehouse %>%
    mutate(Name = sample(1000, 469))

whitehouse_no_names
