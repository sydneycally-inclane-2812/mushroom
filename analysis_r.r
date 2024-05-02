library(tidyverse)
library(ggplot2)

# Read in the data
mydata <- read.csv("mushroom/mushroom_cleaned.csv")
head(mydata)

# Plot the data
ggplot()

# Data types
x <- 2L
y <- 3L
z <- 4L


myvar = x + y + z
typeof(x)