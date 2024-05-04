library(tidyverse)
library(ggplot2)

# Read in the data
mydata <- read.csv("mushroom/mushroom_cleaned.csv")
head(mydata)

# Plot distribution of each class
ggplot(mydata, aes(x=class, fill=class)) +
  geom_bar() +
  scale_fill_manual(values = c("edible" = "green", "poisonous" = "red")) +
  labs(title="Mushroom Class Distribution", x="Class", y="Count") +
  theme_minimal()

# Plot distribution of each cap shape
ggplot(mydata, aes(x=cap.shape, fill=cap.shape)) +
  geom_bar() +
  scale_fill_manual(values = c("bell" = "green", "conical" = "red", "convex" = "blue", "flat" = "purple", "knobbed" = "orange", "sunken" = "yellow")) +
  labs(title="Mushroom Cap Shape Distribution", x="Cap Shape", y="Count") +
  theme_minimal()

# Scatter plot of cap shape vs. class
ggplot(mydata, aes(x=cap.shape, y=class, color=class)) +
  geom_jitter() +
  labs(title="Mushroom Cap Shape vs. Class", x="Cap Shape", y="Class") +
  theme_minimal()

# Scatter plot of stem height and width, with class as color
ggplot(mydata, aes(x=stem.height, y=stem.width, color=class)) +
  geom_point() +
  labs(title="Mushroom Stem Height vs. Width", x="Stem Height", y="Stem Width", color="Class") +
  theme_minimal()

# Scatter plot of cap diameter and stem height, with class as color
ggplot(mydata, aes(x=cap.diameter, y=stem.height, color=class)) +
  geom_point() +
  facet_wrap(, scales="free")
  labs(title="Mushroom Cap Diameter vs. Stem Height", x="Cap Diameter", y="Stem Height", color="Class") +
  theme_minimal()

# Histogram of cap diameter
ggplot(mydata, aes(x=cap.diameter)) +
  geom_histogram(binwidth=20, position="dodge") +
  labs(title="Mushroom Cap Diameter Distribution", x="Cap Diameter", y="Count", fill="Class") +
  theme_minimal()

# Boxplot of cap diameter, shape, gill attachment color, stem height, and stem width
long_data <- gather(mydata, key = "measurement", value = "value", cap.diameter, stem.height, stem.width)
ggplot(long_data, aes(x = class, y = value, fill = class)) +
  geom_boxplot() +
  facet_wrap(~ measurement, scales = "free") +
  labs(title = "Box Plots of Measurements by Class", x = "Class", y = "Value") +
  theme_minimal()

# Start building a decision tree model
library(rpart)
library(rpart.plot)
install.packages("rpart.plot")

# Split the data into training and testing sets`
set.seed(123)
train_index <- sample(1:nrow(mydata), 0.2*nrow(mydata))
train_data <- mydata[train_index,]
test_data <- mydata[-train_index,]
head(train_data)
lengths(train_data)
lengths(test_data)
head(test_data)

typeof(train_data)

# Fit a decision tree model
tree_model <- rpart(class ~ ., data=train_data, method="class")
rpart.plot(tree_model)

# Test on the test data
predictions <- predict(tree_model, test_data, type="class")
head(predictions)
actual <- test_data$class
correct_predictions <- sum(predictions == actual)
accuracy <- correct_predictions / nrow(test_data)
print(accuracy)

# Confusion matrix
confusion_matrix <- table(predictions, actual)
print(confusion_matrix)
