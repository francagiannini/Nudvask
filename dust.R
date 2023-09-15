# Load the "MASS" package
library(MASS)

# Load the Boston data set
data(Boston)

# View the first few rows of the data
head(Boston)

# Select the response variable and predictor variables
response_variable <- "medv"
predictor_variables <- c("rm", "lstat")

# Create a new data frame with only the selected variables
data_subset <- Boston[, c(response_variable, predictor_variables)]

# Remove any rows with missing data
data_subset <- na.omit(data_subset)
# Set the seed for reproducibility
set.seed(123)

# Split the data into a training set and a testing set
training_indices <- sample(nrow(data_subset), nrow(data_subset) * 0.7)
training_data <- data_subset[training_indices, ]
testing_data <- data_subset[-training_indices, ]

# Load the "randomForest" package
library(randomForest)
library(caret)

# Build the random forest regression model
model <- randomForest(
  formula = medv ~ .,
  data = training_data,
  ntree = 500,
  mtry = 2,
  importance = TRUE
)

# train model
fitControl <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 3)

#create tunegrid
tunegrid <- expand.grid(.mtry = seq(2,6,1))


model <- caret::train(
  medv ~ .,#crim+ zn +indus+ chas   +nox   + rm  +age + +  dis+ rad +tax +ptratio + black+ lsta,
  method= "rf",
  data = training_data,
  trControl=fitControl, 
  tuneGrid = tunegrid,
  #ntree = 500,
  #mtry = 2,
  importance = TRUE
)

# Make predictions on the testing data
predictions <- predict(model, newdata = testing_data)

# Calculate the RMSE and R-squared
library(Metrics)
rmse <- rmse(predictions, testing_data$medv)
r_squared <- cor(predictions, testing_data$medv)

plot(predictions, testing_data$medv)

# Print the results
cat("RMSE:", round(rmse, 2), "\n")
cat("R-squared:", round(r_squared, 2), "\n")
