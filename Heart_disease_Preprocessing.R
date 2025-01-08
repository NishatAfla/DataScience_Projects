

install.packages("ggcorrplot")
library(ggcorrplot)


heart_data <- read.csv("C:\\Users\\ASUS\\Downloads\\archive\\heartDisease.csv")
head(heart_data)
str(heart_data)


is.null(heart_data)
sum(is.na(heart_data)) 
summary(heart_data)

numeric_features <- heart_data[, sapply(heart_data, is.numeric) & !(names(heart_data) == "target")]
cor_matrix <- cor(numeric_features)
print(cor_matrix)
ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap")

# Identify categorical variables(p<0.05 / very low= significant)
categorical_features <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
chi_square_results <- lapply(categorical_features, function(feature) {
  table_data <- table(heart_data[[feature]], heart_data$target)
  chisq.test(table_data)
})
names(chi_square_results) <- categorical_features
sapply(chi_square_results, function(test) test$p.value)

# Compute variance for numeric features(high variance most significant)
variances <- apply(numeric_features, 2, var)
variances

# Pearson correlation (high / significance)
cor_target <- cor(numeric_features, heart_data$target)
cor_target

# Identify numeric features
numeric_features <- heart_data[, sapply(heart_data, is.numeric)]

# Perform ANOVA for each numeric feature against the target
anova_results <- lapply(names(numeric_features), function(feature) {
  # Run ANOVA using the formula: feature ~ target
  aov_result <- aov(heart_data[[feature]] ~ heart_data$target)
  # Return summary of the ANOVA result
  summary(aov_result)
})

anova_results

# Load the required library for mutual information
install.packages("infotheo")
library(infotheo)

# Calculate mutual information between features and the target 
mutual_info_results <- sapply(categorical_features, function(feature) {
  mutinformation(discretize(heart_data[[feature]]), discretize(heart_data$target))
})
mutual_info_results

# Drop features from the dataset
heart_data <- heart_data[, !(names(heart_data) %in% c("fbs", "restecg"))]

