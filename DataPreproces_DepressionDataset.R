library(dplyr)
library(visdat) 
dataset <- read.csv("F:\\Fall24\\DATA SCIENCE\\mid project\\Midterm_Dataset_Section(B).csv")
str(dataset)

num_rows <- nrow(dataset)
num_cols <- ncol(dataset)
cat("The dataset has", num_rows, "rows and", num_cols, "columns.\n")

colSums(is.na(dataset))
sum(is.na(dataset))
vis_miss(dataset)

dataset$Age[is.na(dataset$Age)] <- mean(dataset$Age, na.rm = TRUE)
dataset$Study.Hours[is.na(dataset$Study.Hours)] <- median(dataset$Study.Hours, na.rm = TRUE)
gender_freq <- sort(table(dataset$Gender), decreasing = TRUE) 
gender_mode <- as.character(gender_freq[1]) 
dataset$Gender[is.na(dataset$Gender)] <- gender_mode


summary(dataset)
str(dataset)

sum(duplicated(dataset))
dataset <- dataset[!duplicated(dataset), ]
sum(duplicated(dataset))

lapply(dataset[, sapply(dataset, is.character)], unique)
dataset[dataset == ""] <- NA
dataset$Have.you.ever.had.suicidal.thoughts<- recode(dataset$Have.you.ever.had.suicidal.thoughts,
                                                        "Yess" = "Yes", 
                                                        "Noo" = "No")

dataset$Gender[is.na(dataset$Gender)] <- "Male" 
dataset$Sleep.Duration[is.na(dataset$Sleep.Duration)] <- "7-8 hours"  
dataset$Depression[is.na(dataset$Depression)] <- "No"
head(dataset)

sapply(dataset, class)
sapply(dataset, function(x) sum(!is.numeric(x) & !is.na(x)))

sapply(dataset[, sapply(dataset, is.numeric)], function(x) range(x, na.rm = TRUE))

boxplot(dataset$Age, main = "Age (Before Outlier Handling)")
boxplot(dataset$Academic.Pressure, main = "Academic Pressure (Before Outlier Handling)")

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(list(outliers = x[x < lower | x > upper], 
              lower_bound = lower, 
              upper_bound = upper))
}
numeric_columns <- sapply(dataset, is.numeric)
outlier_info <- lapply(dataset[, numeric_columns], detect_outliers)
outlier_info

dataset$Age[dataset$Age %in% c(230, 226)] <- median(dataset$Age, na.rm = TRUE)
dataset$Academic.Pressure[dataset$Academic.Pressure %in% c(20, 15)] <- median(dataset$Academic.Pressure, na.rm = TRUE)


boxplot(dataset$Age, main = "Age (After Outlier Handling)")
boxplot(dataset$Academic.Pressure, main = "Academic Pressure (After Outlier Handling)")


dataset$Gender <- ifelse(dataset$Gender == "Male", 1, 0)
dataset$Depression <- ifelse(dataset$Depression == "Yes", 1, 0)
dataset$Have.you.ever.had.suicidal.thoughts <- ifelse(dataset$Have.you.ever.had.suicidal.thoughts == "Yes", 1, 0)
dataset$Family.History.of.Mental.Illness <- ifelse(dataset$Family.History.of.Mental.Illness == "Yes", 1, 0)
unique(dataset$Sleep.Duration) 
dataset$Sleep.Duration <- as.numeric(factor(dataset$Sleep.Duration, 
                                            levels = c("Less than 5 hours","5-6 hours", "7-8 hours", "More than 8 hours"), 
                                            labels = c(1, 2, 3,4)))
unique(dataset$Dietary.Habits)  
dataset$Dietary.Habits <- as.numeric(factor(dataset$Dietary.Habits, 
                                            levels = c("Unhealthy", "Moderate", "Healthy"), 
                                            labels = c(1, 2, 3)))

head(dataset)

dataset$Age <- (dataset$Age - min(dataset$Age)) / (max(dataset$Age) - min(dataset$Age))
head(dataset)

yes_obs <- which(dataset$Depression == 1)
no_obs <- which(dataset$Depression == 0)
yes_count <- length(yes_obs)
no_count <- length(no_obs)
balanced_yes_sample <- sample(yes_obs, no_count, replace = TRUE)
oversampled_dataset <- dataset[c(balanced_yes_sample, no_obs), ]
oversampled_dataset %>% count(Depression)

yes_obs <- which(dataset$Depression == 1)
no_obs <- which(dataset$Depression == 0)
yes_count <- length(yes_obs)
no_count <- length(no_obs)
balanced_no_sample <- sample(no_obs, yes_count, replace = FALSE)
undersampled_dataset <- dataset[c(yes_obs, balanced_no_sample), ]
undersampled_dataset %>% count(Depression)

write.csv(oversampled_dataset, "Midterm_Dataset_Section_Processed.csv",)
