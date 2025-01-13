dataset <- read.csv("F:\\Fall24\\DATA SCIENCE\\final_lab_task1\\shopping_trends.csv") 
str(dataset)
table(dataset$Location)
#histogram
hist(dataset$Age, 
     main = "Histogram of AGE", 
     xlab = "AGE", 
     col = "lightblue", 
     border = "black", 
     breaks = 20) 
#barplot
barplot(table(dataset$ Gender), 
        main = "Bar Chart of Gender", 
        xlab = "Gender", 
        ylab = "Frequency", 
        col = "orange", 
        border = "black")

# Box plot for AGE feature
boxplot(dataset$Review.Rating , 
        main = "Box Plot of Review rating", 
        ylab = "rating", 
        col = "lightblue", 
        border = "black")

#scattter plot
dataset$Frequency.of.Purchases <- factor(dataset$Frequency.of.Purchases, 
                                         levels = c("Annually", "Quarterly", "Every 3 Months", "Bi-Weekly", 
                                                    "Fortnightly", "Monthly", "Weekly"), 
                                         ordered = TRUE)

ggplot(dataset, aes(x = Age, y = Review.Rating, color = Frequency.of.Purchases)) +
  geom_point(alpha = 0.7) +  
  labs(title = "Age vs. Review Rating Colored by Frequency of Purchases",
       x = "Age", y = "Review Rating", color = "Frequency of Purchases") +
  scale_color_manual(values = c("Annually" = "red", 
                                "Quarterly" = "orange", 
                                "Every 3 Months" = "yellow", 
                                "Bi-Weekly" = "green", 
                                "Fortnightly" = "lightgreen",
                                "Monthly" = "limegreen",
                                "Weekly" = "green4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

library(ggplot2)

#violin plot
ggplot(dataset, aes(x = Frequency.of.Purchases, y = Purchase.Amount..USD., fill = Frequency.of.Purchases)) +
  geom_violin(trim = FALSE) +  
  geom_boxplot(width = 0.1, color = "black", alpha = 0.3) +  
  labs(title = "Violin Plot with Box Plot for Purchase Amount vs. Frequency of Purchases",
       x = "Frequency of Purchases", y = "Purchase Amount (USD)") +
  theme_minimal() +
  scale_fill_viridis_d()  



#line graph
purchase_by_location <- dataset %>%
  group_by(Location) %>%
  summarise(Avg.Purchase.Amount = mean(Purchase.Amount..USD.))

ggplot(purchase_by_location, aes(x = Location, y = Avg.Purchase.Amount)) +
  geom_line(group = 1, color = "blue") +  # Line graph
  geom_point(color = "red") +             
  labs(title = "Average Purchase Amount by Location",
       x = "Location", y = "Average Purchase Amount (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


purchase_by_season <- dataset %>%
  group_by(Season) %>%
  summarise(Avg.Purchase.Amount = mean(Purchase.Amount..USD.))

ggplot(purchase_by_season, aes(x = Season, y = Avg.Purchase.Amount, group = 1)) +
  geom_line(color = "blue") +   # Line graph
  geom_point(color = "red") +   
  labs(title = "Average Purchase Amount by Season",
       x = "Season", y = "Average Purchase Amount (USD)") +
  theme_minimal()


