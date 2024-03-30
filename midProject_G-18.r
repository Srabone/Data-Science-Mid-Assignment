
mydataa<-read.csv("C:/Users/User/Desktop/Data Science/mid_project/Dataset_midterm_Section(C).csv",header=TRUE,sep=",")
mydataa
summary(mydataa)

is.na(mydataa)
which(is.na(mydataa$Age)) 
which(is.na(mydataa$Infection))
View(mydataa)

newDataset <- mydataa[, c("Age", "Infection", "Smoking", "SystolicBP", "DiastolicBP", 
                             "BS", "BodyTemp", "HeartRate", "RiskLevel")]
newDataset


library(dplyr)
install.packages(dplyr)


newDataset <- newDataset %>% 
mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

mydataaAvg <- as.data.frame(newDataset)
mydataaAvg
summary(mydataaAvg)
View(mydataaAvg)





numeric_columns <- mydataaAvg[, sapply(mydataaAvg, is.numeric)]
mean_values <- colMeans(numeric_columns)
mean_df <- data.frame(variable = names(mean_values), mean_value = mean_values)


library(ggplot2)


ggplot(mean_df, aes(x = variable, y = mean_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Values of Only Numeric Columns",
       x = "Column Name",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mydataaAvgN <- mydataaAvg %>%
  mutate(RiskLevel_numeric = case_when(
    RiskLevel == "high risk" ~ 1,
    RiskLevel == "low risk" ~ 0,
    RiskLevel == "mid risk" ~ 0.5,
    TRUE ~ NA_real_  
  )) %>%
  mutate(Infection_numeric = case_when(
    Infection == "yes" ~ 1,
    Infection == "no" ~ 0,
    Infection == "marginal" ~ 0.5,
    TRUE ~ NA_real_ 
  ))

View(mydataaAvgN)
head(mydataaAvgN)


newDatasetNumeric <- mydataaAvgN[, c("Age", "Infection_numeric", "Smoking", "SystolicBP", "DiastolicBP", 
                          "BS", "BodyTemp", "HeartRate", "RiskLevel_numeric")]
newDatasetNumeric
newDatasetNumeric <- na.omit(newDatasetNumeric)
summary(newDatasetNumeric)
View(newDatasetNumeric)


numeric_columns2 <- newDatasetNumeric[, sapply(newDatasetNumeric, is.numeric)]
mean_values <- colMeans(numeric_columns2)
mean_df2 <- data.frame(variable = names(mean_values), mean_value = mean_values)

ggplot(mean_df2, aes(x = variable, y = mean_value)) +
  geom_bar(stat = "identity", fill = "yellow") +
  labs(title = "Mean Values of All Columns",
       x = "Column Name",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



mean_age <- mean(newDatasetNumeric$Age)
median_age <- median(newDatasetNumeric$Age)
mode_age <- as.numeric(names(sort(-table(newDatasetNumeric$Age)))[1])
summary_stats <- data.frame(
  Statistic = c("Mean", "Median", "Mode"),
  Value = c(mean_age, median_age, mode_age)
)
print(summary_stats)

library(ggplot2)
ggplot(summary_stats, aes(x = Statistic, y = Value, fill = Statistic)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Summary Statistics of Age",
       x = "Statistic",
       y = "Value") +
  theme_minimal()

newDatasetNumeric$Smoking <- floor(newDatasetNumeric$Smoking)
head(newDatasetNumeric)

newDatasetNumeric$Smoking <- factor(newDatasetNumeric$Smoking, levels = c(1, 2, 3), labels = c("yes", "sometimes", "no"))

ggplot(newDatasetNumeric, aes(x = Smoking)) +
  geom_bar(width = 0.5, fill = "cyan") +  # Adjust width of bars and fill color
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust = -0.5) +  # Add count labels above bars
  labs(title = "Smoking Information",
       x = "Smoking",
       y = "Frequency") +
  theme_minimal()


newDatasetNumeric <- mydataaAvgN[, c("Age", "Infection_numeric", "Smoking", "SystolicBP", "DiastolicBP", 
                                     "BS", "BodyTemp", "HeartRate", "RiskLevel_numeric")]
newDatasetNumeric$Smoking <- floor(newDatasetNumeric$Smoking)
newDatasetNumeric
head(newDatasetNumeric)



newDatasetNumeric <- newDatasetNumeric[newDatasetNumeric$BodyTemp >= 0, ]
newDatasetNumeric$Age <- floor(newDatasetNumeric$Age)
newDatasetNumeric$DiastolicBP <- floor(newDatasetNumeric$DiastolicBP)
newDatasetNumeric$Infection_numeric[is.na(newDatasetNumeric$Infection_numeric)] <- 0
newDatasetNumeric$Infection_numeric <- na.omit(newDatasetNumeric$Infection_numeric)
head(newDatasetNumeric)



bodyTemp <- newDatasetNumeric$BodyTemp
below_normal_count <- 0
normal_range_count <- 0
illness_count <- 0

for (temp in bodyTemp) {
  if (temp < 97) {
    below_normal_count <- below_normal_count + 1
  } else if (temp >= 97 & temp <= 99) {
    normal_range_count <- normal_range_count + 1
  } else {
    illness_count <- illness_count + 1
  }
}
cat("Below normal Temperature:", below_normal_count, "\n",
    "Normal Temperature:", normal_range_count, "\n",
    "Ill people:", illness_count, "\n")

is.na(newDatasetNumeric)
is.na(mydataa)

most_frequent_DiastolicBP <- names(sort(table(newDatasetNumeric$DiastolicBP), decreasing = TRUE)[1])
most_frequent_DiastolicBP

barplot(table(newDatasetNumeric$DiastolicBP), main = "Count VS DiastolicBP", xlab = "DiastolicBP", ylab = "Count",ylim=c(0,110), col = c("lightgreen", "pink"))



heartRate_mean <- mean(newDatasetNumeric$HeartRate, na.rm = TRUE)
heartRate_sd <- sd(newDatasetNumeric$HeartRate, na.rm = TRUE)
heartRate_range <- range(newDatasetNumeric$HeartRate, na.rm = TRUE)
cat("Heart Rate -> Mean:", heartRate_mean, "SD:", heartRate_sd, "Range:", heartRate_range, "\n")

hist(newDatasetNumeric$HeartRate,main=" Data of Heart Rate", xlab="heart_rate", xlim = c(0,200),ylim=c(60,90), breaks=10)
boxplot(newDatasetNumeric$HeartRate, main = "Data of Heart Rate", ylab = "heart_rate")
summary(newDatasetNumeric$HeartRate)

names(newDatasetNumeric)


  

