library(dplyr)

snapchat_data <- read.csv("D:/inclass/3-HK1/PTTK/project/data/average_monthly_rating_snapchat.csv")
messenger_data <- read.csv("D:/inclass/3-HK1/PTTK/project/data/average_monthly_rating_messenger.csv")

snapchat_data$App <- "Snapchat"
messenger_data$App <- "Messenger"

combined_data <- bind_rows(snapchat_data, messenger_data)

combined_data

#T_test ----------------------------------------------------
t_test_result <- t.test(averageRating ~ App, data = combined_data)

print(t_test_result)


#Chi_square test -------------------------------------------
combined_data$RatingCategory <- ifelse(combined_data$averageRating >= 3, "High", "Low")

combined_data

contingency_table <- table(combined_data$RatingCategory, combined_data$App)
print(contingency_table)

chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)


#Confidence Interval ---------------------------------------
calculate_ci <- function(data, confidence_level = 0.95) {
  n <- length(data)
  mean_value <- mean(data)
  std_error <- sd(data) / sqrt(n)
  t_critical <- qt(1 - (1 - confidence_level) / 2, df = n - 1)
  error_margin <- t_critical * std_error
  lower_bound <- mean_value - error_margin
  upper_bound <- mean_value + error_margin
  return(c(mean = mean_value, lower = lower_bound, upper = upper_bound))
}

snapchat_ci <- calculate_ci(snapchat_data$averageRating)
print("Snapchat Confidence Interval:")
print(snapchat_ci)

messenger_ci <- calculate_ci(messenger_data$averageRating)
print("Messenger Confidence Interval:")
print(messenger_ci)
