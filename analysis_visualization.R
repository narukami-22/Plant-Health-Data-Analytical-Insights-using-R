# Plant Health Analytics
packages <- c("ggplot2", "dplyr", "readr")

for(p in packages){
  if(!require(p, character.only = TRUE)){
    install.packages(p, repos="https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}

data <- read_csv("data/clean_data.csv")

print("First few rows:")
print(head(data))

data$health_status <- factor(
  data$label,
  levels = c(0,1,2),
  labels = c("Healthy", "Moderate Stress", "High Stress")
)

summary_stats <- summary(data)
print("Summary Statistics:")
print(summary_stats)

class_dist <- data %>%
  group_by(health_status) %>%
  summarise(count = n())

print("Class Distribution:")
print(class_dist)

p1 <- ggplot(class_dist, aes(x=health_status, y=count, fill=health_status)) +
  geom_bar(stat="identity") +
  ggtitle("Plant Health Condition Distribution") +
  xlab("Health Status") +
  ylab("Count")

ggsave("health_distribution.png", p1)

p2 <- ggplot(data, aes(x=Ambient_Temperature, fill=health_status)) +
  geom_histogram(bins=30, alpha=0.7) +
  ggtitle("Temperature vs Health")

ggsave("temperature_vs_health.png", p2)

p3 <- ggplot(data, aes(x=Humidity, fill=health_status)) +
  geom_histogram(bins=30, alpha=0.7) +
  ggtitle("Humidity vs Health")

ggsave("humidity_vs_health.png", p3)

p4 <- ggplot(data, aes(x=Light_Intensity, fill=health_status)) +
  geom_histogram(bins=30, alpha=0.7) +
  ggtitle("Light Intensity vs Health")

ggsave("light_vs_health.png", p4)

#Correlation
numeric_data <- data %>%
  select(Soil_Moisture, Ambient_Temperature, Humidity, Light_Intensity)

cor_matrix <- cor(numeric_data)

print("Correlation Matrix:")
print(cor_matrix)

#Automated Insights

insights <- c()

most_common <- class_dist$health_status[which.max(class_dist$count)]
insights <- c(insights, paste("Most frequent plant condition:", most_common))

avg_temp <- aggregate(Ambient_Temperature ~ health_status, data, mean)
highest_temp_class <- avg_temp$health_status[which.max(avg_temp$Ambient_Temperature)]
insights <- c(insights, paste("Highest temperature observed in:", highest_temp_class))

avg_light <- aggregate(Light_Intensity ~ health_status, data, mean)
lowest_light_class <- avg_light$health_status[which.min(avg_light$Light_Intensity)]
insights <- c(insights, paste("Lowest light levels observed in:", lowest_light_class))

writeLines(insights, "insights.txt")

print("Insights Generated:")
print(insights)

print("Analysis completed successfully.")
