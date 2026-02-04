#Plant Health Analytics using R

options(repos = c(CRAN = "https://cloud.r-project.org"))


install.packages(c("ggplot2", "dplyr", "tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("data/clean_data.csv")
head(data)

colnames(data) <- c("soil", "temperature", "humidity", "light", "label")

data$health_status <- factor(
  data$label,
  levels = c(0, 1, 2, 3),
  labels = c(
    "Low Light Stress",
    "Heat Stress",
    "Environmental Stress",
    "Healthy"
  )
)

summary(data)
table(data$health_status)

# Distribution of Plant Health Conditions
p1 <- ggplot(data, aes(x = health_status)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribution of Plant Health Conditions",
    x = "Plant Health Status",
    y = "Count"
  ) +
  theme_minimal()

print(p1)

# Temperature vs Plant Health
p2 <- ggplot(data, aes(x = temperature, fill = health_status)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(
    title = "Temperature Distribution by Plant Health",
    x = "Temperature (Normalized)",
    y = "Count"
  ) +
  theme_minimal()

print(p2)

#Humidity vs Plant Health
p3 <- ggplot(data, aes(x = health_status, y = humidity, fill = health_status)) +
  geom_boxplot() +
  labs(
    title = "Humidity vs Plant Health Status",
    x = "Plant Health Status",
    y = "Humidity (Normalized)"
  ) +
  theme_minimal()

print(p3)

#Light Intensity vs Plant Health
p4 <- ggplot(data, aes(x = light, color = health_status)) +
  geom_density(size = 1.2) +
  labs(
    title = "Light Intensity Distribution by Plant Health",
    x = "Light Intensity (Normalized)",
    y = "Density"
  ) +
  theme_minimal()

print(p4)

#Correlation

numeric_data <- data %>% select(temperature, humidity, light)
cor(numeric_data)

ggsave("outputs/plots/health_distribution.png", p1)
ggsave("outputs/plots/temperature_distribution.png", p2)
ggsave("outputs/plots/humidity_boxplot.png", p3)
ggsave("outputs/plots/light_density.png", p4)

most_common_condition <- names(sort(table(data$health_status), decreasing = TRUE))[1]
cat("Most common plant condition:", most_common_condition, "\n")

healthy_percent <- round(
  (sum(data$health_status == "Healthy") / nrow(data)) * 100, 2
)
cat("Percentage of healthy plants:", healthy_percent, "%\n")

numeric_data <- data[, c("temperature", "humidity", "light")]
cor_values <- cor(numeric_data, as.numeric(data$label))

most_influential <- rownames(cor_values)[which.max(abs(cor_values))]
cat("Most influential environmental factor:", most_influential, "\n")

if (mean(data$temperature) > 0.75) {
  cat("Warning: High average temperature may cause heat stress.\n")
}

if (mean(data$light) < 0.40) {
  cat("Warning: Low light levels may cause light stress.\n")
}

sink("outputs/insights.txt")

cat("Automated Plant Health Insights\n")
cat("-------------------------------\n")
cat("Most common plant condition:", most_common_condition, "\n")
cat("Percentage of healthy plants:", healthy_percent, "%\n")
cat("Most influential environmental factor:", most_influential, "\n")

if (mean(data$temperature) > 0.75) {
  cat("Warning: High average temperature may cause heat stress.\n")
}

if (mean(data$light) < 0.40) {
  cat("Warning: Low light levels may cause light stress.\n")
}

sink()

