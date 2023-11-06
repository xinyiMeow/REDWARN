library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("austin_stages.csv", sep = ",", header = TRUE)
data <- data[, -2] #exclude the "body" column which is not used
dim(data)
colnames(data)
head(data)
## regress
data <- data %>%
  mutate(stage_num = if_else(stage_num == 0, 6, stage_num))
 
# # Run the linear regression with the updated stage_num
model <- lm(polarity ~ stage_num, data = data)
# Get the summary of the model
# Get the summary of the model
model_summary <- summary(model)
# Run the linear regression model
model <- lm(polarity ~ stage_num, data = data)

# Get the summary of the model
model_summary <- summary(model)

# Extract the coefficients with more precision
intercept <- formatC(model_summary$coefficients[1, "Estimate"], format = "e", digits = 2)
slope <- formatC(model_summary$coefficients[2, "Estimate"], format = "e", digits = 2)

# Extract the p-value with more precision
p_value <- formatC(model_summary$coefficients[2, "Pr(>|t|)"], format = "e", digits = 2)

# Create the subtitle text with the model equation and p-value
subtitle_text <- paste("Fitted model: Polarity =", intercept, slope, "* Stage;",
                       "p-value for Stage:", p_value)

# Create the plot with the subtitle
p <- ggplot(data, aes(x = stage_num, y = polarity)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(
    x = "Stage Number",
    y = "Polarity",
    title = "Regression of Polarity on Stage",
    subtitle = subtitle_text
  ) +
  theme_minimal()
ggsave(filename = "polarity.regression.pdf", plot = p)

# Fit the model with subjectivity as the response variable
model_subjectivity <- lm(subjectivity ~ stage_num, data = data)

# Get the summary of the model
model_summary_subjectivity <- summary(model_subjectivity)

# Extract the coefficients for subjectivity
intercept_subjectivity <- round(model_summary_subjectivity$coefficients[1, "Estimate"], 2)
slope_subjectivity <- round(model_summary_subjectivity$coefficients[2, "Estimate"], 2)
p_value_subjectivity <- round(model_summary_subjectivity$coefficients[2, "Pr(>|t|)"], 4)

# Create the subtitle text with the model equation and p-value for subjectivity
subtitle_text_subjectivity <- paste("Fitted model: Subjectivity =", intercept_subjectivity, "+", slope_subjectivity, "* StageNum;",
                                    "p-value:", p_value_subjectivity)

# Create the plot with subjectivity
p_subjectivity <- ggplot(data, aes(x = stage_num, y = subjectivity)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(
    x = "Stage Number",
    y = "Subjectivity",
    title = "Regression of Subjectivity on Stage Number",
    subtitle = subtitle_text_subjectivity
  ) +
  theme_minimal()
ggsave("subjectivity.regression.pdf", plot = p_subjectivity)

## mean plot
mean_se_data <- data %>%
  group_by(stage_num) %>%
  summarise(
    mean_polarity = mean(polarity, na.rm = TRUE),
    se_polarity = sd(polarity, na.rm = TRUE) / sqrt(n()),
    mean_subjectivity = mean(subjectivity, na.rm = TRUE),
    se_subjectivity = sd(subjectivity, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()
long_data <- mean_se_data %>%
  pivot_longer(
    cols = -stage_num,
    names_to = c(".value", "metric"),
    names_pattern = "(mean|se)_(.*)"
  )

# Filter for subjectivity and plot
subjectivity_plot <- ggplot(long_data %>% filter(metric == "subjectivity"),
                            aes(x = as.factor(stage_num), y = mean, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_fill_manual(values = c("subjectivity" = "pink")) +
  labs(x = "Stage Number", y = "Mean Subjectivity", fill = "Metric", title = "Mean of Subjectivity in Different Stages") +
  theme_minimal()

# Filter for polarity and plot
polarity_plot <- ggplot(long_data %>% filter(metric == "polarity"),
                        aes(x = as.factor(stage_num), y = mean, fill = metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  scale_fill_manual(values = c("polarity" = "skyblue")) +
  labs(x = "Stage Number", y = "Mean Polarity", fill = "Metric", title = "Mean of Polarity in Different Stages") +
  theme_minimal()


ggsave("polarity.pdf", plot = polarity_plot)
ggsave("subjectivity.pdf", plot = subjectivity_plot)

