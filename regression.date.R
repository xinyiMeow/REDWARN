library(ggplot2)
library(dplyr)

# ## Mean plot for each day
# Read the data
df <- read.csv("austin_stages.csv", sep = ",", header = TRUE)
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
str(df)
# Sorting the data by date just in case
df <- df %>% arrange(Date)

# Check for any duplicate dates with different stage numbers
duplicated_stages <- df %>%
  group_by(Date) %>%
  summarise(unique_stages = n_distinct(stage_num))

# View the summary to understand the distribution of stages
print(duplicated_stages)

# Calculate the mean polarity for each day
daily_mean <- df %>%
  group_by(Date) %>%
  summarise(mean_polarity = mean(polarity, na.rm = TRUE),
            stage_num = first(stage_num), .groups = 'drop')  # Just take the first stage_num since they should all be the same
daily_mean$group <- 1
ggplot(daily_mean, aes(x = Date, y = mean_polarity, group = group, color = as.factor(stage_num))) +
  geom_line() +  # Connects all points with a single group
  scale_color_manual(values = rainbow(7)) +  # Assign colors
  theme_minimal() +
  labs(x = "Date", y = "Mean Polarity", title = "Mean Polarity by Day with Stage Color", color = "Stage Number") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis text

# Save the plot
ggsave("mean_polarity_by_day.pdf", plot = last_plot(), width = 11, height = 8, units = "in")

# Split the data frame by 'stage_num'
list_of_dfs <- split(df, df$stage_num)

# Now, list_of_dfs is a list where each element is a data frame corresponding to a stage
df_stage_0 <- list_of_dfs[[1]]  # Data frame for stage 0
df_stage_1 <- list_of_dfs[[2]]  # Data frame for stage 1
df_stage_2 <- list_of_dfs[[3]]  # Data frame for stage 2
df_stage_3 <- list_of_dfs[[4]]
df_stage_4 <- list_of_dfs[[5]]
df_stage_5 <- list_of_dfs[[6]]
for (stage in unique(df$stage_num)) {
  assign(paste0("df_stage_", stage), list_of_dfs[[as.character(stage)]])
}

list_of_dfs <- split(df, df$stage_num)
# Find the earliest date to use as a reference
ref_date <- min(df$Date)

# Convert the 'Date' column in each data frame to a numeric value representing days since the reference date
list_of_dfs <- lapply(list_of_dfs, function(x) {
  x$Date_numeric <- as.numeric(x$Date - ref_date)
  return(x)
})
# Initialize a list to store regression models
regression_models <- list()

for (stage in names(list_of_dfs)) {
  # Run the regression of polarity on the numeric date
  regression_models[[stage]] <- lm(polarity ~ Date_numeric, data = list_of_dfs[[stage]])
}
# Print summaries of regression models
lapply(regression_models, summary)

# Or store the summaries in a list if you want to access them later
regression_summaries <- lapply(regression_models, summary)

for (stage in names(list_of_dfs)) {
  # Add the fitted values from the model to the dataframe
  list_of_dfs[[stage]]$Fitted <- predict(regression_models[[stage]], list_of_dfs[[stage]])
}

# Function to create regression plot with equation and p-value annotation
plot_regression <- function(data, stage) {
  model <- regression_models[[stage]]
  
  # Check if the model is valid
  if (!is.null(model) && length(coef(model)) > 1) {
    # Extract the coefficients and p-value
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
    
    # Format the coefficients and p-value in scientific notation if needed
    intercept_formatted <- format(intercept, scientific = TRUE)
    slope_formatted <- format(slope, scientific = TRUE)
    p_value_formatted <- format(p_value, scientific = TRUE)
    
    # Create the subtitle text with the model equation and p-value
    subtitle_text <- paste("Fitted model: Polarity =",
                           intercept_formatted, "+", slope_formatted, "* Days since", format(ref_date, "%b %d, %Y"),
                           "; p-value:", p_value_formatted)
    
    # Ensure 'Date' is of class Date for plotting
    data$Date <- as.Date(data$Date)
    
    # Plot the data and the fitted line
    p <- ggplot(data, aes(x = Date, y = polarity)) +
      geom_point(aes(color = as.factor(stage_num)), alpha = 0.5) +
      geom_line(aes(y = Fitted), color = "skyblue", linewidth = 1) +
      labs(title = paste("Stage", stage, "Regression of Polarity on Date"),
           subtitle = subtitle_text,
           x = "Date",
           y = "Polarity") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Save the plot
    ggsave(paste0("stage_", stage, "_regression_plot.pdf"), plot = p, width = 10, height = 6, units = "in")
  } else {
    warning(paste("The model for stage", stage, "is not valid and was not plotted."))
  }
}
regression_models <- list()

# Only run regressions and create plots for stages 3, 4, and 5
selected_stages <- c("3", "4", "5")

for (stage in selected_stages) {
  if (stage %in% names(list_of_dfs)) {
    # Run the regression of polarity on the numeric date
    regression_models[[stage]] <- lm(polarity ~ Date_numeric, data = list_of_dfs[[stage]])
    
    # Add the fitted values from the model to the dataframe
    list_of_dfs[[stage]]$Fitted <- predict(regression_models[[stage]], newdata = list_of_dfs[[stage]])
    
    # Create a plot
    plot_regression(list_of_dfs[[stage]], stage)
  }
}
