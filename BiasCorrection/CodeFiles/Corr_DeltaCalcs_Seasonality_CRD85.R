#CRD Monthly Temp Changes RCP 8.5

rm(list = ls())

# Load required libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

# Read in the dataset (update file path as necessary)
# Assuming the dataset is stored in a CSV file
Mod_CRD85 <- read.csv("Corr_Mod_CRD85.csv")

# Convert 'time' to Date and extract year and month
Mod_CRD85$time <- as.Date(Mod_CRD85$time, format = "%Y-%m-%d")
Mod_CRD85$Year <- year(Mod_CRD85$time)
Mod_CRD85$Month <- format(Mod_CRD85$time, "%B")

# Calculate monthly averages for each decade
calculate_monthly_averages <- function(data, decade_start, decade_end) {
  # Initialize empty dataframe to store monthly averages
  monthly_avg_table <- data.frame(Pond = character(), Month = factor(), AvgTemp = numeric(), Year = integer(), stringsAsFactors = FALSE)
  
  # Loop through each year in the decade
  for (yr in decade_start:decade_end) {
    yearly_data <- data %>%
      filter(Year == yr)
    
    # Loop through each pond
    for (pond_name in unique(data$pond_name)) {
      pond_data <- yearly_data %>%
        filter(pond_name == pond_name)
      
      # Loop through each month
      for (month in month.name) {
        monthly_data <- pond_data %>%
          filter(Month == month)
        
        # Calculate the mean of 'med' for the current month
        avg_temp <- mean(monthly_data$med, na.rm = TRUE)
        
        # Append results to monthly_avg_table dataframe
        monthly_avg_table <- rbind(
          monthly_avg_table,
          data.frame(Pond = pond_name, Month = factor(month, levels = month.name), AvgTemp = avg_temp, Year = yr)
        )
      }
    }
  }
  
  return(monthly_avg_table)
}

# Calculate monthly averages for the historical and projected decades
monthly_means <- calculate_monthly_averages(Mod_CRD85, 2012, 2020)
monthly_avg_2030s <- calculate_monthly_averages(Mod_CRD85, 2030, 2039)
monthly_avg_2060s <- calculate_monthly_averages(Mod_CRD85, 2060, 2069)
monthly_avg_2090s <- calculate_monthly_averages(Mod_CRD85, 2090, 2099)

# Combine all datasets into one dataframe and add a Decade column
monthly_means <- monthly_means %>% mutate(Decade = "2020s")
monthly_avg_2030s <- monthly_avg_2030s %>% mutate(Decade = "2030s")
monthly_avg_2060s <- monthly_avg_2060s %>% mutate(Decade = "2060s")
monthly_avg_2090s <- monthly_avg_2090s %>% mutate(Decade = "2090s")

combined_monthly_avg <- bind_rows(monthly_means, monthly_avg_2030s, monthly_avg_2060s, monthly_avg_2090s)
str(combined_monthly_avg)

# Calculate the difference from the reference year (2020s)
reference_year_avg <- combined_monthly_avg %>%
  filter(Decade == "2020s") %>%
  group_by(Month) %>%
  summarise(avg_temp_ref_year = mean(AvgTemp, na.rm = TRUE))

combined_monthly_avg <- combined_monthly_avg %>%
  left_join(reference_year_avg, by = "Month") %>%
  mutate(diff_AvgTemp = AvgTemp - avg_temp_ref_year)

# Filter out the reference decade for further analysis
DeltaMonthly_CRD85 <- combined_monthly_avg %>% filter(Decade != "2020s")

# Group by Month and Decade, calculate mean and standard deviation of differences
summary_stats2 <- DeltaMonthly_CRD85 %>%
  mutate(Month = format(as.Date(paste0(Month, " 1 2000"), "%B %d %Y"), "%m")) %>%  # Standardize month to numeric
  group_by(Month, Decade) %>%
  summarise(
    mean_diff_AvgTemp = mean(diff_AvgTemp, na.rm = TRUE),
    sd_diff_AvgTemp = sd(diff_AvgTemp, na.rm = TRUE),
    .groups = "drop"
  )
summary_stats2

# Function to create plots for each decade
create_plot <- function(data, decade_label, y_label = "Delta Temp (°C)") {
  data <- data %>% mutate(Month = as.numeric(Month))  # Convert Month to numeric for plotting
  
  ggplot(data, aes(x = Month)) +
    geom_point(aes(y = mean_diff_AvgTemp), color = "blue") +
    geom_line(aes(y = mean_diff_AvgTemp), color = "blue") +
    geom_ribbon(aes(ymin = mean_diff_AvgTemp - sd_diff_AvgTemp, 
                    ymax = mean_diff_AvgTemp + sd_diff_AvgTemp), 
                fill = "blue", alpha = 0.3) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    ylim(-5, 15) +
    theme_classic() +
    labs(x = "", y = y_label, title = paste("Monthly Delta Temp:", decade_label)) +
    theme(
      axis.text.x = element_text(angle = 65, hjust = 1),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
}

# Create plots for each decade
MonthlyStats_2030 <- summary_stats2 %>% filter(Decade == "2030s")
MonthlyStats_2060 <- summary_stats2 %>% filter(Decade == "2060s")
MonthlyStats_2090 <- summary_stats2 %>% filter(Decade == "2090s")

MonthlyStats_2030Plot <- create_plot(MonthlyStats_2030, "2030s")
MonthlyStats_2030Plot
MonthlyStats_2060Plot <- create_plot(MonthlyStats_2060, "2060s", y_label = "")
MonthlyStats_2060Plot
MonthlyStats_2090Plot <- create_plot(MonthlyStats_2090, "2090s", y_label = "")
MonthlyStats_2090Plot

# Combine the plots into a single panel
DeltaMonth_CRD85_plot <- ggarrange(
  MonthlyStats_2030Plot, 
  MonthlyStats_2060Plot, 
  MonthlyStats_2090Plot, 
  ncol = 3, 
  labels = c("2030s", "2060s", "2090s")
)
DeltaMonth_CRD85_plot

# Save the combined plot
ggsave("Corr_DeltaMonthCRD85.png", plot = DeltaMonth_CRD85_plot, width = 12, height = 3)

# Save data as CSV files for each decade
write.csv(MonthlyStats_2030, file = "Corr_MonthlyStatsCRD85_2030.csv", row.names = FALSE)
write.csv(MonthlyStats_2060, file = "Corr_MonthlyStatsCRD85_2060.csv", row.names = FALSE)
write.csv(MonthlyStats_2090, file = "Corr_MonthlyStatsCRD85_2090.csv", row.names = FALSE)
