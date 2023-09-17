library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

## Read in the 12 monthly files
rm(list=ls())
# Set the directory path
directory_path <- "./data"

# Read all CSV files in the directory and combine into a single data frame
data_frames <- lapply(list.files(directory_path, pattern = "\\.csv$", full.names = TRUE), read_csv) 
bikerides <- bind_rows(data_frames) |> filter(!is.na(end_station_name)) |>
  select(-c(ride_id,end_station_name,end_station_id,start_station_id))

## Drop Rows with missing start or end station names.
bikerides <- bikerides |> filter(!is.na(start_station_name))
# Extract Start date, start day,start hour

# Assuming you have a data frame called "combined_data" with a "started_at" column
# Convert "started_at" column to POSIXct class
bikerides$started_at <- ymd_hms(bikerides$started_at)
# Extract start date (YYYY-MM-DD)
bikerides$start_date <- as.Date(bikerides$started_at)

# Extract day of the week (Mon, Tue, etc.)
bikerides$day_of_week <- wday(bikerides$started_at, label = TRUE, abbr = TRUE)

# Extract hour of the day
bikerides$hour_of_day <- hour(bikerides$started_at)

# Extract the month name
bikerides$month_name <- month(ymd(bikerides$start_date), label = TRUE, abbr = TRUE)

## Begin preparing data vizs

plot1 <- bikerides |> count(month_name,member_casual) |>
  ggplot() + geom_col(aes(x=month_name,n,fill=member_casual)) +
  labs(title ="Rides by Month",x="Ride Month",y="Number of Rides" ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(plot1)

# Calculate the count and percentage for each bike type
bike_type_counts <- bikerides %>%
  group_by(rideable_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create a pie chart
plot <- plot_ly(bike_type_counts, labels = ~rideable_type, values = ~percentage, type = "pie",
                text = ~paste("Count:", count))

# Customize the pie chart
plot <- plot %>% layout(title = "Bike Type Distribution",
                        showlegend = TRUE)

# Display the plot
plot