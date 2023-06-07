library(tidyverse)
library(vioplot)

# loading data
people_moving_df <- read.csv("Public_Life.csv")

locations_df <- read.csv("locations.csv")

unique(locations_df$location_character)

# isolating important rows
small_loc_df <- data.frame(locations_df$location_id, locations_df$location_line_typology_vehicular)

small_move_df <- data.frame(people_moving_df$unique_moving_id,  people_moving_df$moving_row_total, people_moving_df$moving_time_start, people_moving_df$moving_time_end, people_moving_df$location_id)

# changing column names
colnames(small_loc_df) <- c("location", "typology")

colnames(small_move_df) <- c("id", "total_count", "start", "end", "location")

# combining dataframes
new_df <- left_join(small_loc_df, small_move_df, 
                    by = join_by(location), 
                    multiple = "all")

# creating column to categorize neighborhood types

unique(new_df$typology)

new_df$classification <- ifelse(new_df$typology %in% c("Neighborhood Corridor", "Urban Village Neighborhood Access", "Urban Village Neighborhood", "Downtown Neighborhood", "Downtown", "Downtown Neighborhood Access", "Urban Center Connector", "Urban Village Main"), 1, 0)

# creating column for count per unit of time

new_df$start <- as.POSIXlt(new_df$start, format="%d/%m/%Y %I:%M:%S %p", tz = "UTC", na.rm = T)

new_df$end <- as.POSIXlt(new_df$end, format="%d/%m/%Y %I:%M:%S %p", tz = "UTC", na.rm = T)

new_df$duration <- as.numeric((new_df$end - new_df$start) / 60)

new_df$count_per_minute <- new_df$total_count / new_df$duration

# making a boxplot

new_df$land_use <- ifelse(new_df$classification == 1, "Mixed Use", "Single Use")

boxplot <- ggplot(new_df, aes(x = land_use, 
                              y = total_count,
                              fill = land_use), na.rm = TRUE) + 
  geom_boxplot() +
  xlab("Land Use Diversity") +
  ylab ("Number of People Moving") +
  theme(legend.position = "none")
boxplot

# making a violin plot

vioplot(new_df, 
        aes(x = ,
            y = total_count,
            fill = land_use), na.rm = TRUE)


# making a scatterplot 
# plot(new_df$land_use, df$total_count, na.rm = T,
#     xlab = "Land Use",
#    ylab = "Pedestrians Through the Space",
#    col = ifelse(new_df$land_use == 1, "green", "purple"))

# conducting main regression

model <- lm(total_count ~ classification, data = new_df)

summary(model)

plot(model)