

# This code calculates and graphs a 21-day moving average of daily maximum 
# temperatures for AZMET Willcox Bench station for the May 15-July 15 period of
# interest

# AZMET data are at: https://cals.arizona.edu/azmet/

# Author:
# Jeremy Weiss, Climate and Geospatial Extension Scientist
# School of Natural Resources and the Environment
# University of Arizona
# 520-626-8063, jlweiss@email.arizona.edu


# SETUP --------------------


# Load needed libraries
library("dplyr")
library("ggplot2")
library("gghighlight")
library("lubridate")
library("extrafont")
library("viridis")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station name and years of interest
stn_name <- "Willcox Bench"
yr_start <- (stn_list$start_yr[which(stn_list$stn == stn_name)])
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.daily.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


stn_data <- azmet.daily.data.download(stn_name)

# Retain necessary variables
stn_data <- select(stn_data, Date, Year, Month, Day, JDay, Tmax)

# For text annotation in plot
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
stn_data["Month_name"] <- NA
stn_data$Month_name <- months[stn_data$Month]
stn_data <- cbind(stn_data[1:3], stn_data[7], stn_data[4:6])

# For Willcox Bench case, in which data collection starts on June 18
stn_data <- filter(stn_data, Year != yr_start)

# Convert temperature from Celsius to Fahrenheit
stn_data$Tmax <- (1.8 * stn_data$Tmax) + 32

# Calculate moving average values
# http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
# https://vandomed.github.io/moving_averages.html
filter_length <- 21
stn_data["Tmax_mavg"] <- NA
stn_data$Tmax_mavg <- stats::filter(
  x = stn_data$Tmax, 
  filter = rep(1, filter_length) / filter_length,
  method = "convolution",
  sides = 2,
  circular = FALSE)

# Calculate average maximum temperature by Julian day and moving average of 
# these daily average values for the station period of record
stn_data["Tmax_JDay_avg"] <- NA
for (jd in min(stn_data$JDay):max(stn_data$JDay)) {
  df <- filter(stn_data, JDay == jd)
  stn_data$Tmax_JDay_avg[which(stn_data$JDay == jd)] <- mean(df$Tmax)
}
rm(jd, df)

stn_data["Tmax_JDay_mavg"] <- NA
for (yr in min(stn_data$Year):max(stn_data$Year)) {
  df <- filter(stn_data, Year == yr)
  df$Tmax_JDay_mavg <- stats::filter(
    x = df$Tmax_JDay_avg, 
    filter = rep(1, filter_length) / filter_length,
    method = "convolution",
    sides = 2,
    circular = FALSE
    )
  
  stn_data$Tmax_JDay_mavg[which(stn_data$Year == yr)] <- df$Tmax_JDay_mavg
}
rm(yr, df)


# PREP FOR TIMESERIES PLOT --------------------


stn_data_plot <- filter(stn_data, Month >= 4 & Month <= 8)

# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year. This works here, since we are not using 
# data from January nor February
stn_data_plot$JDay[which(leap_year(stn_data_plot$Year) == TRUE)] <- 
  stn_data_plot$JDay[which(leap_year(stn_data_plot$Year) == TRUE)] - 1

# Check for multiple occurrences of the maximum value of the moving average
# time series for each year. Plot code below assumes there is only one
# occurrence of the maximum value
for (yr in min(stn_data_plot$Year):max(stn_data_plot$Year)) {
  df <- filter(stn_data_plot, Year == yr)
  print(yr)
  print(head(sort(df$Tmax_mavg, decreasing = TRUE)))
  print(head(sort(df$Tmax_JDay_mavg, decreasing = TRUE)))
  flush.console()
}
rm(yr, df)

# Create a reference table for start and end Julian days of 21-day period 
# centered on maximum 21-day moving average values for individual years
mavg_window_yr <- 
  data.frame(Year = min(stn_data_plot$Year):max(stn_data_plot$Year))
mavg_window_yr["Month_start"] <- NA
mavg_window_yr["Month_name_start"] <- NA
mavg_window_yr["Day_start"] <- NA
mavg_window_yr["Month_end"] <- NA
mavg_window_yr["Month_name_end"] <- NA
mavg_window_yr["Day_end"] <- NA
mavg_window_yr["Start"] <- NA
mavg_window_yr["End"] <- NA

for (yr in min(stn_data_plot$Year):max(stn_data_plot$Year)) {
  # May 15 - July 15
  df <- filter(stn_data_plot, Year == yr & JDay >= 135 & JDay <= 196)
  
  mavg_window_yr$Start[which(mavg_window_yr$Year == yr)] <-
    df$JDay[which(df$Tmax_mavg == max(df$Tmax_mavg))] - 10
  mavg_window_yr$End[which(mavg_window_yr$Year == yr)] <- 
    df$JDay[which(df$Tmax_mavg == max(df$Tmax_mavg))] + 10
  
  # Some 21-day ranges may be outside of the May 15 - July 15 window
  mavg_window_yr$Month_start[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Month[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) - 10
      ]
  mavg_window_yr$Month_name_start[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Month_name[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) - 10
      ]
  mavg_window_yr$Day_start[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Day[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) - 10
      ]
  mavg_window_yr$Month_end[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Month[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) + 10
      ]
  mavg_window_yr$Month_name_end[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Month_name[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) + 10
      ]
  mavg_window_yr$Day_end[which(mavg_window_yr$Year == yr)] <- 
    stn_data_plot$Day[
      which(stn_data_plot$Year == yr &
              stn_data_plot$Tmax_mavg == max(df$Tmax_mavg)) + 10
      ]
  
}
rm(yr, df)

# Create a reference table for start and end Julian days of 21-day period 
# centered on maximum 21-day moving average values for overall station record

# May 15 - July 15
df <- filter(stn_data_plot, JDay >= 135 & JDay <= 196)
mavg_window_avg <- data.frame(
  Month_start = df$Month[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] - 10
    ],
  Month_name_start = df$Month_name[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] - 10
    ],
  Month_day_start = df$Day[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] - 10
    ],
  Month_end = df$Month[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] + 10
    ],
  Month_name_end = df$Month_name[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] + 10
    ],
  Month_day_end = df$Day[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1] + 10
    ],
  Start = df$JDay[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1]
    ] - 10,
  End = df$JDay[
    which(df$Tmax_JDay_mavg == max(df$Tmax_JDay_mavg, na.rm = TRUE))[1]
    ] + 10
)
rm(df)

# Create a reference table to depict the analysis period
analysis_window <- 
  data.frame(x_min = 135, x_max = 196, y_min = min(stn_data_plot$Tmax), 
             y_max = max(stn_data_plot$Tmax))


# MAKE AND SAVE FOR TIMESERIES PLOT --------------------


p <- ggplot(data = stn_data_plot) +
  
  # Add annotation to mark May 15 - July 15 period of interest, as 'geom_rect'
  # in order to place behind other data (annotations go on top)
  geom_rect(data = analysis_window, 
            mapping = 
              aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = (y_max + 1)),
            alpha = 0.33, color = NA, fill = "gray80") +
  
  geom_segment(
    mapping = aes(x = (135 + 1), xend = (196 - 1), y = 62.5, yend = 62.5),
    arrow = arrow(angle = 45, length = unit(1.5, "mm"), ends = "both", 
                  type = "open"),
    color = "gray70", size = 0.35
  ) +
  
  geom_text(mapping = aes(x = ((135 + 196) / 2), y = (62.5 + 1)),
            label = "period of interest", family = "Source Sans Pro", 
            fontface = "plain", hjust = "center", vjust = "bottom", 
            size = 7 / .pt, color = "gray70", angle = 0) +
  
  # Add Tmax 21-day moving average
  geom_line(mapping = aes(x = JDay, y = Tmax_mavg, color = factor(Year))) +
  
  # Add Tmax daily values
  geom_point(mapping = aes(x = JDay, y = Tmax, color = factor(Year)), 
             alpha = 0.5) +
  
  # Add moving average windows for individual years
  geom_segment(data = mavg_window_yr,
               mapping = aes(x = Start, xend = End, y = 77.5, yend = 77.5),
               color = viridis(n = 4, alpha = 1.0, begin = 0.0, end = 0.75, 
                               direction = -1),
               size = 3) +
  
  geom_text(data = mavg_window_yr, mapping = aes(x = (Start - 1), y = 77.5),
            label = paste(mavg_window_yr$Month_name_start, 
                          mavg_window_yr$Day_start, sep = " "), 
            family = "Source Sans Pro", fontface = "bold", hjust = "right", 
            vjust = "center", size = 8 / .pt, angle = 0,
            color = viridis(n = 4, alpha = 1.0, begin = 0.0, end = 0.75,
                            direction = -1)) +
  
  geom_text(data = mavg_window_yr, mapping = aes(x = (End + 1), y = 77.5),
            label = paste(mavg_window_yr$Month_name_end, 
                          mavg_window_yr$Day_end, sep = " "), 
            family = "Source Sans Pro", fontface = "bold", hjust = "left", 
            vjust = "center", size = 8 / .pt, angle = 0,
            color = viridis(n = 4, alpha = 1.0, begin = 0.0, end = 0.75, 
                            direction = -1)) +
  
  # Add moving average window based on daily mean maximum temperatures
  annotate(geom = "segment", 
           x = mavg_window_avg$Start, xend = mavg_window_avg$End, 
           y = 72.5, yend = 72.5, alpha = 1.0, color = "gray50", size = 3) +
  
  annotate(geom = "text", x = (mavg_window_avg$Start - 1), y = 72.5, 
           label = paste(mavg_window_avg$Month_name_start, 
                         mavg_window_avg$Month_day_start, sep = " "), 
           family = "Source Sans Pro", fontface = "bold", 
           hjust = "right", vjust = "center", 
           size = 8 / .pt, color = "gray50", angle = 0) +
  
  annotate(geom = "text", x = (mavg_window_avg$End + 1), y = 72.5, 
           label = paste(mavg_window_avg$Month_name_end, 
                         mavg_window_avg$Month_day_end, sep = " "), 
           family = "Source Sans Pro", fontface = "bold", 
           hjust = "left", vjust = "center", 
           size = 8 / .pt, color = "gray50", angle = 0) +
  
  # Add symbology for individual years
  gghighlight() +
  scale_color_viridis_d(alpha = 1.0, begin = 0.0, end = 0.75, direction = -1) +
  
  facet_wrap(~ factor(Year, levels = sort(unique(stn_data_plot$Year), 
                                          decreasing = TRUE)), 
             ncol = 1) +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = c(121, 135, 152, 166, 182, 196, 213),
    labels = c("May 1", "May 15", "Jun 1", "Jun 15", "Jul 1", "Jul 15", "Aug 1"),
    limits = c(119, 216),
    expand = c(0.0, 0.0)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = (max(stn_data_plot$Tmax, na.rm = TRUE) + 1), 
                 by = 10),
    limits = c(
      (min(filter(stn_data, JDay >= 119 & JDay <= 215)$Tmax, na.rm = TRUE) - 3),
      (max(filter(stn_data, JDay >= 119 & JDay <= 215)$Tmax, na.rm = TRUE) + 2)),
    expand = c(0.0, 0.0)
  ) +
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("Warmest 21-Day Period of Maximum Temperature") +
  labs(subtitle = "AZMET Willcox Bench station, 2017-2020",
       x = "\nDate",
       y = "°F\n",
       caption = "\ndata source: AZMET (cals.arizona.edu/azmet)") +
  
  # Further customize the figure appearance
  theme_light(base_family = "Source Sans Pro") +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "gray40", size = 10),
        axis.text.y = element_text(color = "gray40", size = 10),
        axis.ticks.x.bottom = element_line(color = "gray80", size = 0.25),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.0, "mm"),
        axis.ticks.length.y = unit(0.0, "mm"),
        axis.title.x = element_text(color = "gray40", size = 10),
        axis.title.y = element_text(color = "gray40", size = 10),
        legend.direction = "vertical",
        legend.spacing = unit(1.0, 'mm'),
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 8),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 11)), 
        plot.title = (
          element_text(face = "bold", family = "Source Serif Pro", size = 14)
        ),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "gray40", size = 10, face = "bold")
  )

p

#  Save the figure as a .png file in the current directory
ggsave(file = paste0("hottest-three-week-period-azmet-willcox-bench-",
                     Sys.Date(),
                     ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 5, height = 7, units = "in", dpi = 300)
  
  
# FIN --------------------

