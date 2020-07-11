

# This code calculates and graphs a 21-day moving average of daily maximum 
# temperatures for AZMET Bonita station

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
library("lubridate")
library("extrafont")

# Load additional font options for plotting
font_import()
y
loadfonts(device = "postscript")

# Load AZMET station list
stn_list <- read.csv("azmet-station-list.csv", sep = ",")

# Set the AZMET station name and years of interest
stn_name <- "Willcox Bench"
yr_start <- stn_list$start_yr[which(stn_list$stn == stn_name)]
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.daily.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


stn_data <- azmet.daily.data.download(stn_name)

# Retain necessary variables
stn_data <- select(stn_data, Date, Year, Month, Day, JDay, Tmax)

# Convert temperature from Celsius to Fahrenheit
stn_data$Tmax <- (1.8 * stn_data$Tmax) + 32








# MAKE AND SAVE TIMESERIES PLOT --------------------


# To help with setting axis breaks on the figures, create a function that rounds 
# numbers to the nearest specified base
myround <- function(x, base) {
  base * round(x / base)
}

# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year. The graph will start on March 1, so this
# won't be an issue.
cgdds$JDay[which(leap_year(cgdds$Year) == TRUE)] <- 
  cgdds$JDay[which(leap_year(cgdds$Year) == TRUE)] - 1

# Create a ggplot object for graphing cumulative growing degree days
p <- ggplot(data = cgdds) +
  
  # Add CGDD data for all years as individual time series
  geom_line(aes(x = JDay, y = CGDDs, color = GS),
            lineend = "round",
            linetype = "solid",
            size = 1.0) +
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("Heat Accumulation") +
  labs(subtitle = "AZMET Willcox Bench station",
       x = "\nDate",
       y = "Cumulative  Growing  Degree  Days\n",
       caption = paste0(
         "\ndata source: AZMET (cals.arizona.edu/azmet)",
         "\n50°F-based GDD calculation, December 1 start date")) +
  
  # Rename variable description and set the time series line colors
  scale_color_brewer(name = "Growing \nSeason", palette = "Dark2") +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(breaks = c(60, 91, 121, 152, 182, 213),
                     labels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug"),
                     limits = c((60 - 1), (213 + 1)),
                     expand = c(0.005, 0.0)) +
  scale_y_continuous(
    breaks = seq(
      0,
      myround(
        ceiling(
          max(filter(cgdds, JDay <= (213 + 1))$CGDDs, na.rm = TRUE)),
        100),
      by = 500
      ),
    limits = c(0, max(filter(cgdds, JDay <= (213 + 1))$CGDDs, na.rm = TRUE)),
    expand = c(0.03, 0.0)) +
  
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
        legend.text = element_text(color = "gray40", size = 10),
        legend.title = element_text(color = "gray40", size = 10, face = "bold"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.25),
        panel.grid.major.y = element_line(color = "gray80", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(color = "gray40", hjust = 0.0, size = 8),
        plot.caption.position = "plot",
        plot.margin = unit(c(1, 1 ,1, 1), "mm"),
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 12)), 
        plot.title = (
          element_text(face = "bold", family = "Source Serif Pro", size = 16)
          ),
        plot.title.position = "plot")

p

#  Save the figure as a .png file in the current directory
ggsave("./cgdds-azmet-willcox-bench-cvn202007.eps",
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 4, units = "in", dpi = 300)

