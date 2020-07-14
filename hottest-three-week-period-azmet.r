

# This code calculates and graphs a 21-day moving average of daily maximum 
# temperatures for AZMET Willcox Bench station

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

# Calculate moving average values
filter_length <- 21
stn_data["Tmax_mavg"] <- stats::filter(
  x = stn_data$Tmax, 
  filter = rep(1, filter_length) / filter_length,
  method = "convolution",
  sides = 2,
  circular = FALSE)
stn_data$Tmax_mavg <- as.numeric(stn_data$Tmax_mavg)


# MAKE AND SAVE TIMESERIES PLOT --------------------


# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year. The graph will start on May 15, so this
# won't be an issue.
stn_data$JDay[which(leap_year(stn_data$Year) == TRUE)] <- 
  stn_data$JDay[which(leap_year(stn_data$Year) == TRUE)] - 1

# Calculate average maximum temperature by calendar day
# http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
# https://vandomed.github.io/moving_averages.html
stn_data["Tmax_davg"] <- NA
df <- as.data.frame(stn_data$JDay)
colnames(df) <- "JDay" 
df["Tmax_davg"] <- NA 
for (cal_day in min(stn_data$JDay):max(stn_data$JDay)) {
  stn_data$Tmax_davg[which(stn_data$JDay == cal_day)] <-  
    mean(stn_data$Tmax[which(stn_data$JDay == cal_day)], na.rm = TRUE)
  df$Tmax_davg[which(df$JDay == cal_day)] <-  
    mean(stn_data$Tmax[which(stn_data$JDay == cal_day)], na.rm = TRUE)
}
rm(cal_day)

# Calculate moving average values of daily average values
df["Tmax_mavgdavg"] <- stats::filter(
  x = df$Tmax_davg, 
  filter = rep(1, filter_length) / filter_length,
  method = "convolution",
  sides = 2,
  circular = FALSE)
df$Tmax_mavgdavg <- as.numeric(df$Tmax_mavgdavg)

stn_data["Tmax_mavgdavg"] <- NA
for (cal_day in min(stn_data$JDay):max(stn_data$JDay)) {
  stn_data$Tmax_mavgdavg[which(stn_data$JDay == cal_day)] <-  
    df$Tmax_mavgdavg[which(df$JDay == cal_day)]
}
rm(cal_day)
rm(df)

# Create a ggplot object for graphing cumulative growing degree days
p <- ggplot(data = stn_data) +
  
  #  Range background shading for veraison
  annotate(
    "rect",
    xmin = stn_data$JDay[which(
      stn_data$Tmax_mavgdavg == max(
        stn_data$Tmax_mavgdavg, na.rm = TRUE))][1] - 10,
    xmax = stn_data$JDay[which(
      stn_data$Tmax_mavgdavg == max(
        stn_data$Tmax_mavgdavg, na.rm = TRUE))][1] + 10,
    ymin = min(filter(stn_data, Month >= 5 & Month <= 7)$Tmax, na.rm = TRUE),
    ymax = max(filter(stn_data, Month >= 5 & Month <= 7)$Tmax, na.rm = TRUE),
    alpha=0.2,
    fill = "gray50") +

  # Add Tmax daily values
  geom_point(mapping = aes(x = JDay, y = Tmax),
             alpha = 0.5,
             color = "gray50",
             size = 3,
             show.legend = TRUE) +

  # Add Tmax daily average values
  geom_line(aes(x = JDay, y = Tmax_davg),
            color = "gray30",
            lineend = "round",
            linetype = "solid",
            size = 1.5,
            show.legend = TRUE) +
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("Daily Maximum Temperature") +
  labs(subtitle = "AZMET Willcox Bench station",
       x = "\nDate",
       y = "°F\n",
       caption = "\ndata source: AZMET (cals.arizona.edu/azmet)") +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = c(121, 135, 152, 166, 182, 196),
    labels = c("May 1", "May 15", "Jun 1", "Jun 15", "Jul 1", "Jul 15"),
    limits = c(134, 197),
    expand = c(0.01, 0.01)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = max(stn_data$Tmax, na.rm = TRUE), by = 5),
    limits = c(
      min(filter(stn_data, Month >= 5 & Month <= 7)$Tmax, na.rm = TRUE),
      max(filter(stn_data, Month >= 5 & Month <= 7)$Tmax, na.rm = TRUE)),
    expand = c(0.06, 0.0)
    ) +
  
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
ggsave("./hottest-three-week-period-azmet-willcox-bench.eps",
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 4, units = "in", dpi = 300)
