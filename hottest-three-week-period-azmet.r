

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
library("gghighlight")
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
yr_start <- (stn_list$start_yr[which(stn_list$stn == stn_name)])
yr_end <- stn_list$end_yr[which(stn_list$stn == stn_name)]

# Load function to download and transform daily AZMET data
source("azmet.daily.data.download.R")


# DOWNLOAD AND TRANSFORM DAILY AZMET DATA --------------------


stn_data <- azmet.daily.data.download(stn_name)

# Retain necessary variables
stn_data <- select(stn_data, Date, Year, Month, Day, JDay, Tmax)

# For Willcox Bench case
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
stn_data$Tmax_mavg <- as.numeric(stn_data$Tmax_mavg)


# MAKE AND SAVE TIMESERIES PLOT --------------------


# To facilitate graphing interannual data that includes leap years, subtract 1
# from 'JDay' values in a leap year
stn_data$JDay[which(leap_year(stn_data$Year) == TRUE)] <- 
  stn_data$JDay[which(leap_year(stn_data$Year) == TRUE)] - 1

# For text annotation
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
stn_data["Month_name"] <- NA
stn_data$Month_name <- months[stn_data$Month]

# Calculate average maximum temperature by calendar day and moving average of 
# daily average values for the station period of record
stn_data_avg <- as.data.frame(seq(from = 1, to = 365))
colnames(stn_data_avg) <- "JDay"
stn_data_avg["Tmax_davg"] <- NA
stn_data_avg["Tmax_mavgdavg"] <- NA

for (cal_day in min(stn_data_avg$JDay):max(stn_data_avg$JDay)) {
  stn_data_avg$Tmax_davg[cal_day] <-  
    mean(stn_data$Tmax[which(stn_data$JDay == cal_day)], na.rm = TRUE)
}
rm(cal_day)

stn_data_avg$Tmax_mavgdavg <- stats::filter(
  x = stn_data_avg$Tmax_davg, 
  filter = rep(1, filter_length) / filter_length,
  method = "convolution",
  sides = 2,
  circular = FALSE)
stn_data_avg$Tmax_mavgdavg <- as.numeric(stn_data_avg$Tmax_mavgdavg)

# For text annotation
stn_data_avg["Month_name"] <- NA
stn_data_avg["Day"] <- NA
for (entry in 1:length(stn_data_avg$Month_name)) {
  stn_data_avg$Month_name[entry] <- 
    stn_data$Month_name[which(stn_data$JDay == entry)[1]]
  stn_data_avg$Day[entry] <- 
    min(stn_data$Day[which(stn_data$JDay == entry)], na.rm = TRUE)
}
rm(entry)

# Create a ggplot object for graphing daily maximum temperature values
p <- ggplot() +
  
  # 21-DAY MOVING AVERAGE SHADED REGION ----------
  
  # Background shading for warmest moving average value and its range
  geom_rect(aes(
    xmin = stn_data_avg$JDay[which(
      stn_data_avg$Tmax_mavgdavg == max(
        stn_data_avg$Tmax_mavgdavg, na.rm = TRUE
      ))][1] - 10,
    xmax = stn_data_avg$JDay[which(
      stn_data_avg$Tmax_mavgdavg == max(
        stn_data_avg$Tmax_mavgdavg, na.rm = TRUE
      ))][1] + 10,
    ymin = min(filter(stn_data, 
                      JDay >= 134 & JDay <= 197)$Tmax, na.rm = TRUE) - 1,
    ymax = max(filter(stn_data, 
                      JDay >= 134 & JDay <= 197)$Tmax, na.rm = TRUE) + 1
      ),
    fill = "gray40", alpha = 0.2, show.legend = FALSE) +
  
  # Label shaded region
  geom_text(aes(
    x = stn_data_avg$JDay[which(
      stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg, 
                                        na.rm = TRUE)
      )],
    y = min(filter(stn_data, JDay >= 134 & JDay <= 197)$Tmax, 
            na.rm = TRUE) + 9,
    label = "Average  Warmest  \n21-day  Period",
    family = "Source Sans Pro", fontface = "bold", 
    hjust = "center", vjust = "bottom"
    ),
    size = 10 / .pt, color = "gray40", show.legend = FALSE) +
  
  # Label start and end dates of shaded region
  geom_text(aes(
    x = c(
      # Start date
      stn_data_avg$JDay[which(
        stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg,
                                          na.rm = TRUE)
        )] - 7.5,
      # End date
      stn_data_avg$JDay[which(
        stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg, 
                                          na.rm = TRUE)
      )] + 8
      ),
    y = min(filter(stn_data, JDay >= 134 & JDay <= 197)$Tmax, na.rm = TRUE) + 5,
    label = c(
      paste(
        # Start date
        stn_data_avg$Month_name[which(
          stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg,
                                            na.rm = TRUE)
        ) - 10],
        stn_data_avg$Day[which(
          stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg,
                                            na.rm = TRUE)
        ) - 10],
        sep = " "
      ),
      paste(
        # End date
        stn_data_avg$Month_name[which(
          stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg,
                                            na.rm = TRUE)
        ) + 10],
        stn_data_avg$Day[which(
          stn_data_avg$Tmax_mavgdavg == max(stn_data_avg$Tmax_mavgdavg,
                                            na.rm = TRUE)
        ) + 10],
        sep = " ")), 
    family = "Source Sans Pro", fontface = "bold", 
    hjust = "center", vjust = "bottom"
    ),
    size = 10 / .pt, color = "gray40", show.legend = FALSE) +
  
  # DAILY VALUES AS POINTS ----------
  
  # Add Tmax daily values
  geom_point(data = stn_data,
             mapping = aes(x = JDay, y = Tmax, shape = "circle"),
             alpha = 0.5,
             color = "gray50",
             size = 3,
             stroke = 0.1) +
             #show.legend = TRUE) +
  
  geom_point(data = filter(stn_data, Year == 2020),
             mapping = aes(x = JDay, y = Tmax, shape = "circle"),
             alpha = 0.5,
             color = "orange",
             size = 3,
             stroke = 0.1) +
             #show.legend = TRUE) +
  
  # Add Tmax daily average values
  geom_line(data = stn_data_avg, 
            mapping = aes(x = JDay, y = Tmax_davg, linetype = "solid"),
            color = "gray30",
            lineend = "round",
            size = 1.5) +
            #show.legend = TRUE) +
  
  guides(shape=guide_legend("Daily Values", order = 1),
         linetype=guide_legend("Daily Average", order = 2)) +
  
  scale_shape_manual(values = "circle", labels = "") +
  scale_linetype_manual(values = "solid", labels = "") +
  
  # Add the title, subtitle, axis labels, and caption
  ggtitle("Daily Maximum Temperature") +
  labs(subtitle = "AZMET Willcox Bench station, 2017-2020",
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
      min(filter(stn_data, JDay >= 134 & JDay <= 197)$Tmax, na.rm = TRUE) - 1,
      max(filter(stn_data, JDay >= 134 & JDay <= 197)$Tmax, na.rm = TRUE) + 1),
    expand = c(0.0, 0.0)
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
        legend.spacing = unit(1.0, 'mm'),
        legend.text = element_blank(),
        legend.title = element_text(color = "gray40", size = 10),
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

