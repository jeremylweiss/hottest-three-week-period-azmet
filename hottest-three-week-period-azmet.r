

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


# MAKE AND SAVE TIMESERIES PLOT --------------------


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




# Create reference tables for start and end Julian days of 21-day period 
# centered on maximum 21-day moving average values for individual years and 
# for overall station record
mavg_window_yr <- as.data.frame(min(stn_data_plot$Year):max(stn_data_plot$Year))
colnames(mavg_window_yr) <- "Year"
mavg_window_yr["Start"] <- NA
mavg_window_yr["End"] <- NA
#stn_data_plot["MA_Start"] <- NA
#stn_data_plot["MA_End"] <- NA

for (yr in min(stn_data_plot$Year):max(stn_data_plot$Year)) {
  # May 15 - July 15
  df <- filter(stn_data_plot, Year == yr & JDay >= 135 & JDay <= 196)
  
  ma_start <- df$JDay[which(df$Tmax_mavg == max(df$Tmax_mavg))] - 10
  ma_end <- df$JDay[which(df$Tmax_mavg == max(df$Tmax_mavg))] + 10
  
  mavg_window_yr$Start[which(mavg_window_yr$Year == yr)] <- ma_start
  mavg_window_yr$End[which(mavg_window_yr$Year == yr)] <- ma_end
}
rm(yr, df, ma_start, ma_end)

# May 15 - July 15
df <- filter(stn_data_plot, JDay >= 135 & JDay <= 196)
mavg_window_avg <- data.frame(
  Start = df$JDay[
    which(df$Tmax_JDay_mavg == 
            max(df$Tmax_JDay_mavg, na.rm = TRUE))[1]
    ] - 10,
  End = df$JDay[
    which(df$Tmax_JDay_mavg == 
            max(df$Tmax_JDay_mavg, na.rm = TRUE))[1]
    ] + 10
)
rm(df)


# The plot
p <- ggplot(data = stn_data_plot) +
  
  # Add annotation to mark May 15 - July 15 period of interest
  #geom_vline(xintercept = c(135, 196), color = "gray40", size = 0.5) +
  annotate(geom = "rect", xmin = 135, xmax = 196,
           ymin = min(stn_data_plot$Tmax), ymax = max(stn_data_plot$Tmax), 
           alpha = 0.25, color = NA, fill = "gray80") +
  
  geom_segment(
    mapping = aes(x = (135 + 1), xend = (196 - 1), y = 62.5, yend = 62.5),
    arrow = arrow(
      angle = 45, length = unit(1.5, "mm"), ends = "both", type = "closed"
    ),
    color = "gray40", size = 0.5
  ) +
  
  geom_text(mapping = aes(x = ((135 + 196) / 2), y = (62.5 + 1)),
            label = "period of interest", family = "Source Sans Pro", 
            fontface = "plain", hjust = "center", vjust = "bottom", 
            size = 6 / .pt, color = "gray40", angle = 0) +
  
  # Add moving average windows for individual years
  geom_segment(data = mavg_window_yr,
               mapping = aes(x = Start, xend = End, y = 77.5, yend = 77.5),
               color = viridis(n = 4, alpha = 1.0, begin = 0.0, end = 0.8),
               size = 3) +
  
  # Add Tmax daily values
  geom_point(mapping = aes(x = JDay, y = Tmax, color = factor(Year))) +
  
  # Add Tmax 21-day moving average
  geom_line(mapping = aes(x = JDay, y = Tmax_mavg, color = factor(Year))) +
  
  # Add symbology for individual years
  gghighlight(unhighlighted_params = aes(color = "gray70")) +
  scale_color_viridis_d(alpha = 1.0, begin = 0.0, end = 0.8) +
  
  facet_wrap(~ factor(Year, levels = sort(unique(stn_data_plot$Year), 
                                          decreasing = TRUE)), 
             ncol = 1) +
  
  # Add moving average window based on daily mean maximum temperatures
  annotate(geom = "segment", 
           x = mavg_window_avg$Start, xend = mavg_window_avg$End, 
           y = 72.5, yend = 72.5, alpha = 1.0, color = "gray60", size = 3) +
  
  annotate(geom = "text", x = (mavg_window_avg$Start - 1), y = 72.5, 
           label = paste(month(6), "day", sep = " "), 
           family = "Source Sans Pro", fontface = "plain", 
           hjust = "right", vjust = "center", 
           size = 6 / .pt, color = "gray60", angle = 0) +
  
  annotate(geom = "text", x = (mavg_window_avg$End + 1), y = 72.5, 
           label = "end date", family = "Source Sans Pro", fontface = "plain", 
           hjust = "left", vjust = "center", 
           size = 6 / .pt, color = "gray60", angle = 0) +
  
  # Specify axis breaks, gridlines, and limits
  scale_x_continuous(
    breaks = c(121, 135, 152, 166, 182, 196, 213),
    labels = c("May 1", "May 15", "Jun 1", "Jun 15", "Jul 1", "Jul 15", "Aug 1"),
    limits = c(119, 215),
    expand = c(0.0, 0.0)
  ) +
  
  scale_y_continuous(
    breaks = seq(from = 0, to = max(stn_data_plot$Tmax, na.rm = TRUE), by = 10),
    limits = c(
      (min(filter(stn_data, JDay >= 119 & JDay <= 215)$Tmax, na.rm = TRUE) - 3),
      (max(filter(stn_data, JDay >= 119 & JDay <= 215)$Tmax, na.rm = TRUE) + 1)),
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
        plot.subtitle = (element_text(family = "Source Serif Pro", size = 12)), 
        plot.title = (
          element_text(face = "bold", family = "Source Serif Pro", size = 16)
        ),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(color = "gray40", size = 9, face = "bold")
  )

p

#  Save the figure as a .png file in the current directory
ggsave(file = paste("hottest-three-week-period-azmet-willcox-bench-",
                    Sys.Date(),
                    ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 4, units = "in", dpi = 300)
  
  
  







# FIN --------------------





# For text annotation
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
stn_data["Month_name"] <- NA
stn_data$Month_name <- months[stn_data$Month]

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
ggsave(file = paste("hottest-three-week-period-azmet-willcox-bench-",
                    Sys.Date(),
                    ".eps"),
       plot = p, device = cairo_pdf, path = NULL, scale = 1,
       width = 6, height = 4, units = "in", dpi = 300)

