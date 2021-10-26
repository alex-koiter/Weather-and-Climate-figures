library(tidyverse)
library(lubridate)
library(weathercan)
library(patchwork)

# Find station_id and climate_id
stations_search(name = "brandon")

# Download daily weather data for 2020
df_day <- weather_dl(station_ids = 49909, interval = "day", start = "2020-01-01", end = "2020-12-31")  %>%
  mutate(title = '2020 Daily')

# download hourly weather data
df_hour  <- weather_dl(station_ids = 49909, interval = "hour", start = "2020-09-18", end = "2020-09-28")

# Download daily climate normals
df_normal <- normals_dl(climate_ids = 5010480) %>%
  unnest(normals) %>%
  filter(period != "Year") %>%
  select(period, temp_daily_average, precip) %>%
  mutate(date = mdy(paste0(period, "-15-2020")))

# Calculate 2020 monthly precip totals
df_month <- df_day %>%
  group_by(month) %>%
  summarise(precip = sum(total_precip, na.rm = TRUE)) %>%
  mutate(date = mdy(paste0(month, "-15-2020")))

# Plot climate normals
p1 <- ggplot() +
  theme_bw() +
  geom_segment(data = df_normal, aes(x = date, y = precip/3 - 30, xend = date, yend = -30), size = 8, colour =  gray(0.5)) +
  stat_smooth(data = df_normal, aes(x = date, y = temp_daily_average), method = "loess", se = FALSE, size = 1, colour =  gray(0.5)) +
  scale_y_continuous(name = expression("Temperature " ( degree*C)), 
                     sec.axis = sec_axis(~ (. + 30) * 3 , name = "Precipitation (mm)"),
                     limits = c(-30, 30),
                     expand = c(0, 0)) + 
  scale_x_date(date_labels = "%b", 
               date_breaks = "1 month", 
               expand = c(0.01,0.01), 
               name = "",
               limits = (c(as_date("2020-01-01"), as_date("2020-12-31")))) +
  annotate(geom="text", x = as_date("2020-03-15"), y = 25, label = "1981-2010 Climate Normals",
           color="black") 

# plot 2020 weather data and indicate sampling period
p2 <- ggplot() + 
  theme_bw() +
  geom_rect(aes(xmin = as_date("2020-09-25"), xmax = as_date("2020-09-27"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = "red") +
  geom_point(data = df_day, aes(x = date, y = mean_temp)) + 
  geom_point(data = df_day, aes(x = date, y = mean_temp), colour = "skyblue4", size = 2,  shape = 21, fill = "white") + 
  stat_smooth(data = df_day, aes(x = date, y = mean_temp), method = "loess", se = FALSE, colour = "skyblue4", size = 1) +
  geom_segment(data = df_day, aes(x = date, y = total_precip/3 -30, xend = date, yend = -30), stat = "identity", colour = "skyblue4") +
  scale_y_continuous(name = expression("Temperature " ( degree*C)), 
                     sec.axis = sec_axis(~ (. + 30) * 3 , name = "Precipitation (mm)"),
                     limits = c(-30, 30),
                     expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", 
               date_breaks = "1 month", 
               expand = c(0.01,0.01), 
               name = "Date",
               limits = (c(as_date("2020-01-01"), as_date("2020-12-31")))) +
  annotate(geom="text", x = as_date("2020-09-15"), y = -15, label="Sampling",
           color="black",
           angle = 90) +
  annotate(geom="text", x = as_date("2020-02-01"), y = 25, label="2020 Daily",
           color="black") 

# Combine weather and climate figures using patchwork
combo_p <- p1 / p2
combo_p

#ggsave(filename = "daily_and_normals_brandon_2020.png", plot = combo_p, height = 175, width = 150, units = "mm", dpi = 600)

# Plot weather and climate on the same figure
p3 <- ggplot() + 
  theme_bw() +
  geom_rect(aes(xmin = as_date("2020-09-25"), xmax = as_date("2020-09-27"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = "red") +
  geom_segment(data = df_normal, aes(x = date, y = precip/3 - 30, xend = date, yend = -30), size = 8, colour = gray(0.5)) +
  geom_segment(data = df_month, aes(x = date, y = precip/3 -30, xend = date, yend = -30), stat = "identity", colour = "skyblue4", size = 4) +
  stat_smooth(data = df_normal, aes(x = date, y = temp_daily_average),method = "loess", se = FALSE, size = 1, colour =  gray(0.5)) +
  geom_point(data = df_day, aes(x = date, y = mean_temp), colour = "skyblue4", size = 2,  shape = 21, fill = "white") + 
  stat_smooth(data = df_day, aes(x = date, y = mean_temp),method = "loess", se = FALSE, colour = "skyblue4", size = 1) +
  scale_y_continuous(name = expression("Temperature " ( degree*C)), 
                     sec.axis = sec_axis(~ (. + 30) * 3 , name = "Precipitation (mm)"),
                     limits = c(-30, 40),
                     expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", 
               date_breaks = "1 month", 
               expand = c(0.01,0.01), 
               name = "Date",
               limits = (c(as_date("2020-01-01"), as_date("2020-12-31")))) +
  annotate(geom="text", x = as_date("2020-09-15"), y = -5, label="Sampling",
           color="black",
           angle = 90) +
  annotate(geom="text", x = as_date("2020-02-01"), y = 30, label="2020 Daily",
           color="skyblue4") +
  annotate(geom="text", x = as_date("2020-03-15"), y = 35, label="1981-2010 Climate Normals",
           color= gray(0.5)) 

p3

#ggsave(filename = "daily_normals_brandon_2020.png", plot = p3, height = 100, width = 150, units = "mm", dpi = 600)

p4 <- ggplot() + 
  theme_bw() +
  geom_rect(aes(xmin = as_datetime("2020-09-25 13:00:00"), xmax = as_datetime("2020-09-25 16:00:00"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = "red") +
  geom_rect(aes(xmin = as_datetime("2020-09-26 9:00:00"), xmax = as_datetime("2020-09-26 12:00:00"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = "red") +
  geom_rect(aes(xmin = as_datetime("2020-09-27 13:00:00"), xmax = as_datetime("2020-09-27 16:00:00"), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = "red") +
  geom_line(data = df_hour, aes(x = time, y = temp), colour = "skyblue4", size = 2) +
  geom_point(data = df_hour, aes(x = time, y = temp)) +
  geom_point(data = df_hour, aes(x = time, y = temp), colour = "skyblue4", size = 2,  shape = 21, fill = "white") +
  geom_segment(data = df_hour, aes(x = time, y = precip_amt*3, xend = time, yend = 0), stat = "identity", colour = "skyblue4", size = 4) +
  scale_y_continuous(name = expression("Temperature " ( degree*C)), 
                     sec.axis = sec_axis(~ (.) / 3 , name = "Precipitation (mm)"),
                     limits = c(0, 30),
                     expand = c(0, 0)) +
  scale_x_datetime(date_labels = "%b-%d", 
                   date_breaks = "1 day", 
                   expand = c(0.01,0.01), 
                   name = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom="text", x = as_datetime(c("2020-09-25 06:0:00", "2020-09-26 02:0:00",  "2020-09-27 06:0:00")), y = 26, label="Sampling",
           color="black",
           angle = 90)

p4

#ggsave(filename = "week_brandon_2020.png", plot = p4, height = 100, width = 150, units = "mm", dpi = 600)
