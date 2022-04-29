# 2022-03-09 @bdill analysis of camera lines over time
# https://docs.google.com/spreadsheets/d/1V8KOfTOap3068VdzyFFE1oisGPfrLEo9icfY9NeGDCA/edit#gid=1073613661

library(tidyverse)
rm(list = ls())

d <- read_tsv("C:/Users/bdill/Downloads/camera_lines.tsv")
d <- janitor::clean_names(d)
d <- d %>% mutate(make_line = paste(make, line))
#str(d)
d$wifi <- as.factor(d$wifi)
#d %>% count(make)
#d %>% count(line)

# Make
ggplot(d, aes(x = year, y = m_pixels, color = make)) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = loess, se = FALSE) +
  facet_wrap(~make) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  labs(title = "Megapixels Over Time by Camera Line",
       subtitle = "Interchangable Lens Cameras",
       y = "Megapixels",
       x = "Year", 
       color = "Make",
       caption = "@bdill\nData: https://bit.ly/camera_db")

#Make/Sensor
d %>% #filter(make %in% c("Nikon")) %>% 
ggplot(aes(x = year, y = m_pixels, color = sensor)) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~make) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  labs(title = "Megapixels Over Time by Make/Sensor",
       subtitle = "Interchangable Lens Cameras",
       y = "Megapixels",
       x = "Year", 
       color = "Sensor",
       caption = "@bdill\nData: https://bit.ly/camera_db")

ggplot(d, aes(x = year, y = m_pixels)) +
  geom_jitter(size = 3, alpha = 0.6, aes(color = make)) +
  geom_smooth(method = loess, se = FALSE, size = 1, alpha = 0.3) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  scale_x_continuous(breaks = seq(1995,2025,5)) +
  labs(title = "Megapixels Over Time by Camera Line",
       subtitle = "Interchangable Lens Cameras",
     y = "Megapixels",
     x = "Year",
     color = "Make",
     caption = "(C) 2022 @bdill\nData: https://bit.ly/camera_db")

d %>% #filter(sensor != "APS-H") %>% 
ggplot(aes(x = year, y = m_pixels, color = sensor)) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = lm, se = TRUE) +
  facet_wrap(~sensor) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  labs(title = "Megapixels Over Time by Sensor Size",
       subtitle = "Interchangable Lens Cameras",
       y = "Megapixels",
       x = "Year", 
       color = "Make",
       caption = "@bdill\nData: https://bit.ly/camera_db")



dff <- d %>% filter(sensor == "FF")
dff <- dff %>% mutate(group = as.factor(
                        case_when( m_pixels < 34 ~ 0,
                                  TRUE ~ 1)))
dff %>% #filter(sensor != "APS-H") %>% 
  ggplot(aes(x = year, y = m_pixels, color = make, group = group)) +
  geom_jitter(size = 4, alpha = 0.5) +
  geom_smooth(method = lm, se = TRUE, alpha = 0.3) +
  facet_wrap(~sensor) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  labs(title = "Megapixels Over Time",
       subtitle = "Full Frame Interchangable Lens Cameras",
       y = "Megapixels",
       x = "Year", 
       color = "High MP",
       shape = "Make",
       caption = "@bdill\nData: https://bit.ly/camera_db")

d %>% filter(year > 2008) %>% 
ggplot(aes(x = year, y = msrp_body, size = m_pixels, color = make)) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~sensor)

#---- Bridge ----
b <- read_tsv("C:/Users/bdill/Desktop/bridge_cameras.tsv")
b <- janitor::clean_names(b)
b <- b %>% mutate(make_line = paste(make, line),
                  make_body = paste(make, body),
                  make_sensor = paste0(make, " ", sensor_size))

b %>% filter(make != "Samsung" & make != "Pentax") %>% 
  ggplot(aes(x = year, y = megapixels, color = make)) +
  geom_jitter() +
  geom_smooth(method = lm, se=F) +
  facet_wrap(~make_body)

# Make_line
b %>% count(make_line)
b %>% count(make_body)
b %>% 
ggplot(aes(x = year, y = megapixels, color = make)) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~make_body) +
  scale_y_continuous(breaks = seq(0,65,8)) +
  coord_cartesian(ylim=c(0,NA)) +
  labs(title = "Bridge Cameras",
       subtitle = "Megapixels Over Time by Camera Line",
       y = "Megapixels",
       x = "Year", 
       color = "Make",
       caption = "@bdill\nData: https://bit.ly/camera_db")

# Make/Line (storage)
b %>% filter(!is.na(storage)) %>% 
  ggplot(aes(x = year, y = megapixels, color = storage, group = make_line)) +
  geom_jitter(size=2, alpha = 0.4) +
  geom_smooth(method = lm, se = FALSE) +
#  facet_wrap(~make_line) +
  labs(title = "Megapixels Over Time")

#Make/Sensor
ms_list <- b %>% count(make_sensor) %>% filter(n > 5) %>% select(make_sensor)

b %>% filter(make_sensor %in% pull(ms_list) ) %>% 
  ggplot(aes(x = year, y = megapixels, color = make_sensor, group = make_sensor)) +
  geom_jitter(size=2, alpha = 0.4) +
  geom_smooth(method = lm, se = T) +
  facet_wrap(~make_sensor) +
  labs(title = "Fixed Lens Cameras",
       subtitle = "Megapixels Over Time",
       color = "Make + Sensor")
