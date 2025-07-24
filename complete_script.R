# Load required packages
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)


# Load camera locations
camera_locations <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_may/camera_detections_coords.csv')
camera_locations <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_may/camera_locations.csv')


# ---------------------------------------------------------------------------
# Cleaning and combining all camera data
# ---------------------------------------------------------------------------

# Set your shapefile path
camera <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/TK Wildlife_Scott_Island_Lure_Preference_main_report.csv') %>%
  select(location, image_date_time, species_common_name, individual_count, observer, tag_comments, tag_is_verified) %>%
  mutate(behaviour = tag_comments)

camera2 <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/TK Wildlife_Scott_Islands_main_report.csv') %>%
  select(location, image_date_time, species_common_name, individual_count, observer, tag_comments, tag_is_verified) %>%
  mutate(behaviour = tag_comments)

camera_total <- rbind(camera, camera2)

camera_total <- camera_total %>%
  mutate(dat_time = ymd_hms(image_date_time),
         date = as.Date(dat_time),
         time = format(dat_time, format = "%H:%M:%S")) %>%
  filter(date >= ymd("2025-01-01"))

camera_total <- camera_total[camera_total$tag_is_verified == "t", ] 

camera_total <- camera_total %>%
  filter(species_common_name %in% c("Common Raccoon", "Mink")) %>%
  mutate(island = ifelse(location %in% c("CAM-91", "CAM-149", "CAM-142"), "Lanz", "Cox"))

camera_total <- camera_total %>%
  filter(tag_comments != "")

camera_total <- camera_total %>%
  mutate(location = case_when(
    location == "CAM-001" ~ "CAM-01",
    location == "CAM-1" ~ "CAM-01",
    location == "CAM-8" ~ "CAM-08",
    location == "CAM-09" ~ "CAM-08",
    location == "CAM-5" ~ "CAM-05",
    location == "CAM-124A" ~ "CAM-124",
    TRUE ~ location # keeping original if no match
  ))

camera_total <- camera_total %>%
  mutate(
    dat_time = ymd_hms(image_date_time),  # make sure it's in POSIXct format
    time_5min = floor_date(dat_time, unit = "5 minutes")  # round down to nearest 5 min
  )

# camera_data <- camera_total %>%
#   mutate(dat_time = ymd_hms(dat_time)) %>%
#   filter(dat_time >= ymd("2025-01-01"))

camera_locations_unique <- camera_locations %>%
  distinct(location, .keep_all = TRUE)

camera_df <- camera_data %>%
  left_join(camera_locations_unique, by = "location")

write.csv(camera_df, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_july/camera_detections.csv')


# ---------------------------------------------------------------------------
# Camera detections since July 1
# ---------------------------------------------------------------------------

# Determining which cameras had raccoon / mink on it since May 1
camera_data_may <- camera_df %>%
  mutate(dat_time = ymd_hms(image_date_time),
         date = as.Date(dat_time),
         time = format(dat_time, format = "%H:%M:%S")) %>%
  filter(date >= ymd("2025-05-01"))

write.csv(camera_data_may, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_july/camera_may_detections.csv')

# Visual of raccoon and mink detections 
detections <- 
  ggplot(camera_data_may, aes(x = date, y = location, fill = species_common_name)) +
  geom_point(shape = 21, size = 3, color = "black") +
  labs(x = "Date", y = "Camera Location", fill = "Species") +
  theme_minimal()

ggsave('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_july/detections.png', plot = detections, dpi = 600)


# ---------------------------------------------------------------------------
# Latest camera detections
# ---------------------------------------------------------------------------

#x <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/camera_detections.csv')

#CAMERA 09 IS ACTUALLY CAMERA 08
latest_detections_species <- camera_df %>%
  mutate(location = as.character(location)) %>%
  mutate(location = case_when(
    location == "CAM-09" ~ "CAM-08",
    TRUE ~ location
  ))


# latest_detections <- camera_data %>%
#   filter(species_common_name %in% c("Mink", "Common Raccoon")) %>%
#   group_by(location) %>%
#   summarise(latest_detection = max(image_date_time, na.rm = TRUE)) %>%
#   ungroup()

latest_detections_species <- latest_detections_species %>%
  filter(species_common_name %in% c("Mink", "Common Raccoon")) %>%
  group_by(location) %>%
  slice_max(order_by = image_date_time, n = 1, with_ties = FALSE) %>%
  ungroup()

# latest_detections_species <- latest_detections_species[, -1]

# need to manually add coordinates for CAM-152 (somewhere on beach 30 on Cox - picked a random location) # 50.793964	-128.630993
# also need to manually add coordinates for CAM-155
write.csv(latest_detections_species, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_july/latest_detections_species.csv')
