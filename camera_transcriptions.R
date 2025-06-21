# Load required packages
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)

# Set your shapefile path
camera <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/TK Wildlife_Scott_Island_Lure_Preference_main_report.csv') %>%
  select(location, image_date_time, species_common_name, individual_count, observer, tag_comments, tag_is_verified) %>%
  mutate(behaviour = tag_comments)

camera <- camera %>%
  mutate(location = case_when(
    location == "CAM-1" ~ "CAM-01",
    location == "CAM-8" ~ "CAM-08",
    location == "CAM-124A" ~ "CAM-124", # tentatively replace CAM-124A with CAM-124 (rows 234:237)
    location == "CAM-09" ~ "CAM-08", 
    TRUE ~ location  # keep original if no match
  ))

# camera_may <- camera %>%
#   mutate(dat_time = ymd_hms(image_date_time),
#          date = as.Date(dat_time),
#          time = format(dat_time, format = "%H:%M:%S")) # last date recorded is 27-Feb-2025


camera2 <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/TK Wildlife_Scott_Islands_main_report.csv') %>%
  select(location, image_date_time, species_common_name, individual_count, observer, tag_comments, tag_is_verified) %>%
  mutate(behaviour = tag_comments)

camera2 <- camera2 %>%
  mutate(location = case_when(
    location == "CAM-001" ~ "CAM-01",
    location == "CAM-1" ~ "CAM-01",
    location == "CAM-8" ~ "CAM-08",
    location == "CAM-09" ~ "CAM-08",
    location == "CAM-5" ~ "CAM-05",
    location == "CAM-124A" ~ "CAM-124", # replace CAM-124A with CAM-124 (rows 234:237)
    TRUE ~ location  # keep original if no match
  ))

# ----------------------------------------------------------------------------
# For which cameras had raccoon / mink on it since May 1
# ----------------------------------------------------------------------------

camera2_may <- camera2 %>%
  mutate(dat_time = ymd_hms(image_date_time),
         date = as.Date(dat_time),
         time = format(dat_time, format = "%H:%M:%S")) %>%
  filter(date >= ymd("2025-05-01"))

camera2_may <- camera2_may[camera2_may$tag_is_verified == "t", ] 

camera2_may_clean <- camera2_may %>%
  filter(species_common_name %in% c("Common Raccoon", "Mink")) %>%
  mutate(island = ifelse(location %in% c("CAM-91", "CAM-149", "CAM-142"), "Lanz", "Cox"))

write.csv(camera2_may_clean, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/camera_may_detections.csv')

# Visual of raccoon and mink detections 
detections <- 
  ggplot(camera2_may_clean, aes(x = date, y = location, fill = species_common_name)) +
  geom_point(shape = 21, size = 3, color = "black") +
  labs(x = "Date", y = "Camera Location", fill = "Species") +
  theme_minimal()

ggsave('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/detections.png', plot = detections, dpi = 600)

# ggplot(camera2_may_clean, aes(x = date, y = location, fill = species_common_name)) +
#   geom_point(shape = 21, size = 3, color = "black") +
#   labs(x = "Date", y = "Camera Location", fill = "Species") +
#   theme_minimal() +
#   facet_wrap(~ island)


# ----------------------------------------------------------------------------
# Combining entire datasets (all months)
# ----------------------------------------------------------------------------

camera_total <- rbind(camera, camera2)

camera_total <- camera_total %>%
  filter(tag_comments != "")

camera_total_clean <- camera_total[camera_total$tag_is_verified == "t", ] 

camera_total_clean <- camera_total_clean %>%
  filter(species_common_name %in% c("Common Raccoon", "Mink"))

camera_data <- camera_total_clean %>%
  mutate(
    dat_time = ymd_hms(image_date_time),  # make sure it's in POSIXct format
    time_5min = floor_date(dat_time, unit = "5 minutes")  # round down to nearest 5 min
  )

camera_data <- camera_data %>%
  mutate(dat_time = ymd_hms(dat_time)) %>%
  filter(dat_time >= ymd("2025-01-01"))

write.csv(camera_data, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/camera_detections.csv')






# ----------------------------------------------------------------------------
# Adding locations to cameras
# ----------------------------------------------------------------------------

# Adding locations for traps from May 1 onwards only

# Load coordinates for cameras
camera_locations <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_may/camera_detections_coords.csv')

df_merged <- camera_data %>%
  left_join(camera_locations, by = "location")

# Remove NA (I have no coordinates for CAM-09)

# camera_locations <- camera_locations %>%
#   mutate(camera = str_replace_all(camera, " ", "_"))
# 
# detections_grouped <- detections_grouped %>%
#   mutate(camera = str_replace_all(location, "-", "_"))
# 
# df <- left_join(detections_grouped, camera_locations, by = "camera")
# 
# df2 <- df %>%
#   group_by(camera, species_common_name, time_5min) %>%
#   slice(1) %>%  # keep just one detection per species per camera per 5 min
#   ungroup()
# 
# daily_detections <- df2 %>%
#   mutate(date = as_date(dat_time)) %>%
#   group_by(date) %>%
#   mutate(daily_total = n()) %>%
#   ungroup()
# 
# ggplot(daily_detections, aes(x = date, y = daily_total, colour = species_common_name)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 2) +
#   # geom_smooth(method = "lm", colour = "black", se = FALSE) +
#   labs(
#     title = "",
#     x = "Date",
#     y = "Number of Detections"
#     # Removed `colour = "Species"` since legend is gone
#   ) +
#   scale_colour_manual(
#     values = c(
#       "Common Raccoon" = "#1b9e77",
#       "Mink" = "#d95f02"
#     )
#   ) +
#   theme_bw() +
#   facet_wrap(~species_common_name) +
#   theme(
#     legend.position = "",  # ⬅️ Hides the legend
#     text = element_text(size = 12),
#     plot.title = element_text(hjust = 0.5, face = "bold")
#   )
# 
# #Calculate time since last encouter: 
# 
# last_detection_per_camera <- detections_grouped %>%
#   mutate(dat_time = ymd_hms(dat_time)) %>%  # ensure POSIXct
#   group_by(camera, site_id, species_common_name) %>%
#   summarise(
#     last_detection = max(dat_time, na.rm = TRUE),
#     total_detections = n(),
#     .groups = "drop"
#   ) %>%
#   ungroup()
# 
# 
# write.csv(last_detection_per_camera, "D:/KENNICOTTIICONSULTING WORK/SCOTTS_ISLAND_MINK_RACCOON_ERADICATION/MAY/last_detection_per_camera.csv")
# write.csv(df2, "D:/KENNICOTTIICONSULTING WORK/SCOTTS_ISLAND_MINK_RACCOON_ERADICATION/MAY/detections_final.csv")
# 
# # add in last detection overall
# 
# camera_data <- merged_data %>%
#   mutate(
#     dat_time = ymd_hms(image_date_time))
# 
# camera_data <- camera_data %>%
#   mutate(dat_time = ymd_hms(dat_time)) %>%
#   filter(dat_time >= ymd("2025-01-01"))
# 
# 
# last_detection_per_camera <- camera_data %>%
#   mutate(dat_time = ymd_hms(dat_time)) %>%  # ensure POSIXct
#   group_by(camera) %>%
#   summarise(
#     last_detection = max(dat_time, na.rm = TRUE),
#     total_detections = n(),
#     .groups = "drop"
#   ) %>%
#   ungroup()
# 
# library(tidyr)
# 
# # Step 1: Define target species
# target_species <- c("Common Raccoon", "Mink")
# 
# # Step 2: Get list of all cameras from metadata (not just detections)
# # Replace this with your actual metadata or deployment table if available
# all_cameras_df <- merged_data %>%
#   distinct(camera)
# 
# # Step 3: Create all combinations of camera × target species
# camera_species_grid <- expand_grid(
#   camera = all_cameras_df$camera,
#   species_common_name = target_species
# )
# 
# # Step 4: Count detections from detection data
# detection_counts <- merged_data %>%
#   filter(species_common_name %in% target_species) %>%
#   group_by(camera, species_common_name) %>%
#   summarise(detections = n(), .groups = "drop")
# 
# # Step 5: Left join & fill in 0 for missing combinations
# raccoon_mink_full <- camera_species_grid %>%
#   left_join(detection_counts, by = c("camera", "species_common_name")) %>%
#   mutate(detections = replace_na(detections, 0))
# 
# camera_total <- camera_total %>%
#   mutate(dat_time = ymd_hms(image_date_time)) %>%
#   filter(dat_time >= ymd("2025-01-01"))
# 
# 
# cam_clean <- cam_clean %>%
#   mutate(
#     dat_time = ymd_hms(image_date_time),  # make sure it's in POSIXct format
#     time_5min = floor_date(dat_time, unit = "5 minutes")  # round down to nearest 5 min
#   )
# 
# detections_grouped2 <- cam_clean %>%
#   group_by(location, species_common_name, time_5min) %>%
#   slice(1) %>%  # keep just one detection per species per camera per 5 min
#   ungroup()
# 
# 
# last_detection_per_camera <- detections_grouped2 %>%
#   mutate(dat_time = ymd_hms(dat_time)) %>%  # ensure POSIXct
#   group_by(location, species_common_name) %>%
#   summarise(
#     last_detection = max(dat_time, na.rm = TRUE),
#     total_detections = n(),
#     .groups = "drop"
#   ) %>%
#   ungroup()