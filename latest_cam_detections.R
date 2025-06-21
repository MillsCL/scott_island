

x <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/camera_detections.csv')

# CAMERA 09 IS ACTUALLY CAMERA 08
x <- x %>%
  mutate(location = as.character(location)) %>%
  mutate(location = case_when(
    location == "CAM-09" ~ "CAM-08",
    TRUE ~ location
  ))


latest_detections <- x %>%
  filter(species_common_name %in% c("Mink", "Common Raccoon")) %>%
  group_by(location) %>%
  summarise(latest_detection = max(image_date_time, na.rm = TRUE)) %>%
  ungroup()

latest_detections_species <- x %>%
  filter(species_common_name %in% c("Mink", "Common Raccoon")) %>%
  group_by(location) %>%
  slice_max(order_by = image_date_time, n = 1, with_ties = FALSE) %>%
  ungroup()

latest_detections_species <- latest_detections_species[, -1]


# need to manually add coordinates for CAM-152 (somewhere on beach 30 on Cox - picked a random location)
# also need to manually add coordinates for CAM-155
write.csv(latest_detections_species, file = '/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/latest_detections_species.csv')
