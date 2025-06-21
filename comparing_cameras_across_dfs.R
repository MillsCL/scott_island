
# Compare initial camera_detection_coords (from Megan) csv to all cameras 
# downloaded from Wildtrax

# Load camera location data
camera_locations <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_may/camera_detections_coords.csv')

# Load camera data (from Wildtrax)
camera_data <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/2025_june/camera_detections.csv')

# Subset 'location' in both datasets (the camera #)
cam1 <- unique(camera_locations$location)
cam2 <- unique(camera_data$location)

# See which cameras from Wildtrax are not in other df
x <- setdiff(cam1, cam2)
x

# To see which cameras aren't in both datesets
y <- setdiff(union(cam1, cam2), intersect(cam1, cam2))
y

# setdiff() removes similar ones that are in both, leaving only those that are
  # in one dataset
# union() gives all unique values across both datasets
# intersect() gives only values present in both