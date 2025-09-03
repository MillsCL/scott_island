library(ggrepel)
library(ggplot2)
library(dplyr)

# Determine bounding box of islands:
#N 50.837149, -128.709637
#E 50.817062, -128.715981
#S 50.781123, -128.614783
#W 50.797852, -128.570802

# Download shapefile from GADM site: https://gadm.org/download_country.html
# Read in the GADM Canada-1 file (provinces)
x <- st_read("/Users/laurenmills/Downloads/gadm41_CAN_shp/gadm41_CAN_1.shp")

# Filter to just British Columbia
bc <- x[x$NAME_1 == "British Columbia", ]

# Define your bounding box coordinates
north <- 50.837149
east  <- -128.560800
south <- 50.681123
west  <- -128.715982

# Create bounding box polygon
bbox <- st_bbox(c(xmin = west, ymin = south, xmax = east, ymax = north), crs = st_crs(bc))
bbox_poly <- st_as_sfc(bbox)

# Crop BC to bounding box
bc_crop <- st_intersection(bc, bbox_poly)


# Load locations
locations <- read.csv('/Users/laurenmills/Library/CloudStorage/OneDrive-UBC/Scott Island Project/test.csv')
locations <- st_as_sf(locations, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
locations <- st_transform(locations, st_crs(bc_crop)) # st_transform b/c terra::project is for spat objects, not sf objects

# Plot to check
plot(st_geometry(bc_crop))
points(locations, col = 'red')




# --------------------------------------------------------------------
# Determine which coordinates are incorrect:
# --------------------------------------------------------------------

# For all coords:
location_id <- locations
location_id$row_id <- 1:nrow(location_id)
#location_id <- st_as_sf(location_id, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")


# Currently: bc_crop is a polygon while locations is an sf POINT
# ggplot() +
#   geom_sf(data = bc_crop, fill = "grey80", color = "black") +
#   geom_sf(data = location_id, aes(color = island), size = 2) +
#   coord_sf(xlim = st_bbox(bc_crop)[c("xmin", "xmax")],
#            ylim = st_bbox(bc_crop)[c("ymin", "ymax")],
#            expand = FALSE) +
#   geom_text_repel(data = location_id,
#                   aes(x = lon, y = lat, label = island,
#                       size = 3, max.overlaps = Inf)) +
#   theme_minimal()

location_id_coords <- location_id %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

ggplot() +
  geom_sf(data = bc_crop, fill = "grey80", color = "black") +
  geom_sf(data = location_id, aes(color = island), size = 2) +
  coord_sf(xlim = st_bbox(bc_crop)[c("xmin", "xmax")],
           ylim = st_bbox(bc_crop)[c("ymin", "ymax")],
           expand = FALSE) +
  geom_text_repel(data = location_id_coords,
                  aes(x = lon, y = lat, label = row_id),
                  size = 3,
                  max.overlaps = Inf) +
  theme_minimal()





# For Cox
loc_cox <- locations[locations$island == "Cox",]

loc_cox$row_id <- 1:nrow(loc_cox)
loc_cox <- st_as_sf(loc_cox, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")

ggplot() +
  geom_sf(data = loc_cox, aes(color = island), size = 2) 

ggplot() +
  geom_sf(data = loc_cox, aes(color = island), size = 2) +
  geom_text_repel(
    data = loc_cox,
    aes(
      x = st_coordinates(loc_cox)[,1],
      y = st_coordinates(loc_cox)[,2],
      label = row_id),
    size = 3) +
  theme_minimal()

# For Lanz
loc_lanz <- locations[locations$island == "Lanz",]

loc_lanz$row_id <- 1:nrow(loc_lanz)
loc_lanz <- st_as_sf(loc_lanz, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")

ggplot() +
  geom_sf(data = loc_lanz, aes(color = island), size = 2) 

ggplot() +
  geom_sf(data = loc_lanz, aes(color = island), size = 2) +
  geom_text_repel(
    data = loc_lanz,
    aes(
      x = st_coordinates(loc_lanz)[,1],
      y = st_coordinates(loc_lanz)[,2],
      label = row_id),
    size = 3) +
  theme_minimal()