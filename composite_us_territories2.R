library(tidyverse)
library(sf)

st_ellide_shift = function(x, x_shift=0, y_shift=0){
  return(x + c(x_shift, y_shift))
}

st_ellide_rotate = function(x, degrees){
  if(degrees < -360 | degrees > 360) stop('Degrees must be in the range -360 to 360')
  radians = degrees * pi/180
  transform_matrix = matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)
  centers = sf::st_centroid(x)
  return((x-centers) * transform_matrix + centers)
}

st_ellide_scale = function(x, scale){
  if(scale<=0) stop('Scale must be positive and non-zero')
  centers = sf::st_centroid(x)
  return(((x-centers)*scale) + centers)
}

st_bbox_by_feature = function(x) {
  geoms = sf::st_geometry(x)
  saved_crs = sf::st_crs(x)
  data_columns = sf::st_set_geometry(x, NULL)
  f <- function(y) sf::st_as_sfc(st_bbox(y))
  box_geoms = do.call("c", lapply(geoms, f))
  sf::st_sf(data_columns, geometry=box_geoms, crs=saved_crs)
}

transform_by_name = function(x, polygon_name, x_shift=0, y_shift=0, scale=1, degrees=0){
  #------------------
  # Pull out feature with name==polygon_name, apply transformations, then put it back in
  # returning the entire sf object
  #------------------
  focal_feature_i = which(x$name==polygon_name)
  focal_geom = x$geometry[focal_feature_i]

  focal_geom = st_ellide_rotate(focal_geom, degrees = degrees)
  focal_geom = st_ellide_shift(focal_geom, x_shift=x_shift, y_shift=y_shift)
  focal_geom = st_ellide_scale(focal_geom, scale=scale)

  x$geometry[focal_feature_i] = focal_geom
  return(x)
}


#-------------------------------
us_composite = st_read('./data/new_us_composite.geojson')

#------------------------------
outliers = c('Puerto Rico','United States Virgin Islands','Northern Mariana Islands','American Samoa','Guam','Alaska','Hawaii')

adjusted = us_composite %>%
  transform_by_name('Northern Mariana Islands', x_shift = -2, y_shift=3, scale=1.2) %>%
  transform_by_name('American Samoa', x_shift = -1.65, y_shift=2.7, scale=1.2) %>%
  transform_by_name('Guam', x_shift = -1.5, y_shift=2.7, scale=1.2) %>%
  transform_by_name('United States Virgin Islands', x_shift = 0, y_shift=-0.5) %>%
  transform_by_name('Puerto Rico', x_shift = 0, y_shift=-0.85) %>%
  transform_by_name('Alaska', degrees = 20, scale=1, x_shift=-8.45) %>%
  transform_by_name('Hawaii', degrees = 0, scale=1.5, x_shift=-4, y_shift = -0.65)

outlier_boxes = adjusted %>%
  filter(name %in% outliers) %>%
  st_bbox_by_feature() %>%
  st_buffer(0.5) %>%
  st_bbox_by_feature()       # bbox + buffer = rounded corners. run it thru bbox again to get sharp corners

ggplot(adjusted) +
  geom_sf() +
  geom_sf(data=outlier_boxes, fill=NA) +
  coord_sf() +
  theme_bw()

st_write(adjusted, './data/final_us_composite.geojson', append=F)
