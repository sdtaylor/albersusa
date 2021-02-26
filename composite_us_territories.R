library(tidyverse)
library(sf)
library(albersusa)

data_dir = './data/'
dir.create(data_dir)
# download if needed
#country_shapefile = 'https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip'
#download.file(country_shapefile, paste0(data_dir, basename(country_shapefile)))
#unzip(paste0(data_dir,'ne_10m_admin_0_countries.zip'), exdir = data_dir)

#---------------------------------

territory_names = c('Puerto Rico','United States Virgin Islands','Northern Mariana Islands','American Samoa','Guam')

us_territories = st_read(paste0(data_dir,'ne_10m_admin_0_countries.shp')) %>%
  select(name = ADMIN) %>%
  filter(name %in% territory_names)

us_composite = albersusa::usa_sf()

#-------------------
# some transformation functions to work on sf geoms
# Note these will make the CRS na for whatever reason,
# though the techniques are in the docs: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
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

#----------------------------------
pr = us_territories %>%
  filter(name == 'Puerto Rico')

saved_crs = st_crs(pr)
pr$geometry = st_ellide_scale(pr$geometry, 2)
pr$geometry = st_ellide_shift(pr$geometry, -25, 6)
st_crs(pr) <- saved_crs

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=pr)

#--------------------------------
us_v = us_territories %>%
  filter(name == 'United States Virgin Islands') %>%
  st_buffer(dist=0.05) # Buffer helps a lot for clarity here

#ggplot(us_v) + geom_sf()

saved_crs = st_crs(us_v)
us_v$geometry = st_ellide_shift(us_v$geometry, -21, 6.25)
us_v$geometry = st_ellide_scale(us_v$geometry, 2.5)

st_crs(us_v) <- saved_crs

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=pr) +
  geom_sf(data=us_v)

#--------------------------------
n_mi = us_territories %>%
  filter(name == 'Northern Mariana Islands') %>%
  st_buffer(dist=0.1) # Buffer helps a lot for clarity here

#ggplot(n_mi) + geom_sf()

saved_crs = st_crs(n_mi)
n_mi$geometry = st_ellide_shift(n_mi$geometry, -272, 20)
n_mi$geometry = st_ellide_scale(n_mi$geometry, 1.5)

st_crs(n_mi) <- saved_crs

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=n_mi)

#--------------------------------
am_sam = us_territories %>%
  filter(name == 'American Samoa') %>%
  st_buffer(0.1)

# crop out Swains Island for clarity
am_sam = st_cast(am_sam, 'POLYGON')
main_islands_bb = st_as_sfc(st_bbox(c(xmin = -171, xmax = -167 , ymax = -13, ymin = -15), crs = st_crs(4326)))
am_sam = am_sam[st_within(am_sam, main_islands_bb, sparse = F)[,1],]
am_sam = am_sam %>% group_by(name) %>% summarise()

saved_crs = st_crs(am_sam)
am_sam$geometry = st_ellide_shift(am_sam$geometry, 44, 44.1)
am_sam$geometry = st_ellide_scale(am_sam$geometry, 1.8)

st_crs(am_sam) <- saved_crs

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=am_sam)

#--------------------------------
guam = us_territories %>%
  filter(name == 'Guam') %>%
  st_buffer(dist=0.1) # Buffer helps a lot for clarity here

#ggplot(guam) + geom_sf()

saved_crs = st_crs(guam)
guam$geometry = st_ellide_shift(guam$geometry, -265, 16.25)
guam$geometry = st_ellide_scale(guam$geometry, 1.5)

st_crs(guam) <- saved_crs

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=guam)

#----------------------------------
# put them all together
#----------------------------------
make_box = function(sf_object){
  st_sf(name = sf_object$name[[1]], geom=st_as_sfc(st_bbox(sf_object)))
}

st_bbox_by_feature = function(x) {
  geoms = sf::st_geometry(x)
  saved_crs = sf::st_crs(x)
  data_columns = sf::st_set_geometry(x, NULL)
  f <- function(y) sf::st_as_sfc(st_bbox(y))
  box_geoms = do.call("c", lapply(geoms, f))
  sf::st_sf(data_columns, geometry=box_geoms, crs=saved_crs)
}

us_territories_composite = pr %>%
  bind_rows(us_v) %>%
  bind_rows(n_mi) %>%
  bind_rows(am_sam) %>%
  bind_rows(guam)

us_territories_boxes  = st_bbox_by_feature(us_territories_composite) %>%
  st_buffer(0.5)

ggplot(us_composite) +
  geom_sf() +
  geom_sf(data=us_territories_composite) +
  geom_sf(data=us_territories_boxes, color='black', fill=NA)

us_composite %>%
  bind_rows(us_territories_composite) %>%
  st_write('./data/new_us_composite.geojson', append=F)
