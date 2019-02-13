library(raster)
library(sf)
library(fasterize)
library(geojsonio)

r <- raster('macav2metdata_huss_bcc-csm1-1_r1i1p1_historical_1950_1954_CONUS_daily.nc')
er <- extent(r)
extent(r) <- c(er@xmin - 360, 
               er@xmax - 360, 
               er@ymin, 
               er@ymax)
geojson_path <- 'nps.geojson'
if (!file.exists(geojson_path)) {
  url <- 'http://open-fedmaps.opendata.arcgis.com/datasets/8b98df1300b749eabc3142864bb9a119_0.geojson'
  download.file(url, destfile = geojson_path)
}

parks <- geojson_read(geojson_path, what = "sp")
poly <- subset(parks, !(STATE %in% c('MP', 'HI', 'PR', 'GU', 'AK', 'VI', 'AS'))) %>%
  spTransform(projection(r))

plot(r)
plot(poly, add = TRUE)


# Filter out polygons that are out of the bounds of MACA ------------------
rpoly <- list()
all_na <- rep(NA, nrow(poly))
pb <- txtProgressBar(max = nrow(poly), style = 3)
for (i in 1:nrow(poly)) {
  rpoly[[i]] <- fasterize(as(poly[i, ], 'sf'), r, field = 'Shape__Area')
  all_na[i] <- all(is.na(raster::values(mask(r, rpoly[[i]]))))
  setTxtProgressBar(pb, i)
}
polys_to_extract <- rpoly[!all_na]
names(polys_to_extract) <- as.character(poly$UNIT_NAME[!all_na])


# For in-bounds polygons, extract a raster --------------------------------
r_list <- vector(mode = 'list', length = length(polys_to_extract))
names(r_list) <- names(polys_to_extract)
for(i in seq_along(polys_to_extract)){
  r_list[[i]] <- trim(mask(r, polys_to_extract[[i]]))
}

idx <- sample(length(r_list), size = 1)
plot(r_list[[idx]], main = names(r_list)[idx])

