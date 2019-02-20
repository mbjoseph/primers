library(geojsonio)
library(raster)
library(tidyverse)
library(viridis)
library(rgeos)

d <- read_delim('data/macav2metdata_urls.txt', delim='\n', col_names = 'url')


destf <- trimws(basename(d$url[1]))
download.file(d$url[1], destfile = destf)

r <- raster(destf)
er <- extent(r)
extent(r) <- c(er@xmin - 360, 
               er@xmax - 360, 
               er@ymin, 
               er@ymax)
projection(r) <- "+proj=longlat +ellps=WGS84 +no_defs"

plot(r, col = viridis(100))


geojson_path <- 'nps.geojson'
if (!file.exists(geojson_path)) {
  url <- 'http://open-fedmaps.opendata.arcgis.com/datasets/8b98df1300b749eabc3142864bb9a119_0.geojson'
  download.file(url, destfile = geojson_path)
}

parks <- geojson_read(geojson_path, what = "sp")

national_parks <- parks %>%
  subset(!(STATE %in% c('MP', 'HI', 'PR', 'GU', 'AK', 'VI', 'AS'))) %>%
  subset(UNIT_TYPE == 'National Park') %>%
  subset(UNIT_NAME != 'Dry Tortugas National Park') %>%
  spTransform(projection(r))

plot(r, col = viridis(100), axes = FALSE, box = FALSE, legend = FALSE)
plot(national_parks, add = TRUE)


# Iterate over parks, saving raster versions of each ----------------------
pb <- txtProgressBar(max = nrow(national_parks), style = 3)
for (i in seq_len(nrow(national_parks))) {
  park_r <- rasterize(national_parks[i, ], r)
  all_na <- all(is.na(values(park_r)))
  if (all_na) {
    centroid <- gCentroid(national_parks[i, ])
    park_r <- rasterize(centroid, r)
    print('Using point representation of:')
    print(as.character(national_parks$UNIT_NAME[i]))
  }
  code <- national_parks$UNIT_CODE[i]
  writeRaster(trim(park_r), 
              filename = file.path('data/masks', paste0(code, '.tif')), 
              overwrite = TRUE)
  setTxtProgressBar(pb, i)
}
