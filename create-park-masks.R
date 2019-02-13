library(geojsonio)
library(raster)
library(tidyverse)
library(viridis)
library(rgeos)
library(ggrepel)

d <- read_delim('data/macav2metdata_urls.txt', delim='\n', col_names = 'url')

download.file(d$url[1], destfile='out.nc')

r <- raster('out.nc')
er <- extent(r)
extent(r) <- c(er@xmin - 360, 
               er@xmax - 360, 
               er@ymin, 
               er@ymax)

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



plot(trim(mask(r, national_parks[36, ])), col = viridis(100), axes = FALSE, 
     box = FALSE, legend = FALSE)

plot(trim(mask(r, national_parks[46, ])), col = viridis(100), axes = FALSE, 
     box = FALSE, legend = FALSE)

plot(trim(mask(r, national_parks[17, ])), col = viridis(100), axes = FALSE, 
     box = FALSE, legend = FALSE)


park_rasters <- list()
for (i in seq_len(nrow(national_parks))) {
  park_rasters[[i]] <- rasterize(national_parks[i, ], r)
  all_na <- all(is.na(values(park_rasters[[i]])))
  if (all_na) {
    centroid <- gCentroid(national_parks[i, ])
    park_rasters[[i]] <- rasterize(centroid, r)
    print('Using point representation of:')
    print(national_parks$UNIT_NAME[i])
  }
  park_rasters[[i]] <- trim(park_rasters[[i]])
}
names(park_rasters) <- national_parks$UNIT_CODE

dir.create('data/masks')
for (i in seq_along(park_rasters)) {
  code <- names(park_rasters)[i]
  writeRaster(park_rasters[[i]], filename = file.path('data/masks', 
                                                      paste0(code, '.tif')))
}


# get number of pixels per park
pixel_df <- lapply(park_rasters, function(x) sum(values(x), na.rm = TRUE)) %>%
  unlist %>%
  enframe(name = 'UNIT_CODE') %>%
  arrange(value) %>%
  mutate(order = 1:n()) %>%
  left_join(as.data.frame(national_parks))

pixel_df %>%
  ggplot(aes(value, order)) + 
  geom_point() + 
  geom_text_repel(aes(label = UNIT_NAME), data = filter(pixel_df, order > 44), 
                  nudge_y = -15) +
  ylab('Order (smallest to largest)') + 
  xlab('Number of MACA pixels')
