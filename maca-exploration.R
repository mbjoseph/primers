library(raster)
library(tidyverse)
library(parallel)
library(pbapply)

mask_filenames <- list.files('data/masks', 
                             pattern = '\\.tif$', 
                             full.names = TRUE) 
masks <- mask_filenames %>%
  lapply(raster::raster)
mask_names <- basename(mask_filenames) %>%
  gsub(pattern = '\\.tif', replacement = '', x = .)
names(masks) <- mask_names

outdir <- file.path('data', 'nps')
dir.create(outdir, showWarnings = FALSE)

d <- read_delim('data/macav2metdata_urls.txt', delim='\n', col_names = 'url')


process_file <- function(i) {
  # download and process a  MACA file to create cropped files for each park
  # check whether output already exists
  destf <- trimws(basename(d$url[i]))
  out_filenames <- paste(names(masks), 
                         gsub('\\.nc$', '.tif', destf), 
                         sep = '_') %>%
    file.path(outdir, .)
  files_already_exist <- all(file.exists(out_filenames))
  if (files_already_exist) {
    return(NULL)
  }
  
  # if output doesn't exist yet, create it
  download.file(d$url[i], destfile = destf)
  
  r <- stack(destf)
  er <- extent(r)
  extent(r) <- c(er@xmin - 360, 
                 er@xmax - 360, 
                 er@ymin, 
                 er@ymax)
  projection(r) <- "+proj=longlat +ellps=WGS84 +no_defs"
  
  # iterate over the masks and save geotiffs
  for (j in seq_along(masks)) {
    r %>%
      raster::crop(masks[[j]]) %>%
      writeRaster(filename = out_filenames[j])
  }
  unlink(destf)
}


cl <- makeCluster(detectCores())
clusterExport(cl, 'outdir')
clusterExport(cl, 'masks')
clusterExport(cl, 'd')
clusterEvalQ(cl, library(raster))
clusterEvalQ(cl, library(magrittr))
out <- pblapply(as.list(1:nrow(d)), process_file, cl = cl)
stopCluster(cl)
