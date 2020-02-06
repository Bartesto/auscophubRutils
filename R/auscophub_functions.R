#' Internal function for constructing archive folders for Sentinel-2 data.
#'
#' \code{s_mk_fold} will construct a nested folder structure of the format,
#' tile > date. The details are taken from a downloaded Sentinel 2 data zipped
#' file name and the folders are used to house the zipped file for all future
#' processing actions.
#'
#' @param dzip is a character filepath for one zipped Sentinel download which
#'  is passed from the parent function.
#'
#' @param path is a character filepath to the location of the Sentinel zipped
#'  folder and is where the archive folders will be situated.
#'
#' @return It will create a folder structure (tile > date) for processing of
#'  the Sentinel download and return the file path the end location.
#'
#' @importFrom stringr str_extract
s_mk_fold <- function(dzip = dzip, path = path){
  tile <- stringr::str_extract(dzip, "[T]{1}[:digit:]{2}[A-Z]{3}")
  date <- stringr::str_extract(dzip, "[:digit:]{8}")
  tfold <- file.path(path, tile)
  dfold <- file.path(tfold, date)
  if(!file.exists((dfold))){dir.create(dfold, recursive = T)}
  return(dfold)
}

#' Internal function for unzipping and compiling Sentinel 2 data.
#'
#' \code{s_organise} will unzip a Sentinel-2 downloaded file and compile the
#' jp2 spectral band data into a multi band raster. It can compile specific
#' spectral bands based on a provided pixel resolution (10m, 20m or 60m) or can
#' compile for all three. On completion it cleans up by leaving only the
#' downloaded zip file, the raster stack/s and an xml with original metadata.
#'
#' @param dzip is a character filepath for one zipped Sentinel download which
#'  is passed from the parent function.
#'
#' @param path is a character filepath to the location of the Sentinel-2 zipped
#'  file and is where the archive folders will be situated. This will be
#'  passed from the parent function.
#'
#' @param res is a character representation of the required pixel resolution and
#' can be ONE of "10", "20", "60" or "all" to produce all three possible. This
#' will be passed from the parent function.
#'
#' @param ext is a character representation of the desired file extension for
#' the output raster stack/s. Intended to be either "tif" or "img" but would
#' handle any format that GDAL supports. This will be passed from the parent
#' function.
#'
#' @return It will create intended raster stack/s outputs and leave a clean
#' directory minus everything bar the original data download and a metadata xml.
#'
#' @importFrom utils unzip
#'
#' @importFrom raster stack writeRaster
s_organise <- function(dzip = dzip, path = path, res = res, ext = ext){
  ## zip work
  cat(paste0("Working on '", basename(dzip), "'...", "\n"))
  #move it
  loc <- s_mk_fold(dzip, path)
  mzip <- file.path(loc, basename(dzip))
  file.rename(dzip, mzip)
  #find jp2's only
  files <- unzip(mzip, list = T)$Name
  f_mtd_xml <- files[grep("MTD", files)][1]
  f_jp2 <- grep("IMG_DATA", grep(".jp2", files, value = T), value = T)
  f_bjp2 <- f_jp2[grep(pattern = "[B][A-Z0-9]{2}", f_jp2)]
  level <- substr(strsplit(dzip, "_")[[1]][3], 4, 6)
  #extract the band jp2's and QA xml
  cat("Extracting files...\n")
  get <- c(f_bjp2, f_mtd_xml)
  unzip(zipfile = mzip, exdir = loc, files = get, overwrite = T,
        junkpaths = T)
  files_bjp2 <- list.files(path = loc, pattern = ".jp2", full.names = T)

  ## stack work
  #res groups
  b10 <- files_bjp2[grep(pattern = "[1]{1}[0]{1}[m]", files_bjp2)]
  b20 <- files_bjp2[grep(pattern = "[2]{1}[0]{1}[m]", files_bjp2)]
  b60 <- files_bjp2[grep(pattern = "[6]{1}[0]{1}[m]", files_bjp2)]

  #geotiff name
  gname <- paste0(substr(basename(files_bjp2)[1], 1, 15), "_", level, "_", res,
                  "m.", ext)
  #stacking bands
  cat("Stacking and saving...\n")
  if(res == "10"){
    s <- raster::stack(b10)
    raster::writeRaster(s, filename = file.path(loc, gname))
  }
  if(res == "20"){
    s <- raster::stack(b20)
    raster::writeRaster(s, filename = file.path(loc, gname))
  }
  if(res == "60"){
    s <- raster::stack(b60)
    raster::writeRaster(s, filename = file.path(loc, gname))
  }
  if(res == "all"){
    s1 <- raster::stack(b10)
    s1name <- paste0(substr(gname, 1, 20), "10m.", ext)
    raster::writeRaster(s1, filename = file.path(loc, s1name))

    s2 <- raster::stack(b20)
    s2name <- paste0(substr(gname, 1, 20), "20m.", ext)
    raster::writeRaster(s2, filename = file.path(loc, s2name))

    s3 <- raster::stack(b60)
    s3name <- paste0(substr(gname, 1, 20), "60m.", ext)
    raster::writeRaster(s3, filename = file.path(loc, s3name))
  }

  ## clean up
  file.remove(files_bjp2)
}

#' Compile Sentinel-2 data into multiband rasters
#'
#'  \code{sentinel_extract} will unzip all Sentinel-2 downloaded files and
#' compile the jp2 spectral band data into multi band rasters. It can compile
#' groups of spectral bands based on  pixel resolution (10m, 20m or
#' 60m) or can compile for all three. On completion it cleans up by leaving only
#' the downloaded zip file, the raster stack/s and an xml with original metadata
#' in an archivable folder structure (tile > date). It is intended that the
#' folder/s and their contents be moved to a final location which could be
#' dependent on processing level of the downloaded data (e.g. L1C or L2A).
#'
#' @param path is a character filepath to the location of the Sentinel-2 zipped
#'  file/s. Note that this is also where the processed data will be saved.
#'
#' @param res is a character representation of the required pixel resolution and
#' can be ONE of "10", "20", "60" or "all" to produce all three possible. Default
#' is for the 10 metre optical bands.
#'
#' @param ext is a character representation of the desired file extension for
#' the output raster stack/s. Intended to be either "tif" or "img" but would
#' handle any format that GDAL supports. Default is "tif".
#'
#' @return all zipped files found will be processed to produce compiled raster
#' stacks in format of choice. Processed data will be stored in appropriate
#' folders (tile > date). The compiled data, a metadata xml file and
#' the zipped data for the original download will all be housed at this location.
#'
#' @examples
#' \dontrun{
#' sentinel_extract(path = "C:/downloads", res = "60", ext = "img")}
#'
#' @export
sentinel_extract <- function(path = path, res = "10", ext = "tif"){
  dzips <- list.files(path = path, pattern = ".zip", full.names = T)
  for(i in seq_along(dzips)){
    dzip <- dzips[i]
    s_organise(dzip, path, res, ext)
  }
}
