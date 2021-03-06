# auscophubRutils
[![Travis-CI Build Status](https://travis-ci.org/Bartesto/auscophubRutils.svg?branch=master)](https://travis-ci.org/Bartesto/auscophubRutils) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Bartesto/auscophubRutils?branch=master&svg=true)](https://ci.appveyor.com/project/Bartesto/auscophubRutils)

These functions have been designed to be part of a workflow incorporating data from the Australian Regional Copernicus Hub and follow on from using some of their python scripts which identify suitable Sentinel-2 data tiles for download.

In short these functions:

1. Create a local archive for storing Sentinel-2 data
2. Moves the original zip file to this archive and unzips it 
3. Extracts the jp2 band data and the metadata xml
4. Compiles spectral band data according to user specified pixel resolution
5. Cleans up the archive just leaving the multiband raster, the metadata xml and the original zipped download file

## Installation

You can install auscophubRutils from github with:

```R
# install.packages("devtools")
devtools::install_github("Bartesto/auscophubRutils", build_vignettes = TRUE)
```


## Suggested Workflow

### 1. Download
Download Sentinel 2 data from the Australian Copernicus Hub. This should be done with either the [SARA GUI](https://copernicus.nci.org.au/sara.client/#/home) or through the SARA API using Python scripts. The data should be left in the original zipped files in one location

### 2. Process
As there have been numerous Sentinel products developed and issued by the ESA, our Sentinel archive has grown. With the aim of avoiding a single archive of mixed and confusing products, the scripts here won't send any processed data direct to the established archive on our Programs `Y:/drive`. The intention is that the user will assess what level of data was acquired and processed (i.e. L1C or L2A) and then move them to an appropriate location.

There are a number of R functions written to handle the details but there is only one that you need to interact with and that is `sentinel_extract()`. You will need to supply the following parameters:

* **path** - a character string representing the file path to the downloaded zip file or files. Please not that the products will be found here once the function is run and it is up to the user to move the products to the appropriate final destination.
* **res** - a character string representing the pixel resolution of the required compiled multiband raster/s. Spectral band data comes in 3 pixel resolutions, 10m, 20m and 60m and only like resolutions can be combined. Valid choices are `"10"`, `"20"`, `"60"` or `"all"` to save at all data. Defaults to `"10"` for optical data.
* **ext** - a character string representation of the desired file extension (raster format) for the output. Intention was for `"tif"` or `"img"` however any format recognised by GDAL could work. Defaults to `"tif"` for GeoTiff format.

For example if you ran the below on a Sentinel-2 download called "*S2B_MSIL2A_20200117T021339_N0213_R060_T50HLK_20200117T055505.zip*":
```R
sentinel_extract(path = "C:/downloads", res = "all", ext = "tif")
```
The above call would find the zipped file in "*C:/downloads*" and create the following folder structure and contents:

* T50HLK
  + 20200117
    + *MTD_MSIL2A.xml* - image metadata
    + *S2B_MSIL2A_20200117T021339_N0213_R060_T50HLK_20200117T055505.zip* - original zipped folder
    + *T50HLK_20200117_L2A_10m.tif* - 10m pixel bands
    + *T50HLK_20200117_L2A_20m.tif* - 20m pixel bands
    + *T50HLK_20200117_L2A_60m.tif* - 60m pixel bands

**NOTE** that the function can handle multiple zipped files and that the data doesn't have to be all for the same tile. The function checks for the existence of the folder before creating it. If for instance you had created 10m data but then wanted to re-run it to create 60m data it will simply add this to the folder already existing. You will however need to move the zipped folder to the location that contains the tile folder.
