#' downloadCopernicusDem
#'
#' @description
#' Download Copernicus Digital Elevation Model considering area of interest
#'
#'
#' @param aoi sf object. Area of interest.
#' @param outputDir character. Output directory folder path. default = './copernicusDem'.
#' @param outputFileName character. Output filename with '.tiff' extension. default = 'copernicusDem.tif'.
#' @param res numeric. desired resolution (30 or 90 meters). default = 90.
#' @param type character. Copernicus DEM product. Default = 'DGED'.
#' @param tempDir character. Temporary directory folder name. Default = 'tempDirDem'.
#' @param keepInvidualTiles logical. Keep downloaded tiles?. Default = FALSE.
#'
#' @return
#'
#' Digital elevation model for area of interest
#'
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#'
#'require(sf)
#'
#'area_of_interest = st_read("./area_of_interest.shp")
#'
#'
#'dem = downloadCopernicusDem(aoi = area_of_interest,
#'                            outputDir = "./copernicusDem",
#'                            outputFileName = "copernicusDem.tif",
#'                            res = 90,
#'                            type = "DGED",
#'                            tempDir = "tempDirDem",
#'                            keepInvidualTiles = FALSE)
#'
#'}
#'
#'




downloadCopernicusDem = function(aoi,
                                 outputDir = "./copernicusDem",
                                 outputFileName = "copernicusDem.tif",
                                 res = 90,
                                 type = "DGED",
                                 tempDir = "tempDirDem",
                                 keepInvidualTiles = FALSE){



  #some verifications----

  stopifnot(
    "`outputDir` parameter must be character indicating output folder path (i.e `./copernicusDem`)" = is.character(outputDir),
    "`outputFileName` parameter must be character indicating dem output filename with '.tif' extension (i.e `copernicusDem.tif`)" = is.character(outputFileName),
    "`res` must be numeric vector indicating dem resolution (30 ou 90)" = res %in% c(30,90),
    "`type` must be character vector indicating Copernicus dem data type ('DGED' or 'DTED')" = type %in% c("DGED", "DTED"),
    "`aoi` must be a polygon of class `SpatVector` (terra package)" = "sf" %in% class(aoi),
    "`tempDir` parameter must be character indicating temporary directory name (i.e `tempDirDem`)" = is.character(tempDir),
    "`keepInvidualTiles` parameter must be logical" = is.logical(keepInvidualTiles))


  aoi = aoi %>%
    sf::st_transform(4326)

  #identify grid lat long----

  outputDirTempFile = paste0(outputDir, "/outputDirTemp1234")

  #create folder
  dir.create(outputDir, recursive = T, showWarnings = F)
  dir.create(outputDirTempFile, showWarnings = F)


  #download grid data at github

  print("Downloading Copernicus DEM Grid tiles")

  utils::download.file(
    "https://github.com/hydroversebr/miscellaneous/blob/main/copDemGrid.zip?raw=TRUE",
    paste0(outputDirTempFile, "/copDemGrid.zip"),
    mode = "wb",quiet = T
  )

  #unzip
  utils::unzip(zipfile = paste0(outputDirTempFile, "/copDemGrid.zip"),
        exdir = paste0(outputDirTempFile))

  #delete zip file
  unlink(paste0(outputDirTempFile, "/copDemGrid.zip"))

  #identify gridshape at outputDirTempFile
  print("Identifing tiles at 'aoi'")

  shp = list.files(outputDirTempFile, full.names = T, pattern = ".shp")

  #read gridshape
  shp = sf::st_read(shp, quiet = T)

  #mask gridshape by aoi
  gridDem = sf::st_intersection(shp, aoi) %>%
    suppressWarnings()

  #get gridshape code
  gridCode = gridDem %>%
    dplyr::pull(GeoCellID) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(lat = substr(.$value, 0,3),
           long = substr(.$value, 4,7))



  lat = gridCode %>%
    dplyr::pull(lat)

  long = gridCode %>%
    dplyr::pull(long)


  #identify http files names to be donwloaded----

  print("Listing 'http' files to download")

  #download http file names at git
  xmlDownload = paste0("COP-DEM_GLO-", res, "-", type, "__2023_1")

  utils::download.file(
    paste0("https://github.com/hydroversebr/miscellaneous/blob/main/", xmlDownload, "?raw=TRUE"),
    paste0(outputDirTempFile, "/arquivos.xml"),
    mode = "wb", quiet = T
  )

  #convert xml to df
  data = XML::xmlParse(paste0(outputDirTempFile, "/arquivos.xml"))

  df_data = XML::xmlToDataFrame(data)

  #code to rename files
  if(res == 30){
    value = 10
  } else { value = 30}


  #dt with http names, ordem, gridCode and file name
  fileNames = df_data %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(ordem = 1:dplyr::n(),
           gridCode = gsub(pattern = paste0("https://prism-dem-open.copernicus.eu/pd-desk-open-access/prismDownload/COP-DEM_GLO-", res, "-", type, "__2023_1/Copernicus_DSM_", value, "_"),
                           replacement = "", .$text),
           nome = gsub(pattern = paste0("https://prism-dem-open.copernicus.eu/pd-desk-open-access/prismDownload/COP-DEM_GLO-", res, "-", type, "__2023_1/"),
                       replacement = "",
                       x = .$text)) %>%
    dplyr::filter(grepl(paste(long,collapse="|"), x = .$gridCode)) %>%
    dplyr::filter(grepl(paste(lat,collapse="|"), x = .$gridCode))



  #download dem----


  i = 1
  for (i in 1:nrow(fileNames)){

    options(timeout=600)

    print(paste0("Downloading ", i, "/", nrow(fileNames), " tiles"), sep = " ")

    destfile = paste(outputDirTempFile,"/", fileNames$nome[i], sep = "")

    utils::download.file(fileNames$text[i], destfile = destfile, mode = "wb", quiet = T)

    nome = gsub(fileNames$nome[i], pattern = ".tar", replacement = "")

    utils::untar(destfile, files = paste0(nome, "/DEM/", nome, "_DEM.tif"), exdir = paste0(outputDirTempFile, "/outputs"))

    unlink(destfile)



  }


  #delete or not invidual tiles?----

  fileToDelete = list.files(outputDirTempFile, recursive = T)

  demFiles = list.files(outputDirTempFile, recursive = T, pattern = "DEM.tif", full.names = T)

  if(keepInvidualTiles == TRUE){

    newDemFiles = gsub(pattern = ".*DEM/",
                       replacement = paste0(outputDir, "/tilesCopernicusDem/"),
                       x = demFiles)

    dir.create(paste0(outputDir, "/tilesCopernicusDem"), showWarnings = F)

    file.copy(from = demFiles,
              to = newDemFiles)

  }


  #merge dem ----

  print("Merging tiles")

  d = list.files(outputDirTempFile, full.names = T, pattern = ".tif", recursive = T)

  rsrc <- terra::sprc(d)

  finalDem = terra::mosaic(rsrc)

  finalDem = terra::mask(terra::crop(finalDem, aoi), aoi)

  #export dem and delete tempFiles----

  terra::writeRaster(finalDem, paste0(outputDir,"/", outputFileName), overwrite = T)

  unlink(outputDirTempFile, force = T, recursive = T)

  gc()

  print("Job Complete")

  return(finalDem)

}



if(getRversion() >= "2.15.1")  utils::globalVariables(c("GeoCellID"))
