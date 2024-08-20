#' downloadCopernicusDemElps
#'
#' @description
#' Downloads the Copernicus Digital Elevation Model (DEM) for a specified area of interest (AOI).
#' This function supports parallel processing to expedite the download of individual tiles and includes
#' error handling to manage any issues that may arise during the download process.
#'
#' @param aoi sf object. The area of interest for which the DEM is to be downloaded.
#' @param outputDir character. The directory where the output files will be saved. Default is './copernicusDem'.
#' @param outputFileName character. The name of the output file with a '.tiff' extension. Default is 'copernicusDem.tif'.
#' @param res numeric. The desired resolution of the DEM, either 30 or 90 meters. Default is 90.
#' @param type character. The type of Copernicus DEM product. Default is 'DGED'.
#' @param outputDirTempFile character. The directory for temporary files during the download process. Default is './copernicusDem/tempDirDem'.
#' @param keepInvidualTiles logical. Whether to keep the individual downloaded tiles. Default is FALSE.
#' @param timeout numeric. The timeout in seconds for each download attempt. Default is 600.
#' @param ncores numeric. The number of processor cores to use for parallelizing the download operation. Default is 1 (no parallelization).
#' @param saveAsInteger logical. Whether to save the final raster as an integer. Default is FALSE.
#' @param multiplier numeric. A multiplier to apply to the data before converting to integer. Default is 1.
#' @param showRaster logical. Whether to plot the final raster. Default is FALSE.
#' @param retry numeric. The number of retry attempts for failed downloads. Default is 0 (no retries).
#'
#' @return
#' An sf object containing the digital elevation model for the specified area of interest.
#'
#' @details
#' This function downloads the Copernicus Digital Elevation Model (DEM) for a specified area of interest (AOI).
#' It identifies the necessary tiles based on the AOI, downloads them in parallel (if specified), and merges them into a single DEM file.
#' The function includes error handling to manage any issues that arise during the download process, ensuring robustness.
#'
#' @references
#' Bielski, C.; López-Vázquez, C.; Grohmann, C.H.; Guth. P.L.; Hawker, L.; Gesch, D.; Trevisani, S.; Herrera-Cruz, V.; Riazanoff, S.; Corseaux, A.; Reuter, H.; Strobl, P., 2024.
#' Novel approach for ranking DEMs: Copernicus DEM improves one arc second open global topography. IEEE Transactions on Geoscience & Remote Sensing.
#' https://ieeexplore.ieee.org/document/10440392
#'
#' European Space Agency, Sinergise (2021). Copernicus Global Digital Elevation Model. Distributed by OpenTopography. https://doi.org/10.5069/G9028PQB. Accessed: 2024-06-27
#'
#' @importFrom sf st_transform st_read st_intersection
#' @importFrom dplyr pull as_tibble mutate filter n
#' @importFrom utils download.file unzip untar
#' @importFrom XML xmlParse xmlToDataFrame
#' @importFrom terra sprc mosaic rast mask crop writeRaster
#' @importFrom future.apply future_lapply
#' @importFrom future plan multisession sequential
#' @importFrom graphics plot
#' @importFrom progressr handlers progressor with_progress
#' @export
#'
#' @examples
#' \dontrun{
#' require(sf)
#' area_of_interest <- st_read("./area_of_interest.shp")
#' dem <- downloadCopernicusDemElps(aoi = area_of_interest,
#'                              outputDir = "./copernicusDem",
#'                              outputFileName = "copernicusDem.tif",
#'                              res = 90,
#'                              type = "DGED",
#'                              outputDirTempFile = "./tempDirDem",
#'                              keepInvidualTiles = FALSE,
#'                              timeout = 600,
#'                              ncores = 1,
#'                              saveAsInteger = FALSE,
#'                              multiplier = 1,
#'                              showRaster = FALSE,
#'                              retry = 3)
#' }
#'
#

downloadCopernicusDemElps <- function(aoi,
                                      outputDir,
                                      outputFileName,
                                      res = 90,
                                      type = "DGED",
                                      outputDirTempFile = "./copernicusDem/tempDirDem",
                                      keepInvidualTiles = FALSE,
                                      timeout = 1000,
                                      ncores = future::availableCores()-2,
                                      saveAsInteger = FALSE,
                                      multiplier = 1,
                                      showRaster = FALSE,  #
                                      retry = 5) {

  # Verificações iniciais
  stopifnot(
    "`outputDir` parameter must be character indicating output folder path (i.e `./copernicusDem`)" = is.character(outputDir),
    "`outputFileName` parameter must be character indicating dem output filename with '.tif' extension (i.e `copernicusDem.tif`)" = is.character(outputFileName),
    "`res` must be numeric vector indicating dem resolution (30 ou 90)" = res %in% c(30, 90),
    "`type` must be character vector indicating Copernicus dem data type ('DGED' ou 'DTED')" = type %in% c("DGED", "DTED"),
    "`aoi` must be a polygon of class `sf` (sf package)" = "sf" %in% class(aoi),
    "`tempDir` parameter must be character indicating temporary directory name (i.e `tempDirDem`)" = is.character(outputDirTempFile),
    "`keepInvidualTiles` parameter must be logical" = is.logical(keepInvidualTiles),
    "`saveAsInteger` parameter must be logical" = is.logical(saveAsInteger),
    "`multiplier` parameter must be numeric" = is.numeric(multiplier),
    "`showRaster` parameter must be logical" = is.logical(showRaster),
    "`retry` parameter must be numeric indicating the number of retry attempts for failed downloads" = is.numeric(retry)
  )

  unlink(outputDirTempFile, recursive = T, force = TRUE)
  unlink(outputDir, recursive = T, force = TRUE)
  tryCatch({
    aoi <- sf::st_transform(aoi, 4326)
  }, error = function(e) {
    stop("Error transforming AOI to EPSG:4326: ", e$message)
  })

  # Criação de diretórios
  tryCatch({
    dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
    dir.create(outputDirTempFile, showWarnings = FALSE)
  }, error = function(e) {
    stop("Error creating directories: ", e$message)
  })

  # Download dos dados de grade do Copernicus DEM
  print("Downloading Copernicus DEM Grid tiles")

  tryCatch({
    utils::download.file(
      "https://github.com/hydroversebr/miscellaneous/blob/main/copDemGrid.zip?raw=TRUE",
      paste0(outputDirTempFile, "/copDemGrid.zip"),
      mode = "wb", quiet = TRUE
    )
  }, error = function(e) {
    stop("Error downloading Copernicus DEM Grid tiles: ", e$message)
  })

  # Descompactação
  tryCatch({
    utils::unzip(zipfile = paste0(outputDirTempFile, "/copDemGrid.zip"),
                 exdir = paste0(outputDirTempFile))
    unlink(paste0(outputDirTempFile, "/copDemGrid.zip"))
  }, error = function(e) {
    stop("Error unzipping Copernicus DEM Grid tiles: ", e$message)
  })

  # Identificação dos tiles na área de interesse
  print("Identifing tiles at 'aoi'")

  shp <- tryCatch({
    list.files(outputDirTempFile, full.names = TRUE, pattern = ".shp")
  }, error = function(e) {
    stop("Error identifying gridshape files: ", e$message)
  })

  # Leitura do shapefile
  shp <- tryCatch({
    sf::st_read(shp, quiet = TRUE)
  }, error = function(e) {
    stop("Error reading gridshape files: ", e$message)
  })

  # Interseção do grid com a área de interesse
  gridDem <- tryCatch({
    suppressWarnings(sf::st_intersection(shp, aoi))
  }, error = function(e) {
    stop("Error intersecting gridshape with AOI: ", e$message)
  })

  # Extração dos códigos dos tiles
  gridCode <- tryCatch({
    gridDem %>%
      dplyr::pull(GeoCellID) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(lat = substr(.$value, 0, 3),
                    long = substr(.$value, 4, 7))
  }, error = function(e) {
    stop("Error extracting gridshape codes: ", e$message)
  })

  lat <- gridCode %>%
    dplyr::pull(lat)

  long <- gridCode %>%
    dplyr::pull(long)

  # Listagem dos arquivos HTTP para download
  print("Listing 'http' files to download")

  # Download dos nomes dos arquivos HTTP
  xmlDownload <- paste0("COP-DEM_GLO-", res, "-", type, "__2023_1")

  tryCatch({
    utils::download.file(
      paste0("https://github.com/hydroversebr/miscellaneous/blob/main/", xmlDownload, "?raw=TRUE"),
      paste0(outputDirTempFile, "/arquivos.xml"),
      mode = "wb", quiet = TRUE
    )
  }, error = function(e) {
    stop("Error downloading HTTP file names: ", e$message)
  })

  # Conversão de XML para DataFrame
  data <- tryCatch({
    XML::xmlParse(paste0(outputDirTempFile, "/arquivos.xml"))
  }, error = function(e) {
    stop("Error parsing XML file: ", e$message)
  })

  df_data <- tryCatch({
    XML::xmlToDataFrame(data)
  }, error = function(e) {
    stop("Error converting XML to DataFrame: ", e$message)
  })

  # Código para renomear arquivos
  value <- if (res == 30) 10 else 30

  # DataFrame com nomes HTTP, ordem, gridCode e nome do arquivo
  fileNames <- tryCatch({
    df_data %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(ordem = 1:dplyr::n(),
                    gridCode = gsub(pattern = paste0("https://prism-dem-open.copernicus.eu/pd-desk-open-access/prismDownload/COP-DEM_GLO-", res, "-", type, "__2023_1/Copernicus_DSM_", value, "_"),
                                    replacement = "", .$text),
                    nome = gsub(pattern = paste0("https://prism-dem-open.copernicus.eu/pd-desk-open-access/prismDownload/COP-DEM_GLO-", res, "-", type, "__2023_1/"),
                                replacement = "",
                                x = .$text)) %>%
      dplyr::filter(grepl(paste(long, collapse = "|"), x = .$gridCode)) %>%
      dplyr::filter(grepl(paste(lat, collapse = "|"), x = .$gridCode))
  }, error = function(e) {
    stop("Error processing file names: ", e$message)
  })

  # Função para baixar tiles com lógica de retry
  download_tiles_with_retry <- function(indices, retry, timeout, outputDirTempFile, fileNames) {
    results <- future.apply::future_lapply(indices, function(i) {
      p()
      download_tile(i, timeout, outputDirTempFile, fileNames)
    })

    failed_downloads <- which(sapply(results, function(x) x$status == "download_failed"))

    failed_untar <- which(sapply(results, function(x) x$status == "untar_failed"))

    for (attempt in 1:retry) {
      if (length(failed_downloads) == 0 && length(failed_untar) == 0) break
      if (length(failed_downloads) > 0) {
        #message(paste("Retrying download for tiles:", paste(failed_downloads, collapse = ", ")))
        results[failed_downloads] <- future.apply::future_lapply(failed_downloads, function(i) {
          download_tile(i, timeout, outputDirTempFile, fileNames)
        })
        failed_downloads <- which(sapply(results, function(x) x$status == "download_failed"))
      }
      if (length(failed_untar) > 0) {
        #message(paste("Retrying untar for tiles:", paste(failed_untar, collapse = ", ")))
        results[failed_untar] <- future.apply::future_lapply(failed_untar, function(i) {
          download_tile(i, timeout, outputDirTempFile, fileNames)
        })
        failed_untar <- which(sapply(results, function(x) x$status == "untar_failed"))
      }
    }

    list(results = results, failed_downloads = failed_downloads, failed_untar = failed_untar)
  }

  # Configuração de processamento paralelo ou sequencial
  if (ncores > 1) {
    future::plan(future::multisession, workers = ncores)
    print(paste("Downloading", nrow(fileNames), "tiles in parallel using", ncores, "cores"))
  } else {
    future::plan(future::sequential)
    print("Downloading tiles sequentially")
  }

  start_time <- Sys.time()

  # Use progressr para adicionar uma barra de progresso
  progressr::handlers("txtprogressbar")
  progressr::with_progress({
    p <- progressr::progressor(along = 1:nrow(fileNames))
    download_results <- suppressWarnings(
      download_tiles_with_retry(1:nrow(fileNames), retry, timeout, outputDirTempFile, fileNames)
    )
  })

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

  # Verifique se houve falhas no download
  if (length(download_results$failed_downloads) > 0) {
    warning("Some tiles failed to download: ", paste(download_results$failed_downloads, collapse = ", "))
  }
  if (length(download_results$failed_untar) > 0) {
    warning("Some tiles failed to untar: ", paste(download_results$failed_untar, collapse = ", "))
  }

  num_downloaded <- sum(sapply(download_results$results, function(x) x$status == "success"))
  print(paste("Downloaded", num_downloaded, "out of", nrow(fileNames), "tiles in", round(duration, 2), "minutes"))

  # Deletar ou não os tiles individuais
  fileToDelete <- list.files(outputDirTempFile, recursive = TRUE)
  demFiles <- list.files(outputDirTempFile, recursive = TRUE, pattern = "DEM.tif$", full.names = TRUE)

  if (keepInvidualTiles) {
    newDemFiles <- gsub(pattern = ".*DEM/",
                        replacement = paste0(outputDir, "/tilesCopernicusDem/"),
                        x = demFiles)
    tryCatch({
      dir.create(paste0(outputDir, "/tilesCopernicusDem"), showWarnings = FALSE)
      file.copy(from = demFiles, to = newDemFiles)
    }, error = function(e) {
      warning("Error copying individual tiles: ", e$message)
    })
  }

  # Mesclar DEM
  print("Merging tiles")

  finalDem <- tryCatch({
    d <- list.files(outputDirTempFile, full.names = TRUE, pattern = ".tif$", recursive = TRUE)
    if (length(d) > 1) {
      rsrc <- terra::sprc(d)
      terra::mosaic(rsrc)
    } else {
      terra::rast(d)
    }
  }, error = function(e) {
    stop("Error merging tiles: ", e$message)
  })

  finalDem <- tryCatch({
    terra::mask(terra::crop(finalDem, aoi), aoi)
  }, error = function(e) {
    stop("Error cropping and masking DEM: ", e$message)
  })

  # Aplicar multiplicador e converter para inteiro se especificado
  if (saveAsInteger) {
    finalDem <- tryCatch({
      finalDem <- finalDem * multiplier
      finalDem <- round(finalDem)
      finalDem
    }, error = function(e) {
      stop("Error converting DEM to integer: ", e$message)
    })
  }

  # Exportar DEM e deletar arquivos temporários
  tryCatch({
    if (saveAsInteger) {
      terra::writeRaster(finalDem, paste0(outputDir, "/", outputFileName), datatype = "INT2S", overwrite = TRUE)
    } else {
      terra::writeRaster(finalDem, paste0(outputDir, "/", outputFileName), overwrite = TRUE)
    }
    unlink(outputDirTempFile, force = TRUE, recursive = TRUE)
    gc()
  }, error = function(e) {
    stop("Error writing final DEM to file: ", e$message)
  })

  # Plotar o raster final se especificado
  if (showRaster) {  # Usando o novo nome do parâmetro
    tryCatch({
      terra::plot(finalDem)
    }, error = function(e) {
      warning("Error plotting final DEM: ", e$message)
    })
  }

  print("Job Complete")
  future::plan(future::sequential)
  return(finalDem)
}


download_tile <- function(i, timeout, outputDirTempFile, fileNames) {
  options(timeout = timeout)
  destfile <- paste(outputDirTempFile, "/", fileNames$nome[i], sep = "")
  tryCatch({
    # Download do arquivo
    utils::download.file(fileNames$text[i], destfile = destfile, mode = "wb", quiet = TRUE)

    # Extrair o nome do arquivo sem a extensão .tar
    nome <- gsub(fileNames$nome[i], pattern = ".tar", replacement = "")

    # Descompactar o arquivo e capturar avisos como erros
    result <- tryCatch({
      utils::untar(destfile, files = paste0(nome, "/DEM/", nome, "_DEM.tif"), exdir = paste0(outputDirTempFile, "/outputs"))
      list(status = "success")
    }, warning = function(w) {
      list(status = "untar_failed")
    }, error = function(e) {
      list(status = "untar_failed")
    })

    # Remover o arquivo .tar baixado
    unlink(destfile)

    return(result)
  }, error = function(e) {
    return(list(status = "download_failed"))
  })
}


if (getRversion() >= "2.15.1") utils::globalVariables(c("GeoCellID"))

