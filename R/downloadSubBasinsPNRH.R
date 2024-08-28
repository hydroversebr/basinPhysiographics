#' Download subbasins of Water Resource National Plan
#'
#' @param subBasinLevel numeric. subbasin level (1 or 2)
#' @param outputDir character. output directory.
#'
#' @return
#' An sf object containing subbasins of Water Resource National Plan
#'
#' @details
#' File are download to outputdir. Subbasins level 1 and 2 shapefiles are named as "GEOFT_PNRH_SUB1.shp" and "GEOFT_PNRH_SUB2.shp", respectively.
#'
#' @references
#'
#' Nível 1 de Sub-bacias do Plano Nacional de Recursos Hídricos
#' https://metadados.snirh.gov.br/geonetwork/srv/api/records/f50527b9-24ed-41d5-b063-b5acfb25e10d
#'
#'
#' Nível 2 de Sub-bacias do Plano Nacional de Recursos Hídricos
#' https://metadados.snirh.gov.br/geonetwork/srv/api/records/6141f37f-f15d-42e7-8495-ae9ddad0846f
#'
#'
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' require(sf)
#' area_of_interest <- st_read("./area_of_interest.shp")
#'
#' pnrh1 <- downloadSubBasinsPNRH(subBasinLevel = 1,
#'                              output = "./resultados")
#'
#' }

downloadSubBasinsPNRH = function(subBasinLevel = 1, outputDir){

  stopifnot(
    "`subBasinLevel` parameter must be numeric (1 or 2) indicating subbasin level" = subBasinLevel %in% c(1,2),
    "`outputDir` parameter must be character indicating subbasin output directory (i.e. './dados')" = is.character(outputDir))


  if (subBasinLevel == 1){

    download.file("https://metadados.snirh.gov.br:/geonetwork/srv/api/records/f50527b9-24ed-41d5-b063-b5acfb25e10d/attachments/GEOFT_PNRH_SUB1.zip", destfile = paste0(outputDir, "/GEOFT_PNRH_SUB1.zip"), mode = "wb")

    zip::unzip(zipfile = paste0(outputDir, "/GEOFT_PNRH_SUB1.zip"), exdir = outputDir)

    sub = sf::st_read(paste0(outputDir, "/GEOFT_PNRH_SUB1.shp"), quiet = T) %>%
      st_transform(4326)

    unlink(paste0(outputDir, "/GEOFT_PNRH_SUB1.zip"))

  } else {

    download.file("https://metadados.snirh.gov.br:/geonetwork/srv/api/records/6141f37f-f15d-42e7-8495-ae9ddad0846f/attachments/GEOFT_PNRH_SUB2.zip", destfile = paste0(outputDir, "/GEOFT_PNRH_SUB2.zip"), mode = "wb")

    zip::unzip(zipfile = paste0(outputDir, "/GEOFT_PNRH_SUB2.zip"), exdir = outputDir)

    sub = sf::st_read(paste0(outputDir, "/GEOFT_PNRH_SUB2.shp"), quiet = T) %>%
      st_transform(4326)

    unlink(paste0(outputDir, "/GEOFT_PNRH_SUB2.zip"))

  }

  return(sub)

}
