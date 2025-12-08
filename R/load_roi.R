# Função auxiliar para carregar ROI --------------------------------------

#' Carregar região de interesse (ROI)
#'
#' @param roi_input Pode ser:
#'   - Caminho para shapefile (character)
#'   - Objeto sf ou SpatVector
#'   - Código de estado brasileiro (2 caracteres)
#'
#' @return Objeto sf com a geometria da ROI
#' @importFrom sf st_transform
#'
#' @export
load_roi <- function(roi_input) {

  # Caso 1: Caminho para arquivo shapefile
  if (is.character(roi_input) && length(roi_input) == 1) {

    # Verificar se é código de estado (2 caracteres) ou caminho de arquivo
    if (nchar(roi_input) == 2 && !grepl("\\.", roi_input)) {
      # É um código de estado
      message(sprintf("Carregando estado: %s", roi_input))

      if (!requireNamespace("geobr", quietly = TRUE)) {
        stop("Pacote 'geobr' necessário para carregar estados. Instale com: install.packages('geobr')")
      }

      roi <- tryCatch({
        geobr::read_state(code_state = roi_input, showProgress = FALSE)
      }, error = function(e) {
        stop(sprintf("Erro ao carregar estado '%s': %s", roi_input, e$message))
      })

      nome_area <- unique(roi$name_state)

    } else {
      # É um caminho de arquivo
      message(sprintf("Carregando shapefile: %s", roi_input))

      # Verificar se arquivo existe
      if (!file.exists(roi_input)) {
        # Tentar adicionar extensão .shp
        if (file.exists(paste0(roi_input, ".shp"))) {
          roi_input <- paste0(roi_input, ".shp")
        } else {
          stop(sprintf("Arquivo não encontrado: %s", roi_input))
        }
      }

      roi <- tryCatch({
        sf::st_read(roi_input, quiet = TRUE)
      }, error = function(e) {
        stop(sprintf("Erro ao ler shapefile: %s", e$message))
      })

      nome_area <- tools::file_path_sans_ext(basename(roi_input))
    }

    # Caso 2: Objeto sf
  } else if (inherits(roi_input, "sf")) {
    message("Usando objeto sf fornecido")
    roi <- roi_input
    nome_area <- "area_personalizada"

    # Caso 3: Objeto SpatVector (terra)
  } else if (inherits(roi_input, "SpatVector")) {
    message("Convertendo SpatVector para sf")
    roi <- sf::st_as_sf(roi_input)
    nome_area <- "area_personalizada"

  } else {
    stop("roi_input deve ser um caminho para shapefile, código de estado (2 caracteres), objeto sf ou SpatVector")
  }

  # Validações básicas
  if (is.null(roi) || nrow(roi) == 0) {
    stop("ROI vazia ou inválida")
  }

  # Garantir que tem CRS definido
  if (is.na(sf::st_crs(roi))) {
    warning("ROI sem sistema de coordenadas definido. Assumindo WGS84 (EPSG:4326)")
    sf::st_crs(roi) <- 4326
  }

  # Converter para WGS84 se necessário (SITS requer lat/lon)
  if (sf::st_crs(roi)$epsg != 4326) {
    message(sprintf("Convertendo CRS de EPSG:%s para EPSG:4326",
                    sf::st_crs(roi)$epsg))
    roi <- sf::st_transform(roi, 4326)
  }

  # Calcular área em km²
  roi_area <- sf::st_area(roi) / 1e6  # converter m² para km²
  total_area <- sum(as.numeric(roi_area))

  # Obter bbox
  bbox <- sf::st_bbox(roi)

  message(sprintf("ROI carregada: %s", nome_area))
  message(sprintf("Número de feições: %d", nrow(roi)))
  message(sprintf("Área total: %.2f km²", total_area))
  message(sprintf("BBox: %.4f, %.4f, %.4f, %.4f (xmin, ymin, xmax, ymax)",
                  bbox[1], bbox[2], bbox[3], bbox[4]))
  message(sprintf("CRS: EPSG:%s", sf::st_crs(roi)$epsg))

  attr(roi, "nome_area") <- nome_area

  return(roi)
}
