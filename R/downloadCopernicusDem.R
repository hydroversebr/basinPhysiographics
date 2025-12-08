#' Baixar e processar MDE para uma área de interesse
#'
#' @param roi Região de interesse. Pode ser:
#'   - Caminho para shapefile (ex: "limites/area.shp")
#'   - Código de estado brasileiro (ex: "ES", "MG")
#'   - Objeto sf ou SpatVector
#' @param dir_saida Diretório onde os dados serão salvos
#' @param n_tentativas Número de tentativas de download
#' @param n_cores Número de cores para processamento paralelo
#' @param sobrescrever Sobrescrever arquivos existentes?
#' @param recortar_roi Recortar o MDE final pelos limites exatos da ROI?
#'
#' @return Caminho para o arquivo MDE final
#'
#' @examples
#' # Usando código de estado
#' processar_mde("ES", "../mde")
#'
#' # Usando shapefile
#' processar_mde("limites/bacia.shp", "../mde")
#'
#' # Usando objeto sf
#' area_sf <- sf::st_read("minha_area.shp")
#' processar_mde(area_sf, "../mde")
#'
#' @export

downloadCopernicusDem <- function(roi,
                                  dir_saida = "../mde",
                                  n_tentativas = 50,
                                  n_cores = 18,
                                  sobrescrever = FALSE,
                                  recortar_roi = TRUE) {

  # Validações de entrada ------------------------------------------------

  if (missing(roi) || is.null(roi)) {
    stop("Parâmetro 'roi' é obrigatório")
  }

  if (!is.numeric(n_tentativas) || n_tentativas < 1) {
    stop("n_tentativas deve ser um número inteiro positivo")
  }

  if (!is.numeric(n_cores) || n_cores < 1) {
    stop("n_cores deve ser um número inteiro positivo")
  }

  # Criar diretório de saída se não existir
  if (!dir.exists(dir_saida)) {
    message(sprintf("Criando diretório: %s", dir_saida))
    dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)
  } else {
    message(sprintf("Usando diretório existente: %s", dir_saida))
  }

  # 1. Carregar ROI ------------------------------------------------------

  message("\n[1/5] Carregando região de interesse...")

  roi_sf <- load_roi(roi)
  nome_area <- attr(roi_sf, "nome_area")

  # Criar nome de arquivo seguro
  nome_arquivo_seguro <- gsub("[^a-zA-Z0-9_-]", "_", nome_area)

  # Verificar se arquivo final já existe
  arquivo_final <- file.path(dir_saida, sprintf("%s_mde_merge.tif", nome_arquivo_seguro))

  if (file.exists(arquivo_final) && !sobrescrever) {
    message(sprintf("Arquivo já existe: %s", arquivo_final))
    resposta <- readline("Deseja sobrescrever? (s/n): ")
    if (tolower(resposta) != "s") {
      message("Operação cancelada pelo usuário")
      return(invisible(arquivo_final))
    }
  }

  # 2. Criar cubo de dados -----------------------------------------------

  message("\n[2/5] Criando cubo de dados do Copernicus DEM...")

  cubo <- tryCatch({
    sits::sits_cube(
      source     = "MPC",
      collection = "COP-DEM-GLO-30",
      bands      = "ELEVATION",
      roi        = roi_sf
    )
  }, error = function(e) {
    stop(sprintf("Erro ao criar cubo: %s", e$message))
  })

  if (is.null(cubo)) {
    stop("Falha ao criar cubo de dados")
  }

  message(sprintf("Cubo criado com sucesso: %d tile(s) encontrado(s)", nrow(cubo)))

  # 3. Baixar tiles ------------------------------------------------------

  message(sprintf("\n[3/5] Baixando tiles (tentativas: %d, cores: %d)...",
                  n_tentativas, n_cores))
  message("Isso pode demorar dependendo do tamanho da área e velocidade da internet...")

  download_sucesso <- tryCatch({
    sits::sits_cube_copy(
      cube       = cubo,
      n_tries    = n_tentativas,
      multicores = n_cores,
      roi        = roi_sf,
      output_dir = dir_saida
    )
    TRUE
  }, error = function(e) {
    warning(sprintf("Erro durante download: %s", e$message))
    FALSE
  })

  if (!download_sucesso) {
    stop("Falha no download dos tiles")
  }

  # 4. Processar e mesclar tiles -----------------------------------------

  message("\n[4/5] Processando e mesclando tiles...")

  # Listar arquivos .tif (excluir arquivo final se já existir)
  tiles_mde <- list.files(dir_saida, pattern = "\\.tif$", full.names = TRUE)
  tiles_mde <- tiles_mde[!grepl("_mde_merge\\.tif$", tiles_mde)]

  if (length(tiles_mde) == 0) {
    stop(sprintf("Nenhum arquivo .tif encontrado em: %s", dir_saida))
  }

  message(sprintf("Encontrados %d arquivo(s) .tif", length(tiles_mde)))

  # Verificar se há apenas um tile
  if (length(tiles_mde) == 1) {
    message("Apenas um tile encontrado, carregando diretamente...")
    mde_mesclado <- terra::rast(tiles_mde[1])

  } else {
    message("Mesclando múltiplos tiles...")

    # Criar coleção espacial de rasters
    tiles_sprc <- tryCatch({
      terra::sprc(tiles_mde)
    }, error = function(e) {
      stop(sprintf("Erro ao criar coleção de rasters: %s", e$message))
    })

    # Mesclar tiles
    mde_mesclado <- tryCatch({
      terra::merge(tiles_sprc)
    }, error = function(e) {
      stop(sprintf("Erro ao mesclar tiles: %s", e$message))
    })
  }

  # 5. Recortar pela ROI (opcional) --------------------------------------

  if (recortar_roi) {
    message("\n[5/5] Recortando MDE pelos limites da ROI...")

    # Converter sf para SpatVector
    roi_vect <- terra::vect(roi_sf)

    # Recortar e mascarar
    mde_final <- tryCatch({
      terra::crop(mde_mesclado, roi_vect)
      terra::mask(mde_mesclado, roi_vect)
    }, error = function(e) {
      warning(sprintf("Erro ao recortar MDE: %s. Usando MDE completo.", e$message))
      mde_mesclado
    })

  } else {
    message("\n[5/5] Pulando recorte (recortar_roi = FALSE)")
    mde_final <- mde_mesclado
  }

  # Salvar resultado -----------------------------------------------------

  message(sprintf("Salvando MDE final em: %s", arquivo_final))

  terra::writeRaster(
    mde_final,
    arquivo_final,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES")
  )

  # Resumo final ---------------------------------------------------------

  message("\n========================================")
  message("PROCESSAMENTO CONCLUÍDO COM SUCESSO!")
  message("========================================")
  message(sprintf("Área: %s", nome_area))
  message(sprintf("Arquivo final: %s", arquivo_final))
  message(sprintf("Dimensões: %d x %d pixels",
                  terra::nrow(mde_final),
                  terra::ncol(mde_final)))
  message(sprintf("Resolução: %.6f x %.6f graus",
                  terra::res(mde_final)[1],
                  terra::res(mde_final)[2]))

  # Calcular resolução em metros no centro da área
  centro_lat <- mean(terra::ext(mde_final)[3:4])
  res_m_x <- terra::res(mde_final)[1] * 111320 * cos(centro_lat * pi/180)
  res_m_y <- terra::res(mde_final)[2] * 111320
  message(sprintf("Resolução aproximada: %.1f x %.1f metros", res_m_x, res_m_y))

  message(sprintf("Extensão: %.6f, %.6f, %.6f, %.6f (xmin, xmax, ymin, ymax)",
                  terra::ext(mde_final)[1],
                  terra::ext(mde_final)[2],
                  terra::ext(mde_final)[3],
                  terra::ext(mde_final)[4]))

  elevacao_stats <- terra::global(mde_final, c("min", "max", "mean"), na.rm = TRUE)
  message(sprintf("Elevação mín: %.1f m", elevacao_stats[1, "min"]))
  message(sprintf("Elevação máx: %.1f m", elevacao_stats[1, "max"]))
  message(sprintf("Elevação média: %.1f m", elevacao_stats[1, "mean"]))

  # Tamanho do arquivo
  tamanho_mb <- file.size(arquivo_final) / 1024^2
  message(sprintf("Tamanho do arquivo: %.2f MB", tamanho_mb))

  message("========================================\n")




  limpar_tiles <- function(dir_saida, manter_merge = TRUE) {

    if (!dir.exists(dir_saida)) {
      warning(sprintf("Diretório não existe: %s", dir_saida))
      return(invisible(NULL))
    }

    todos_arquivos <- list.files(dir_saida, pattern = "\\.tif$", full.names = TRUE)
    arquivos_merge <- list.files(dir_saida, pattern = "_mde_merge\\.tif$", full.names = TRUE)

    if (manter_merge && length(arquivos_merge) > 0) {
      arquivos_deletar <- setdiff(todos_arquivos, arquivos_merge)
    } else {
      arquivos_deletar <- todos_arquivos
    }

    if (length(arquivos_deletar) == 0) {
      message("Nenhum arquivo para deletar")
      return(invisible(NULL))
    }

    message(sprintf("Deletando %d arquivo(s)...", length(arquivos_deletar)))

    sucesso <- file.remove(arquivos_deletar)

    message(sprintf("Deletados: %d arquivo(s)", sum(sucesso)))

    return(invisible(sum(sucesso)))
  }


  limpar_tiles(dir_saida, manter_merge = TRUE)

  return(invisible(arquivo_final))


}
