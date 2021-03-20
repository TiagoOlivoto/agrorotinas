#' Importar, exportar e mostrar imagens
#'
#'  Ferramentas basicas para trabalhar com imagens .png, .jpeg, .jpg, .tiff (ou
#'  .tif, .TIFF, .TIF)
#'  * `importar_imagem()` importa uma imagem.
#'  * `mostrar_imagem()` mostra uma imagem.
#'  * `salvar_imagem()` salva uma imagem.
#'  * `image_to_mat()` converte uma imagem para uma lista, contendo tres
#'  matrizes (RGB) e um data frame com tres colunas (RGB).
#'  * `image_extension()` obtem a extensao de uma imagem.
#'  * `image_name()` obtem o nome de uma imagem.
#' @name utils_imagem
#' @param imagem A imagem.
#' * Para `mostrar_imagem()`, Qualquer objeto R que pode ser coagido a um objeto
#' raster.
#' * Para `importar_imagem()`, uma string especificando o caminho para a imagem.
#' * Para `salvar_imagem()`, um objeto bidimensional ou tridimensional (matriz,
#' quadro de dados ou array).
#' @param nome Uma string especificando o nome da imagem a ser salva.
#' @param randomize Aleatorizar as linhas apos conversao de matriz para data
#'   frame? Padrao `TRUE`.
#' @param nrows Numero de linhas a ser selecionada no data frame criado.
#' @param ... Argumentos alternativos a serem passados para outras funcoes:
#' * Para `mostrar_imagem()`, argumentos passados para [grid::grid.raster()].
#' * Para `importar_imagem()`, argumentos passados para [readPNG()],
#' [readJPEG()] e [readTIFF()].
#' * Para `salvar_imagem()`, argumentos passados para [writePNG()],
#' [writeJPEG()] e [writeTIFF()].
#' @md
#' @import jpeg
#' @import tiff
#' @import png
#' @export
#' @examples
#' img1 <- system.file("tmp_images", "img1.png", package = "agrorotinas")
#' a <- importar_imagem(img1)
#' mostrar_imagem(a)
mostrar_imagem <- function(imagem, ...){
  grid.raster(imagem, ...)
}
#' @export
#' @name utils_imagem
importar_imagem <- function(imagem, ...){
  tentar_imagem =
    inherits(tryCatch(normalizePath(imagem, mustWork = T), error = function(e) e), "error")
  if (!inherits(imagem, "character") || tentar_imagem == T) {
    stop("Caminho para a imagem invalido ou a imagem nao existe")
  }
  flag_type = image_extension(imagem)
  if (length(flag_type) == 0) {
    stop("Imagem invalida")
  }
  if (flag_type == "png") {
    img = png::readPNG(imagem, ...)
  }
  else if (flag_type == "jpg" || flag_type == "jpeg") {
    img = jpeg::readJPEG(imagem, ...)
  }
  else if (flag_type %in% c("tiff", "tif", "TIFF", "TIF")) {
    img = tiff::readTIFF(imagem, ...)
  }
  else {
    stop("os tipos de imagem compativeis sao .png, .jpeg, .jpg, .tiff (ou .tif, .TIFF, .TIF)")
  }
  return(img)
}
#' @export
#' @name utils_imagem
salvar_imagem <- function(imagem, nome, ...){
  if (inherits(imagem, "data.frame")) {
    imagem = as.matrix(imagem)
  }
  if (!inherits(imagem, c("matrix", "array"))) {
    stop("Os dados de imagem suportados sao matriz, quadro de dados, arrays.")
  }
  if (!inherits(nome, "character"))
    stop("O 'nome' deve ser uma cadeia de caracteres. Por exemplo, '/home/my_image.png'")
  flag_type = image_extension(nome)
  if (flag_type == "png") {
    png::writePNG(imagem, target = nome, ...)
  }
  else if (flag_type == "jpg" || flag_type == "jpeg") {
    jpeg::writeJPEG(imagem, target = nome, ...)
  }
  else if (flag_type %in% c("tiff", "tif", "TIFF", "TIF")) {
    tiff::writeTIFF(imagem, where = nome, ...)
  }
  else {
    stop("Os tipos de imagem compativeis sao .png, .jpeg, .jpg, .tiff (ou .tif, .TIFF, .TIF)")
  }
}
#' @export
#' @name utils_imagem
image_to_mat <- function(imagem, randomize =  TRUE, nrows = 5000){
  d <- match.call()
  ncols <- ncol(imagem[,,1])
  im <- cbind(c(imagem[,,1]), c(imagem[,,2]), c(imagem[,,3]))
  df_in <- data.frame(im) %>% add_cols(code = paste(d[["imagem"]]), .before = 1)
  df_man <- df_in
  if(randomize == TRUE){
    df_man <- df_man[sample(1:nrow(df_man)),]
  }
  if(!missing(nrows)){
    df_man <- df_man[1:nrows, ]
  }
  colnames(df_man) <- colnames(df_in) <-  c("CODE", "R", "G", "B")
  rbg <- list(R = matrix(im[, 1], ncol = ncols),
              G = matrix(im[, 2], ncol = ncols),
              B = matrix(im[, 3], ncol = ncols),
              df_man = data.frame(df_man),
              df_in = df_in)
  return(rbg)
}
#' @export
#' @name utils_imagem
image_extension <- function(imagem){
  ex <- strsplit(basename(imagem), split="\\.")[[1]]
  return(ex[-1])
}
#' @export
#' @name utils_imagem
image_name <- function(imagem){
  ex <- strsplit(basename(imagem), split="\\.")[[1]]
  return(ex[1])
}
