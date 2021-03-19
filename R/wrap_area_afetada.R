#' Uma funcao wrapper para a funcao area_afetada
#'
#' @param pattern_files Um padrao de nome de arquivo
#' @param im_sadia Uma paleta de cores das areas sadias
#' @param im_sintoma Uma paleta de cores das areas com sintomas
#' @param im_fundo Uma paleta de cores das areas com sintomas
#' @param cor_fundo Cor do fundo apos o processamento da imagem
#' @param ... argumentos a serem passados para a funcao [area_afetada()].
#' @md
#' @return Um data frame
#' @export
#' @importFrom purrr map_chr
wrap_area_afetada <- function(pattern_files,
                              im_sadia,
                              im_sintoma,
                              im_fundo,
                              ...){
  image_extension <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
  image_name <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[1])
  }
  if(is.null(dir_original)){
    diretorio_original <- paste("./originais", sep = "")
  } else{
    diretorio_original <- paste("./", dir_original, sep = "")
  }
  if(is.null(dir_processada)){
    diretorio_processada <- paste("./processadas", sep = "")
  } else{
    diretorio_processada <- paste("./", dir_processada, sep = "")
  }
  plants <- list.files(pattern = pattern_files, diretorio_original)
  extensions <- map_chr(plants, image_extension)
  names_plant <- map_chr(plants, image_name)
  if(!all(extensions %in% c("png", "jpeg", "jpg", "tiff"))){
    stop("Somente imagens de extensao .png, .jpeg, .jpg, .tiff sao permitidas")
  }
  results <- list()
  pb <- progress(max = length(plants))
  for (i in 1:length(plants)) {
    run_progress(pb, actual = i,
                 text = paste("Processando imagem", names_plant[i]))
    results[[i]] <-   area_afetada(im_original  = names_plant[i],
                                   im_sadia  = im_sadia,
                                   im_sintoma  = im_sintoma,
                                   im_fundo  = im_fundo, ...)
  }
  rbind_fill_id(results) %>%
    add_cols(amostra = names_plant, .before = 1) %>%
    return()
}
