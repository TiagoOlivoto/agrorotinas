#' Calcula a percentagem de area afetada
#'
#' @param im_original A imagem a ser analisada
#' @param im_sadia Uma paleta de cores das areas sadias
#' @param im_sintoma Uma paleta de cores das areas com sintomas
#' @param im_fundo Uma paleta de cores das areas com sintomas
#' @param cor_fundo Cor do fundo apos o processamento da imagem
#' @param show_canal Qual canal mostrar (1, 2, ou 3)
#' @param randomize Aleatorizar as linhas para treinar o modelo?
#' @param nrows O numero de linhas a ser utilziado no treinamento do modelo.
#' @param salva_image Salvar a imagem apos processamento?
#' @param dir_original Diretorio que contem as imagens originais
#' @param dir_processada Diretorio que contem as imagens processadas
#' @return Um data frame com os resutlados para cada imagem.
#' @export
#' @importFrom stats binomial glm predict
#' @import OpenImageR
#' @import metan
#' @importFrom grid grid.raster
area_afetada <- function(im_original,
                         im_sadia,
                         im_sintoma,
                         im_fundo,
                         cor_fundo = "branco",
                         show_canal = 2,
                         randomize = TRUE,
                         nrows = 20000,
                         salva_image = FALSE,
                         dir_original = NULL,
                         dir_processada = NULL){
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
  ################ funções auxiliares #################################
  correct_image =function(img2,perc,imagem=T,individual=F){
    if(imagem==T){t=img2@.Data[,,1]}
    if(imagem==F){t=img2}

    if(individual==F){
      n=round(perc*min(c(ncol(t),nrow(t))),0)

      p1=function(t){
        t2=t
        for( i in 2:(nrow(t)-n-1)){
          for(j in 1:ncol(t)){
            if(t[i,j]==1){
              if(t[i-1,j]==0){

                a=0
                while(a<n){
                  a=a+1

                  if(sum(t[i:(i+a),j]==1)<a){t2[i:(i+a),j]=0;a=n}

                }

              }
            }
          }
        }
        return(t2)
      }

      Pp=p1(t)
      Pp=p1(t(Pp))
      return(t(Pp))
    }

    if(individual==T){

      t2=Contorno(t,imagem = F)
      display(t2)
      m=cbind(expand.grid(1:nrow(t2),1:ncol(t2)),c(t2))
      m=as.matrix(m[m[,3]<1,])

      ind=unique(m[,1])
      for(y in 1:length(ind)){
        t2[ind[y],min(m[m[,1]==ind[y],2]):max(m[m[,1]==ind[y],2])]=0
      }


      return(t2)

    }
  }
  image_to_mat <- function(image, randomize, nrows){
    d <- match.call()
    ncols <- ncol(image[,,1])
    im <- cbind(c(image[,,1]), c(image[,,2]), c(image[,,3]))
    df_in <- data.frame(im) %>% add_cols(code = paste(d[["image"]]), .before = 1)
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
  image_extension <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }
  image_name <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[1])
  }
  #####################################################################
  all_files <- sapply(list.files(diretorio_original), image_name)
  names <- c(im_original, im_sadia, im_sintoma, im_fundo)
  if(!all(names %in% all_files)){
    stop(" '", paste(names[which(!names %in% all_files)], collapse = ", "), "' not found in the directory" , call. = FALSE)
  }
  imag <- list.files(diretorio_original, pattern = im_original)
  name <- image_name(imag)
  extens <- image_extension(imag)
  sadio <- list.files(diretorio_original, pattern = im_sadia)
  sintoma <- list.files(diretorio_original, pattern = im_sintoma)
  fundo <- list.files(diretorio_original, pattern = im_fundo)
  im_original <- readImage(paste(diretorio_original, "/", name, ".", extens, sep = ""))
  im_sadia <- readImage(paste(diretorio_original, "/", image_name(sadio), ".", image_extension(sadio), sep = ""))
  im_sintoma <- readImage(paste(diretorio_original, "/", image_name(sintoma), ".", image_extension(sintoma), sep = ""))
  im_fundo <- readImage(paste(diretorio_original, "/", image_name(fundo), ".", image_extension(fundo), sep = ""))
  original <- image_to_mat(im_original, randomize = randomize, nrows = nrows)
  sadio <- image_to_mat(im_sadia, randomize = randomize, nrows = nrows)
  sintoma <- image_to_mat(im_sintoma, randomize = randomize, nrows = nrows)
  fundo <- image_to_mat(im_fundo, randomize = randomize, nrows = nrows)
  # separar o fundo
  fundo_resto <-
    rbind(sadio$df_man,
          sintoma$df_man,
          fundo$df_man) %>%
    mutate(Y = ifelse(CODE == "im_fundo", 0, 1)) %>%
    remove_cols(CODE)
  modelo1 <- suppressWarnings(glm(Y ~ R + G + B, family = binomial("logit"), data = fundo_resto))
  pred1 <- predict(modelo1, newdata = original$df_in, type="response") %>% round(0)
  plant_fundo <- matrix(pred1, ncol = ncol(original$R))
  plant_fundo <- correct_image(plant_fundo==0, imagem = FALSE, perc = 0.009)
  # separar sadio - sintoma
  sadio_sintoma <-
    rbind(sadio$df_man,
          sintoma$df_man) %>%
    mutate(Y = ifelse(CODE == "im_sadia", 1, 0)) %>%
    remove_cols(CODE)
  modelo2 <- suppressWarnings(glm(Y ~ R + G + B, family = binomial("logit"), data = sadio_sintoma))
  # isolar planta
  ID <- c(plant_fundo==0)
  mat3 <- original$df_in[ID,]
  pred2 <- predict(modelo2, newdata = original$df_in[ID,], type="response") %>% round(0)
  # colocar sintoma na imagem origianl
  cor_fundo <- ifelse(cor_fundo == "branco", 1, 0)
  im2 <- im_original
  im2[,,show_canal][ID] <- pred2
  im2[,,1][!ID] <- cor_fundo
  im2[,,2][!ID] <- cor_fundo
  im2[,,3][!ID] <- cor_fundo

  if(dir.exists(diretorio_processada) == FALSE){
    dir.create(diretorio_processada)
  }
  if(salva_image == TRUE){
    writeImage(im2, file_name = paste(diretorio_processada, "/proc_", name, ".", extens, collapse = "", sep = ""))
  }
  sadio <- mean(pred2)
  afetado <- 1 - sadio
  results <- data.frame(sadio = sadio,
                        afetado = afetado)
  return(results)
}
