#' Title
#'
#' @param data
#' @param filename
#' @param train_size
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
train_test_split <- function(data,
                             filename,
                             train_size = .85,
                             directory = 'data') {
  set.seed(123)
  
  train_size <- floor(0.7 * nrow(data))
  train_ind <- sample(seq_len(nrow(data)), size = train_size)
  
  train <- data[train_ind,]
  test <- data[-train_ind,]
  
  print(paste('|train_set| =', nrow(train), ' |test_set| = ', nrow(test)))
  
  return(list(train, test))
}


#' Title
#'
#' @param model
#' @param fname
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
save_model <- function(model, fname, directory = 'models') {
  fname <-
    paste(fname, '_', Sys.Date() |> format(format = "%Y%m"), '.rds', sep = '')
  fname <- file.path(directory, fname)
  saveRDS(model, file = fname)
  print(paste('Modelo salvo em:', fname))
}


#' Title
#'
#' @param data
#' @param fmla
#'
#' @return
#' @export
#'
#' @examples
train_model <- function(data, fmla) {
  ti <- Sys.time()
  modelo <- mgcv::bam(fmla, data = data)
  print(paste('Tempo descorrido:', Sys.time() - ti))
  return(modelo)
}


#' Title
#'
#' @param model
#' @param data_pred
#' @param confianca
#'
#' @return
#' @export
#'
#' @examples
predict_model <- function(model, data_pred, confianca = 0.975) {
  pred <- predict.gam(model, newdata = data_pred, se.fit = T)
  
  valor <- as.numeric(pred$fit)
  se <- as.numeric(pred$se.fit)
  z <- qnorm(confianca)
  maximo <- valor + (se * z)
  minimo <- valor - (se * z)
  
  df_pred <- data.frame(
    minimo = minimo,
    media = valor,
    maximo = maximo,
    se = se
  )
  
  return(df_pred)
}


#' Title
#'
#' @param ytrue
#' @param ypred
#'
#' @return
#' @export
#'
#' @examples
calc_erro_abs <- function(ytrue, ypred, islog = T) {
  if(islog){ytrue <- ytrue |> exp()}
  abs_diff <- abs(ytrue - ypred)
  error_pct <- c(abs_diff / ytrue * 100) |> round(3)
  error_quants <-
    quantile(error_pct,
             seq(0, 1, .01),
             na.rm = TRUE) |> as.data.frame()
  
  return(list(error_pct, error_quants))
}


#' Title
#'
#' @param model
#' @param test_set
#' @param ytrue
#' @param fname
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
report_model <- function(model,
                         test_set,
                         ytrue,
                         fname,
                         directory = 'reports') {
  fname <-
    paste(fname, '_', Sys.Date() |> format(format = "%Y%m"), '.txt', sep = '')
  fname <- file.path(directory, fname)
  
  pred <- predict_model(model, test_set)
  avm <- exp(pred$media)
  avm_max <- exp(pred$maximo)
  avm_min <- exp(pred$minimo)
  avm_se <- pred$se
  
  q_err <- calc_erro_abs(ytrue, avm)
  error_quants <- q_err[[2]]
  error_pct <- q_err[[1]]
  
  df_pred <- data.frame(
    avm_min = avm_min,
    avm = avm,
    avm_max = avm_max,
    avm_se = avm_se,
    abs_err_pct = error_pct
  )
  
  pdf(file = gsub('.txt', '.pdf', fname))
  sink(file = fname)
  print(gam.check(model))
  cat('--\n--')
  print(summary(model))
  cat('--\n--')
  print(error_quants)
  cat('--\n--')
  sink(file = NULL)
  plot.gam(model)
  dev.off()
  
  print(paste('Relatorio do modelo salvo em:', fname))
  
  return(df_pred)
}


#' Title
#'
#' @param data
#' @param fmla
#' @param fname
#' @param test_split
#'
#' @return
#' @export
#'
#' @examples
full_pipeline <- function(data, fmla, fname, test_split = F) {
  if (test_split) {
    df_split <- train_test_split(data)
    df_train <- df_split[[1]]
    df_pred <- df_split[[2]]
    modelo <- train_model(df_pred, fmla)
    save_model(modelo, paste0(fname, '_traintest'))
    pred <- report_model(modelo,
                         df_pred,
                         df_pred$ln_ticket,
                         paste0(fname, '_traintest'))
    
    df_pred$avm_min <- pred$avm_min 
    df_pred$avm <- pred$avm
    df_pred$avm_max <- pred$avm_max
  }
  else{
    modelo <- train_model(data, fmla)
    save_model(modelo, fname)
    df_pred <- report_model(modelo,
                            data,
                            data$ln_ticket,
                            fname)
  }
  
  return(list(modelo, df_pred))
  
}
