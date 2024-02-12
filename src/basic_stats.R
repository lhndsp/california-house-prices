library(ggplot2)
library(patchwork)
library(moments)


test_norm <- function(dados){
  # Calcular a estatística de teste de D'Agostino-Pearson
  estatistica_teste <- sum((skewness(dados))^2, (kurtosis(dados) - 3)^2 / 4)
  
  # Calcular o valor-p usando uma distribuição qui-quadrado
  graus_de_liberdade <- 2
  valor_p <- 1 - pchisq(estatistica_teste, df = graus_de_liberdade)
  
  # Exibir o resultado do teste
  return(c(estatistica_teste, valor_p))
}

#' Plotar estatisticas de uma variavel
#'
#' @param x variavel
#' @param x_nm Titulo do grafico
#'
#' @return
#' @export
#'
#' @examples plot_descritiva(mtcars$mpg, 'Miles/(US) gallon')
plot_descritiva <- function(x, x_nm=NA){
  
  x_nm <- ifelse(is.na(x_nm), deparse(substitute(x)), x_nm)
  
  q <- seq(.01, 1, .01)
  x_quantiles <- quantile(x, probs = q, na.rm = T)
  x_iqr <- IQR(x)
  x_mean <- mean(x)
  x_kurtosis <- kurtosis(x)
  x_std <- sd(x)
  x_cv <- x_std/x_mean
  x_skew <- skewness(x)
  x_outliers_sup <- x_quantiles[75] + (1.5 * x_iqr)
  x_outliers_inf <- x_quantiles[25] - (1.5 * x_iqr)
  x_outliers_sup <- ifelse(any(x >= x_outliers_sup), x_outliers_sup, NA)
  x_outliers_inf <- ifelse(any(x <= x_outliers_inf), x_outliers_inf, NA)
  
  p_box <- ggplot() + 
    geom_boxplot(aes(x=x)) + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    labs(x = "", 
         y = "",
         title = sprintf("Medidas descritivas: %s", x_nm)
    )
  
  texto <- 
    sprintf(
      "
        Min = %.3f
        Média = %.3f
        P10 = %.3f
        P25 = %.3f
        P50 = %.3f
        P75 = %.3f
        P90 = %.3f
        Max = %.3f
        Skew = %.3f
        Kurtosis = %.3f
        Std = %.3f
        CV = %.3f
        Outliers+ = %.3f
        Outliers- = %.3f
      ", 
  min(x), 
  x_mean, 
  x_quantiles[10], 
  x_quantiles[25], 
  x_quantiles[50], 
  x_quantiles[75], 
  x_quantiles[90],
  max(x), 
  x_skew, 
  x_kurtosis, 
  x_std, 
  x_cv,
  x_outliers_sup,
  x_outliers_inf)
  
  hist_bins = round(sqrt(length(x)))
  
  p_hist <- ggplot() + 
    geom_histogram(aes(x), bins = hist_bins) +
    geom_line() +
    sapply(list(
      list(x_mean, '#ffc500')
    ), 
    function(xint) geom_vline(aes(xintercept = xint[[1]]),  
                              colour=xint[[2]])
    ) + 
    labs(x = '',
         y = "Obs.",
         tag = texto
    ) +
    theme(plot.title = element_text(hjust = 0.5, size=12),
          plot.subtitle = element_text(hjust=0.5, size=12),
          plot.margin = margin(0, 8, 0, 0, "lines"),
          plot.tag.position = c(1,.6),
          plot.tag = element_text(hjust =0, size=9))
  
  if (!is.na(x_outliers_sup)){
    p_hist <- p_hist +
      geom_rect(aes(xmin=x_outliers_sup, xmax=max(x), 
                    ymin=0, ymax=Inf), 
                alpha=.1, fill='#ff0000')
  }
  
  if (!is.na(x_outliers_inf)){
    p_hist <- p_hist +
      geom_rect(aes(xmin=min(x), xmax=x_outliers_inf, 
                    ymin=0, ymax=Inf), 
                alpha=.1, fill='#ff0000')
  }
  
  p_qq <- ggplot() + 
    geom_point(aes(x=q, y=x_quantiles), size = .2) + 
    labs(x = '', 
         y = '') + 
    scale_x_continuous(breaks = seq(0, 1, .1))
  
  p_qq_zoom_inf <- 
    ggplot() +
    geom_point(aes(x = seq(0, .1, .001),
                   y = quantile(x, seq(0, .1, .001)), na.rm = T), size = .2)+ 
    labs(x = '', 
         y = '') + 
    scale_x_continuous(breaks = seq(0, .1, .05))
  
  p_qq_zoom_sup <- 
    ggplot() +
    geom_point(aes(x = seq(.9, 1, .001),
                   y = quantile(x, seq(.9, 1, .001)), na.rm = T), size = .2)+ 
    labs(x = '', 
         y = '') + 
    scale_x_continuous(breaks = seq(.9, 1, .05))

  
  layout <- 
    "
  AAAAAA
  AAAAAA
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  BBBBBB
  CCCCCC
  CCCCCC
  CCCCCC
  DDDEEE
  DDDEEE
  DDDEEE
  "
  
  final_plot <- (
    p_box /
      p_hist /
      p_qq /
      p_qq_zoom_inf /
      p_qq_zoom_sup
  ) + 
    plot_layout(design = layout, widths = c(4, -1.1 ,4.5)) 
  
  return(final_plot)
}
