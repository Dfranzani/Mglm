#' @encoding UTF-8
#' @title Curva de ROC
#'
#' @param realestrain valores reales de entrenamiento.
#' @param probabilidadestrain probabilidades de la predicción del entrenamiento.
#' @param realestest valores reales de prueba.
#' @param probabilidadestest probabilidades de la predicción de la prueba.
#'
#' @return Curva de ROC y sus respectivos AUC.
#' @export
model.roc = function(realestrain, probabilidadestrain, realestest, probabilidadestest){

  AUC = c()

  aux.calculus = function(reales, probabilidades){
    levels(reales) = c(FALSE, TRUE)
    reales = as.logical(reales)
    reales = reales[order(probabilidades, decreasing = TRUE)]
    TPR = cumsum(reales)/sum(reales)
    FPR = cumsum(!reales)/sum(!reales)
    n = length(TPR)
    AUC <<- c(AUC, format(round(sum((FPR[2:n] - FPR[1:(n - 1)])*TPR[2:n]), 2), nsmall = 2))
    return(data.frame("TPR" = TPR, "FPR" = FPR))
  }

  aux = rbind(aux.calculus(realestrain, probabilidadestrain),
              aux.calculus(realestest, probabilidadestest))
  aux$Conjunto = c(rep("Entrenamiento", length(realestrain)),
                   rep("Prueba", length(realestest)))

  g = ggplot(data = aux) +
    geom_line(aes(x = FPR, y = TPR, colour = Conjunto)) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", size = 0.1) +
    theme_minimal() +
    labs(title = "Curva de ROC") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          # legend.text = element_text(size = 11),
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.title = element_blank()) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_manual(labels = c(paste0("AUC Entrenamiento: ", AUC[1], collapse = ""),
                                  paste0("AUC Prueba: ", AUC[2], collapse = "")),
                       values = c("darkred","darkblue"))
  return(g)
}
