#' @encoding UTF-8
#' @title Curva de ganancia
#'
#' @param reales valores reales
#' @param probabilidades probabilidades de las predicciones
#'
#' @return Gráfico de ganancia
#' @export
model.ganancia = function(reales, probabilidades){
  aux = data.frame("Reales" = as.numeric(reales) - 1,
                   "Probabilidades" = probabilidades)

  aux = aux[rev(order(aux$Probabilidades)),]
  aux$index = (1:dim(aux)[1])/dim(aux)[1]
  aux$AcumPerfecto = cumsum(aux$Reales)/sum(aux$Reales)
  aux$AcumReales = NA

  for(i in 1:dim(aux)[1]){
    p = ifelse(aux$Probabilidades <= aux$Probabilidades[i], 0, 1)
    p = aux$Reales + p
    p = ifelse(p == 2, 1, 0)
    aux$AcumReales[i] = sum(p)/sum(aux$Reales)
  }
  aux = rbind(c(0,0,0,0,0), aux)
  aux = aux[c(1, round(dim(aux)[1]*seq(0.1,1,0.1),0)),]

  g = ggplot(data = aux) +
    geom_line(aes(x = index, y = AcumPerfecto, colour = "c1")) +
    geom_line(aes(x = index, y = AcumReales, colour = "c2")) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = 0.1) +
    theme_minimal() +
    labs(title = "Curva de Ganancia",
         x = "Proporción de la población",
         y = "Proporción objetivo") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.title = element_blank()) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.1), labels = seq(0,1,0.1)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), labels = seq(0,1,0.1)) +
    scale_color_manual(labels = c("Perfecta", "Modelada"),
                       values = c("red","darkblue"))
  return(g)
}
