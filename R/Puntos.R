#' @encoding UTF-8
#' @title Gráfico de puntos para métricas de influencia
#'
#' @param data vector de valores de la métrica.
#' @param color vector con los valores que violan la métrica.
#' @param title título del gráfico.
#'
#' @return Se utiliza para los gráficos de características influencia de los residuos.
#' @export
scatter.model = function(data, color, title){
  if(length(color) != 0){
    data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                      "v3" = ifelse(seq(1:length(data)) %in% color, "red", "black"))
  } else {
    data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                      "v3" = "black")
  }

  ggplot(data = data, mapping = aes(x = v2, y = v1)) +
    geom_point(colour = data$v3) +
    theme_minimal() +
    labs(title = title, x = "", y = "") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"))
}
