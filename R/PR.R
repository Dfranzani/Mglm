model.pr = function(realestrain, probabilidadestrain, realestest, probabilidadestest){

  aux.calculus = function(reales, probabilidades, conjunto){
    cuts = unique(probabilidades)
    Precision = Recall = c()
    for (i in 1:length(cuts)) {
      predichos = ifelse(probabilidades <= cuts[i], 0, 1)
      TP = sum(reales == 1 & predichos == 1)
      FN = sum(reales == 1 & predichos == 0)
      FP = sum(reales == 0 & predichos == 1)
      Precision[i] = TP/(TP + FP)
      Recall[i] = TP/(TP + FN)
    }
    return(data.frame("Precision" = Precision, "Recall" = Recall,
                      "Conjunto" = conjunto))
  }

  aux = rbind(aux.calculus(realestrain, probabilidadestrain, "Entrenamiento"),
              aux.calculus(realestest, probabilidadestest, "Prueba"))

  g = ggplot(data = aux, mapping = aes(x = Recall, y = Precision, colour = Conjunto)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Curva PR") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          # legend.text = element_text(size = 11),
          legend.position = "top",
          legend.title = element_blank()) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_manual(values = c("darkred","darkblue"))
  return(g)
}
