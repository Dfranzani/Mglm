model.ks = function(realestrain, probabilidadestrain, realestest, probabilidadestest){

  ks = corte = c()

  aux.calculus = function(reales, probabilidades, conjunto){
    reales = reales[order(probabilidades, decreasing = TRUE)]
    acum.unos = cumsum(as.numeric(reales) - 1)
    acum.ceros = (seq(1,length.out = length(acum.unos)) - acum.unos)
    acum.unos = acum.unos/max(acum.unos)
    acum.ceros = acum.ceros/max(acum.ceros)
    ks <<- c(ks, format(round(max(acum.unos - acum.ceros),2), nsmall = 1))
    corte <<- c(corte, which.max(acum.unos - acum.ceros))
    ind = seq(1, length.out = length(acum.ceros))
    ind = ind/length(ind)
    return(data.frame("Reprobados" = acum.unos, "Aprobados" = acum.ceros,
                      "index" = ind, "Conjunto" = conjunto))
  }

  aux = rbind(aux.calculus(realestrain, probabilidadestrain, "Entrenamiento"),
              aux.calculus(realestest, probabilidadestest, "Prueba"))

  g = ggplot() +
    geom_line(data = aux, aes(x = index, y = Reprobados, colour = interaction(Conjunto))) +
    geom_line(data = aux, aes(x = index, y = Aprobados, colour = interaction(Conjunto))) +
    theme_minimal() +
    labs(title = "Curva KS", y = "Chance acumulada", x = "Tamaño de la muestra") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          # legend.text = element_text(size = 11),
          legend.position = c(1, 0),
          legend.justification = c("right", "bottom"),
          legend.title = element_blank()) +
    scale_color_manual(labels = c(paste0("Entrenamiento: ", ks[1], collapse = ""),
                                  paste0("Prueba: ", ks[2], collapse = "")),
                       values = c("darkred","darkblue"))
  return(g)
}
