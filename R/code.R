#' Métricas de GLM
#'
#' @param y variable respuesta binaria
#' @param x data frame de covariables
#' @param link función de enlace ("logit", "probit" o "cloglog"), por defecto es "logit".
#' @param p valor entre 0 y 1 que indica la proporción de entrenamiento, por defecto es 0.8.
#' @param balance tipo de balanceo ("under" u "over"), por defecto es NULL (sin balancear).
#' @param ms método de selección de covariables ("forward" o "backward"), por defecto es NULL (se ocupan todas las covariables).
#' @param semilla semilla de muestreo, por defecto es 9999.
#' @param influencia eliminación de los puntos de influecias del modelo, por defecto es TRUE.
#'
#' @return métricas de desempeño
#' @export
#'
#' @examples
#' df = iris[, c(1,4,5)]
#' df$Species = ifelse(df$Species == "virginica", 1, 0)
#' metrics = mtrglm(df$Species, df[,c(1:2)])
#' metrics$Train_Metrics
#' metrics$ROC
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme element_text element_blank theme_minimal scale_y_continuous scale_x_continuous scale_color_manual geom_segment geom_point
#' @importFrom UBL RandUnderClassif RandOverClassif
#' @importFrom blorr blr_plot_difchisq_fitted
#' @importFrom stats binomial formula glm predict rstudent step

mtrglm = function(y, x, link = "logit", p = 0.8, balance = NULL, ms = NULL, semilla = 9999, influencia = TRUE){

  metricas = function(reales, predichos){
    TP = sum(reales == 1 & predichos == 1)
    FN = sum(reales == 1 & predichos == 0)
    FP = sum(reales == 0 & predichos == 1)
    TN = sum(reales == 0 & predichos == 0)
    aux = c(
      "Recall" = TP/(TP + FN),
      "Precision" = TP/(TP + FP),
      "Accuracy" = (TP + TN)/(TP + TN + FP + FN),
      "F1 Score" = 2*TP/(2*TP+FP+FN)
    )
    return(aux)
  }

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
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = c(1, 0),
            legend.justification = c("right", "bottom"),
            legend.title = element_blank()) +
      scale_color_manual(labels = c(paste0("Entrenamiento: ", ks[1], collapse = ""),
                                    paste0("Prueba: ", ks[2], collapse = "")),
                         values = c("darkred","darkblue"))
    return(g)
  }

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
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "top",
            legend.title = element_blank()) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(0,1)) +
      scale_color_manual(values = c("darkred","darkblue"))
    return(g)
  }

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
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
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

  scatter.model = function(data, color, title){
    if(length(color) != 0){
      data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                        "v3" = ifelse(seq(1:length(data)) %in% color, "red", "black"))
    } else {
      data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                        "v3" = "black")
    }

    ggplot(data = data, mapping = aes(x = v2, y = v1)) +
      geom_point(colour = data$v3, size = 1.5) +
      theme_minimal() +
      labs(title = title, x = "", y = "") +
      theme(legend.position = "none",
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold"))
  }

  if(!is.factor(y)){
    y = as.factor(y)
  }

  base = data.frame("y" = y, x)

  set.seed(semilla)

  n = c(sample(which(base$y == 1), p*length(base$y[base$y == 1])),
        sample(which(base$y == 0), p*length(base$y[base$y == 0])))
  train = base[n,]
  test = base[-n,]

  if(!is.null(balance)){
    if(balance == "under"){
      train = UBL::RandUnderClassif(y~., dat = train)
    }
    if(balance == "over"){
      train = UBL::RandOverClassif(y~., dat = train)
    }
  }

  modelo.y.seleccion.variables = function(){
    if(is.null(ms)){
      modelo = glm(y ~ ., family = binomial(link = link), data = train)
      modelo.nulo = glm(y ~ 1, data = train, family = binomial(link = link))
    } else{
      modelo.max = glm(y ~ ., family = binomial(link = link), data = train)
      modelo.nulo = glm(y ~ 1, data = train, family = binomial(link = link))
      formula.modelo = step(modelo.nulo, scope = list(lower = formula(modelo.nulo), upper = formula(modelo.max)), direction = ms, trace = 0)
      modelo = glm(formula(formula.modelo), family = binomial(link = link), data = train)
    }

    cook = stats::cooks.distance(modelo)
    n1 = which(cook >= 4/(dim(train)[1]-length(modelo$coefficients)-2))

    deltachi = blorr::blr_plot_difchisq_fitted(modelo)
    deltachi = deltachi$plot_env$y
    n2 = which(deltachi >= 5)

    residuos = rstudent(modelo)
    n3 = which(abs(residuos) > 2)

    n_1 = intersect(intersect(n1, n2), n3)
    n_2 = unique(c(n1, n2, n3))

    if(influencia == TRUE & length(n_1) != 0){
      train = train[-c(n_1),]
      modelo = glm(formula(modelo), data = train, family = binomial(link))
    } else if(influencia == TRUE & length(n_2) != 0){
      train = train[-c(n_2),]
      modelo = glm(formula(modelo), data = train, family = binomial(link))
    } else {
      modelo = glm(formula(modelo), data = train, family = binomial(link))
    }

    metricas.entrenamiento = metricas(train$y, ifelse(predict(modelo, type = "response") < 0.5 , 0, 1))
    metricas.prueba = metricas(test$y, ifelse(predict(modelo, newdata = test, type = "response") < 0.5 , 0, 1))

    # metricas.entrenamiento = paste(format(round(metricas.entrenamiento*100, 2), nsmall = 2), "%", sep = " ")
    # metricas.prueba = paste(format(round(metricas.prueba*100, 2), nsmall = 2), "%", sep = " ")

    roc = model.roc(train$y, predict(modelo, type = "response"),
                    test$y, predict(modelo, newdata = test, type = "response"))
    pr = model.pr(train$y, predict(modelo, type = "response"),
                  test$y, predict(modelo, newdata = test, type = "response"))
    ks = model.ks(train$y, predict(modelo, type = "response"),
                  test$y, predict(modelo, newdata = test, type = "response"))

    Cooks = scatter.model(cook, n1, title = "Distancias de Cook")
    Deltachi = scatter.model(deltachi, n2, title = "Valores Delta Chi")
    Residuos = scatter.model(residuos, n3, title = "Residuos estandarizados")

    modelo = list("Model" = modelo,
                  "Null model" = modelo.nulo,
                  "Train_Metrics" = metricas.entrenamiento,
                  "Test_Metrics" = metricas.prueba,
                  "Cooks" = Cooks,
                  "DeltaChi" = Deltachi,
                  "Residuos" = Residuos,
                  "ROC" = roc,
                  "PR" = pr,
                  "KS" = ks)

    return(modelo)
  }
  return(modelo.y.seleccion.variables())
}
