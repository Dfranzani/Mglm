#' @encoding UTF-8
#' @title Métricas de GLM
#'
#' @param y variable respuesta binaria.
#' @param x data frame de covariables.
#' @param link función de enlace ("logit", "probit" o "cloglog"), por defecto es "logit".
#' @param p valor entre 0 y 1 que indica la proporción de entrenamiento, por defecto es 0.8.
#' @param balance tipo de balanceo ("under" u "over"), por defecto es NULL (sin balancear).
#' @param ms método de selección de covariables ("forward" o "backward"), por defecto es NULL (se ocupan todas las covariables).
#' @param semilla semilla de muestreo, por defecto es 9999.
#' @param influencia eliminación de los puntos de influencias del modelo, por defecto es TRUE.
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
