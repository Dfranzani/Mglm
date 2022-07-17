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
