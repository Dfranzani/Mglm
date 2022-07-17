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
