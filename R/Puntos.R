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
          # axis.title = element_text(size = 11),
          # axis.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold"))
}
