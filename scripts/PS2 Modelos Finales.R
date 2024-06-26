#Modelo Final

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(ggplot2)
datos <- data.frame(
  variable = c("Logit", "Elstic Net", "CARTs", "Random Forest"),
  frecuencia = c(0.52, 0.49, 0.36, 0.55)
)

# Crear el gráfico de barras
p_3 <- ggplot(datos, aes(x = variable, y = frecuencia)) +
  geom_bar(stat = "identity",fill = "skyblue") +
  labs(x = "Variable", y = "F1", title = "Éxito de la Predicción por Método para clasificación")+
  theme_minimal()

png("MECA4107_G5_ProblemSet_02/views/p_3.png", width = 800, height = 600)
