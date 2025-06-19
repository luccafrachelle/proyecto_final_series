# Predicción:
cbind(AO_2019_09, 
      TC_2019_12, 
      AO_2021_12,
      AO_2023_08,
      TC_2023_10,
      TC_2024_06)


xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), (length(cantidad_clientes_ts) + 12)),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  AO37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 37), length(cantidad_clientes_ts) + 12),
  #AO51 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 51), (length(cantidad_clientes_ts) + 12)),
  AO57 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 57), (length(cantidad_clientes_ts) + 12)),   
  TC59 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 59), length(cantidad_clientes_ts) + 12),
  TC67 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 67), (length(cantidad_clientes_ts) + 12))
)

Predicciones_Modelo_3 <- forecast(Modelo_3, fan = TRUE, xreg = as.matrix(xreg_prediccion[74:85,]), h = 12)

autoplot(object = Predicciones_Modelo_3) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Comparacion_Prediccion_Realidad <- tibble(
  Fecha = c(1,2,3),
  Predicha = Predicciones_Modelo_3$mean[1:3],
  Realidad = cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-2):length(cantidad_clientes_ts_completa)]) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Comparacion_Prediccion_Realidad) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(color = "Predicción")

# Errores de Predicción dentro de la Muestra:
accuracy(Modelo_3)

# Predicción afuera de la Muestra:
# Muestra de entrenamiento ("training set") hasta 2022 inclusive
train_clientes <- window(cantidad_clientes_ts_completa, end = c(2024,6))
length(train_clientes)

# Dejamos los datos de 2023 como conjunto de prueba ("test set")
test_clientes <- window(cantidad_clientes_ts_completa, start = 2024)
n <- length(test_clientes)

# Modelo 3
Modelo_3_train <- Arima(y = train_clientes,
                        order = c(10, 1, 0),
                        seasonal = list(order = c(0,1,1), period = 12),
                        fixed = c(
                          0, NA, 0, 0, 0, 0, 0, NA, 0, NA, # originalmente era 2,1,0, pero usando lungbox se paso a 8 y luego a 10.
                          NA,
                          NA, NA, NA, NA, NA, NA
                        ), # Un outlier vendría después.
                        xreg = xreg[1:67,1:6],
                        method = "ML")

# Predecimos fuera de la muestra (el horizonte de predicción
# será igual al largo del test set)

# Modelo 3
Pred_3_test <- forecast(Modelo_3_train, h = n, 
                        xreg = as.matrix(xreg_prediccion[68:(dim(xreg_prediccion)[1]),1:6]))
accuracy(Pred_3_test, test_clientes)

autoplot(object = Pred_3_test) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Predicciones_Dentro_Muestra_Tibble <- tibble(
  Fecha = seq(1:17),
  Predicha = Pred_3_test$mean[1:17],
  Realidad = cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-16):length(cantidad_clientes_ts_completa)]) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Predicciones_Dentro_Muestra_Tibble) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(color = "Predicción")
# Diferencia explicada por el atípico que no logramos capturar en el período.
