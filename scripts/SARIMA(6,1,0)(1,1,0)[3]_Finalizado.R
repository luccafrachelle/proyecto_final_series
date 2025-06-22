# Carga de Datos:
rm(list = ls())
df <- read_excel("series_bcu.xlsx" , sheet = "cantidad_personas_deuda_vigente") %>%
  select(fecha, cantidad_clientes, tipoinstitucion) %>%
  filter(tipoinstitucion == "Santander")
cantidad_clientes_ts <- ts(df$cantidad_clientes, start = c(2018, 12), end = c(2024, 12), frequency = 12)
cantidad_clientes_ts_completa <- ts(df$cantidad_clientes, start = c(2018, 12), frequency = 12) 

# Se toma el SARIMA(2,1,0)(0,1,1).
# Orden del AR de la FAC y FACP de la Serie Diferenciada.
# Orden Estacional del MA de la Serie Diferenciada en 3. Puede ser un AR también.
# Se interviene por sus Outliers.
# Se modela estacionalidad en rezago 3.

Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(2, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 3),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_1)
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 3),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_1)
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0),
                  seasonal = list(order = c(1,1,0), period = 3),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_1) 
# Se va con AR en ambas partes.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_1 <- data.frame(
  AIC = Modelo_1$aic,
  AICc = Modelo_1$aicc,
  BIC = Modelo_1$bic
)
Metricas_Modelo_1

# Primero se observa los Residuos:
Residuos_Modelo_1 <- residuals(Modelo_1)
residuos_Modelo_1_acf <- ggAcf(Residuos_Modelo_1, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_1_pacf <- ggAcf(Residuos_Modelo_1, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_1_acf, residuos_Modelo_1_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_1 <- Residuos_Modelo_1/sqrt(Modelo_1$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_1) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Hay Outliers, se rechaza la Normalidad:
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_1 <- shapiro.test(Residuos_Modelo_1) %>% tidy()
jarque_bera_test_result_Modelo_1 <- jarque.bera.test(Residuos_Modelo_1) %>% tidy()
shapiro_test_result_Modelo_1
jarque_bera_test_result_Modelo_1

outlier_m1 <- tso(log(cantidad_clientes_ts), 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(6, 1, 0),
                                       seasonal = list(order = c(1,1,0), period = 3),
                                       include.mean= FALSE)
) 
outlier_m1$outliers

# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

AO_2021_12 <- tsoutliers::outliers(type = "AO", ind = 37)
AO_2021_12 <- tsoutliers::outliers.effects(AO_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

AO_2023_02 <- tsoutliers::outliers(type = "AO", ind = 51)
AO_2023_02 <- tsoutliers::outliers.effects(AO_2023_02, length(cantidad_clientes_ts))

AO_2023_08 <- tsoutliers::outliers(type = "AO", ind = 57)
AO_2023_08 <- tsoutliers::outliers.effects(AO_2023_08, length(cantidad_clientes_ts))

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts))

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67) # Este no incide.
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts)) 

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              AO_2021_12,
              AO_2023_02,
              AO_2023_08,
              TC_2023_10,
              TC_2024_06
)

# Se incorpora un primer atípico:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0), # Original 1,1,0
                  seasonal = list(order = c(1,1,0), period = 3),
                  xreg = xreg,
                  fixed = c(
                    NA,
                    NA,
                    NA, 0, 0, 0, 0, 0, 0
                  ),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_2)

# Primero se observa los Residuos:
Residuos_Modelo_2 <- residuals(Modelo_2)
residuos_Modelo_2_acf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_2_pacf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_2_acf, residuos_Modelo_2_pacf)

# Se cambia el orden del AR Regular a 6:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(6, 1, 0), # Original 1,1,0
                  seasonal = list(order = c(1,1,0), period = 3),
                  xreg = xreg,
                  fixed = c(
                    NA, NA, NA, NA, NA, NA,
                    NA,
                    NA, 0, 0, 0, 0, 0, 0
                  ),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_2)
# Se fija coeficientes 1 a 5 en 0:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(6, 1, 0), # Original 1,1,0
                  seasonal = list(order = c(1,1,0), period = 3),
                  xreg = xreg,
                  fixed = c(
                    0, 0, 0, 0, 0, NA,
                    NA,
                    NA, 0, 0, 0, 0, 0, 0
                  ),
                  method = "ML"#, lambda = 0
                  )
coeftest(Modelo_2)

Metricas_Modelo_2 <- data.frame(
  AIC = Modelo_2$aic,
  AICc = Modelo_2$aicc,
  BIC = Modelo_2$bic
)
Metricas_Modelo_1
Metricas_Modelo_2

Residuos_Modelo_2 <- residuals(Modelo_2)
# Residuos estandarizados:
residuos_estandarizados_Modelo_2 <- Residuos_Modelo_2/sqrt(Modelo_2$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_2) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_2 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_2 <- rbind(ljung_box_df_Modelo_2, (Box.test(
    Residuos_Modelo_2, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_2
head(ljung_box_df_Modelo_2 %>% arrange(p.value))

# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_2 <- shapiro.test(Residuos_Modelo_2) %>% tidy()
jarque_bera_test_result_Modelo_2 <- jarque.bera.test(Residuos_Modelo_2) %>% tidy()
shapiro_test_result_Modelo_2
jarque_bera_test_result_Modelo_2

# Se continua la intervención de Outliers, se termina incorporando 5 en total:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(6, 1, 0), # Original 1,1,0
                  seasonal = list(order = c(1,1,0), period = 3),
                  xreg = xreg,
                  fixed = c(
                    0, 0, NA, 0, 0, NA,
                    NA,
                    NA, NA, NA, NA, NA, NA, NA # El penúltimo outlier puede o no incluirse.
                  ),
                  method = "ML"#, lambda = 0
)
coeftest(Modelo_2)

Residuos_Modelo_2 <- residuals(Modelo_2)
residuos_Modelo_2_acf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_2_pacf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_2_acf, residuos_Modelo_2_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_2 <- Residuos_Modelo_2/sqrt(Modelo_2$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_2) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Buen comportamiento.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_2 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_2 <- rbind(ljung_box_df_Modelo_2, (Box.test(
    Residuos_Modelo_2, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_2
head(ljung_box_df_Modelo_2 %>% arrange(p.value))

# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_2 <- shapiro.test(Residuos_Modelo_2) %>% tidy()
jarque_bera_test_result_Modelo_2 <- jarque.bera.test(Residuos_Modelo_2) %>% tidy()
shapiro_test_result_Modelo_2
jarque_bera_test_result_Modelo_2
# No se rechaza la Hipótesis Nula.

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
Residuos_Modelo_2_Cuadradado <- Residuos_Modelo_2^2
residuos_cuadrado_Modelo_2_acf <- ggAcf(Residuos_Modelo_2_Cuadradado, 
                                        lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC del Cuadrado de los Residuos") +
  theme_minimal()
residuos_cuadrado_Modelo_2_pacf <- ggAcf(Residuos_Modelo_2_Cuadradado, 
                                         lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP del Cuadrado de los Residuos") +
  theme_minimal()
grid.arrange(residuos_cuadrado_Modelo_2_acf, residuos_cuadrado_Modelo_2_pacf)

# Ljung-Box para los Cuadrados de los Residuos.
ljung_box_df_Modelo_2_residuos_cuadrado <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_2_residuos_cuadrado <- rbind(
    ljung_box_df_Modelo_2_residuos_cuadrado, 
    (Box.test(Residuos_Modelo_2_Cuadradado, 
              lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_2_residuos_cuadrado
head(ljung_box_df_Modelo_2_residuos_cuadrado %>% arrange(p.value))

################################################################################
################################################################################
# Predicción:
Modelo_Prediccion <- Modelo_2

# Fited del Modelo:
Modelo_Fitted_Tibble <- tibble(
  serie_original = as.vector((cantidad_clientes_ts)),
  serie_ajustada = as.vector((Modelo_Prediccion$fitted)),
  fecha = time(cantidad_clientes_ts) %>% as.Date()
)
ggplot(Modelo_Fitted_Tibble) +
  geom_line(aes(x = fecha, y = serie_original, color = "Serie Original")) +
  geom_line(aes(x = fecha, y = serie_ajustada, color = "Serie Ajustada")) +
  scale_color_manual(values = c("Serie Ajustada" = "red", "Serie Original" = "blue")) +
  theme_minimal() +
  labs(
    title = "Cantidad de Clientes con Deuda en Santander: Serie Ajustada contra Serie Original",
    subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
    y = "",
    x = "Fecha",
    color = ""
  ) +
  guides(color = guide_legend(position = "bottom"))

# Predicciones a 12 pasos:
xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), length(cantidad_clientes_ts) + 12),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  AO37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 37), length(cantidad_clientes_ts) + 12),
  AO51 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 51), length(cantidad_clientes_ts) + 12),
  AO57 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 57), length(cantidad_clientes_ts) + 12),   
  TC59 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 59), length(cantidad_clientes_ts) + 12),
  TC67 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 67), length(cantidad_clientes_ts) + 12)
)

Predicciones_Modelo <- forecast(
  Modelo_Prediccion, fan = TRUE, xreg = as.matrix(xreg_prediccion[74:85,]), h = 12)

autoplot(object = Predicciones_Modelo) +
  labs(title = "Cantidad de Clientes con Deuda en Santander: Predicción de 2025",
       subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
       x = "Fecha", y = "") +
  theme_minimal()

# Plot solo con los 3 meses de 2025:
Comparacion_Prediccion_Realidad <- tibble(
  Fecha = (time(cantidad_clientes_ts_completa) %>% as.Date())[(length(cantidad_clientes_ts_completa)-2):length(cantidad_clientes_ts_completa)],
  Predicha = (Predicciones_Modelo$mean[1:3]),
  Realidad = (cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-2):length(cantidad_clientes_ts_completa)])) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Comparacion_Prediccion_Realidad) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  theme_minimal() +
  labs(
    title = "Cantidad de Clientes con Deuda en Santander: Predicción contra Realidad",
    subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
    y = "",
    x = "Fecha",
    color = ""
  ) +
  guides(color = guide_legend(position = "bottom")) 

# Plot con 2024 incluído:
Comparacion_Prediccion_Realidad <- tibble(
  Fecha = (time(cantidad_clientes_ts_completa) %>% as.Date())[(length(cantidad_clientes_ts_completa)-14):length(cantidad_clientes_ts_completa)],
  Predicha = c(cantidad_clientes_ts[62:73],(Predicciones_Modelo$mean[1:3])),
  Realidad = (cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-14):length(cantidad_clientes_ts_completa)])) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Comparacion_Prediccion_Realidad) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  theme_minimal() +
  labs(
    title = "Cantidad de Clientes con Deuda en Santander: Predicción contra Realidad",
    subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
    y = "",
    x = "Fecha",
    color = ""
  ) +
  guides(color = guide_legend(position = "bottom")) 

# Errores de Predicción dentro de la Muestra:
accuracy(Modelo_Prediccion)

# Predicción afuera de la Muestra:
# Muestra de entrenamiento ("training set") hasta 2023 inclusive:
train_clientes <- window(cantidad_clientes_ts_completa, end = c(2023,12))
length(train_clientes)

# Se deja los datos de 2024 como conjunto de prueba ("test set")
test_clientes <- window(cantidad_clientes_ts_completa, start = 2024)
n <- length(test_clientes)

# Modelo 2 Entrenado hasta 2023:
Modelo_Final_train <- Arima(y = train_clientes,
                            order = c(6, 1, 0),
                            seasonal = list(order = c(1,1,0), period = 3),
                            fixed = c(
                              0, 0, NA, 0, 0, NA,
                              NA,
                              NA, NA, NA, NA, NA, NA, 0
                            ), # Un outlier vendría después, por eso se lo anula.
                            xreg = xreg[1:61,],
                            method = "ML"#, 
                            #lambda = 0
                            )

# Predicción dentro de la muestra:
Pred_Final_Test <- forecast(Modelo_Final_train, h = n, 
                            xreg = as.matrix(xreg_prediccion[62:(dim(xreg_prediccion)[1] -9),]))
accuracy(Pred_Final_Test, test_clientes)

autoplot(object = Pred_Final_Test) +
  labs(title = "Cantidad de Clientes con Deuda en Santander: Predicción de 2024 y Principios 2025",
       subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
       x = "Fecha", y = "") +
  theme_minimal()

Predicciones_Dentro_Muestra_Tibble <- tibble(
  Fecha = Pred_Final_Test$mean %>% time %>% as.Date(),
  Predicha = (Pred_Final_Test$mean[1:length(Pred_Final_Test$mean %>% time %>% as.Date())]),
  Realidad = (cantidad_clientes_ts_completa[
    (length(cantidad_clientes_ts_completa)-(
      length(Pred_Final_Test$mean %>% time %>% as.Date())-1)):length(cantidad_clientes_ts_completa)])) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Predicciones_Dentro_Muestra_Tibble) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  theme_minimal() +
  labs(
    title = "Cantidad de Clientes con Deuda en Santander: Predicciones dentro de Muestra",
    subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
    y = "",
    x = "Fecha",
    color = ""
  ) +
  guides(color = guide_legend(position = "bottom")) 
# Diferencia explicada por el atípico que no logramos capturar en el período.

# Si se lograra capturar el atípico, es decir, darse cuenta que en
# Junio de 2024 ocurrió un cambio transitorio.
train_clientes <- window(cantidad_clientes_ts_completa, end = c(2024,6))
length(train_clientes)

# Dejamos los datos de 2023 como conjunto de prueba ("test set")
test_clientes <- window(cantidad_clientes_ts_completa, start = c(2024,7))
n <- length(test_clientes)

# Modelo:
Modelo_Final_train <- Arima(y = train_clientes,
                            order = c(6, 1, 0),
                            seasonal = list(order = c(1,1,0), period = 3),
                            fixed = c(
                              0, 0, NA, 0, 0, NA,
                              NA,
                              NA, NA, NA, NA, NA, NA, NA
                            ), # Un outlier vendría después.
                            xreg = xreg[1:67,],
                            method = "ML", lambda = 0)
Pred_Final_Test <- forecast(Modelo_Final_train, h = n, 
                            xreg = as.matrix(xreg_prediccion[68:(dim(xreg_prediccion)[1]),]))
accuracy(Pred_Final_Test, test_clientes)

autoplot(object = Pred_Final_Test) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Predicciones_Dentro_Muestra_Tibble <- tibble(
  Fecha = (Pred_Final_Test$mean %>% time %>% as.Date())[1:9],
  Predicha = (Pred_Final_Test$mean[1:9]),
  Realidad = (cantidad_clientes_ts_completa[
    (length(cantidad_clientes_ts_completa)-(8)):length(cantidad_clientes_ts_completa)])) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Predicciones_Dentro_Muestra_Tibble) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(
    title = "Cantidad de Clientes con Deuda en Santander: Predicciones dentro de Muestra",
    subtitle = TeX(r"(Modelo SARIMA$(6,1,0)(1,1,0)[3]$)"),
    y = "",
    x = "Fecha",
    color = ""
  ) +
  theme_minimal() +
  guides(color = guide_legend(position = "bottom")) 




