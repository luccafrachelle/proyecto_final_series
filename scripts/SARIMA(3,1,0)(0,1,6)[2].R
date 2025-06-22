# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

TC_2021_12 <- tsoutliers::outliers(type = "TC", ind = 37)
TC_2021_12 <- tsoutliers::outliers.effects(TC_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts)) # Gráfico RE.

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              TC_2021_12,
              TC_2023_10)

Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(0,1,6), period = 2),
                  fixed = c(
                    0, 0, NA,
                    NA, 0, 0, 0, NA, NA,
                    NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_3) # Se fija ar1 y ar2 de la Estacionalidad en 0, lo que va de la mano con la FAC.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_3 <- data.frame(
  AIC = Modelo_3$aic,
  AICc = Modelo_3$aicc,
  BIC = Modelo_3$bic
)
Metricas_Modelo_3

# Diagnósticos:
# Primero se observa los Residuos:
Residuos_Modelo_3 <- residuals(Modelo_3)
residuos_Modelo_3_acf <- ggAcf(Residuos_Modelo_3, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_3_pacf <- ggAcf(Residuos_Modelo_3, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_3_acf, residuos_Modelo_3_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_3 <- Residuos_Modelo_3/sqrt(Modelo_3$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_3) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_3 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_3 <- rbind(ljung_box_df_Modelo_3, (Box.test(
    Residuos_Modelo_3, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_3

# Segundo se observa la Normalidad:
# QQ-plot
ggplot(data.frame(residuos = Residuos_Modelo_3), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la muestra", 
       title = "QQ-plot de los Residuos") +
  theme_minimal()
# Histograma
ggplot(data.frame(residuos = Residuos_Modelo_3)) +
  geom_histogram(aes(x = residuos, y = ..density..), bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(Residuos_Modelo_3), 
                                         sd = sd(Residuos_Modelo_3)),
                col = "red", size = 1) +
  labs(x = "Residuos", y = "Densidad", title = "Histograma de los Residuos") +
  theme_minimal()
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_3 <- shapiro.test(Residuos_Modelo_3) %>% tidy()
jarque_bera_test_result_Modelo_3 <- jarque.bera.test(Residuos_Modelo_3) %>% tidy()
shapiro_test_result_Modelo_3
jarque_bera_test_result_Modelo_3
# Se rechaza la Normalidad de los Residuos.

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
Residuos_Modelo_3_Cuadradado <- Residuos_Modelo_3^2
residuos_cuadrado_Modelo_3_acf <- ggAcf(Residuos_Modelo_3_Cuadradado, 
                                        lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC del Cuadrado de los Residuos") +
  theme_minimal()
residuos_cuadrado_Modelo_3_pacf <- ggAcf(Residuos_Modelo_3_Cuadradado, 
                                         lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP del Cuadrado de los Residuos") +
  theme_minimal()
grid.arrange(residuos_cuadrado_Modelo_3_acf, residuos_cuadrado_Modelo_3_pacf)

# Ljung-Box para los Cuadrados de los Residuos.
ljung_box_df_Modelo_3_residuos_cuadrado <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_3_residuos_cuadrado <- rbind(
    ljung_box_df_Modelo_3_residuos_cuadrado, 
    (Box.test(Residuos_Modelo_3_Cuadradado, 
              lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_3_residuos_cuadrado

################################################################################