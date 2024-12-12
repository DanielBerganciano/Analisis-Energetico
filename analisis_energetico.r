#Vectores Iniciales
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(120, 150, NA, 130, 140, NA, 160, 170, 180, NA, 200, 210, NA, 190, 220, 230, 240, NA, 250, 260)
costo_kwh <- c(rep(0.15, 10), rep(0.20, 10))

mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

consumo[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "No Renovable"] <- mediana_no_renovable

#Crear el dataframe
df_consumo <- data.frame(Energia = energia, Consumo = consumo, Costo_kWh = costo_kwh)

df_consumo$Costo_Total <- df_consumo$Consumo * df_consumo$Costo_kWh

consumo_total <- tapply(df_consumo$Consumo, df_consumo$Energia, sum)
costo_total <- tapply(df_consumo$Costo_Total, df_consumo$Energia, sum)
media_consumo <- tapply(df_consumo$Consumo, df_consumo$Energia, mean)

df_consumo$Ganancia <- df_consumo$Costo_Total * 1.1

df_ordenado <- df_consumo[order(-df_consumo$Costo_Total), ]

top_3_costos <- head(df_ordenado, 3)

#Crear la lista resumen
resumen_energia <- list(Consumo_Total = consumo_total, Costo_Total = costo_total, Media_Consumo = media_consumo, Top_3_Costos = top_3_costos)
print(resumen_energia)