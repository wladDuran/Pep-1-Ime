# Fijar carpeta de trabajo.
setwd("D:/Dropbox/Inferencia/Ejercicios pr�cticos 1-2022/EP01")

# Importar paquetes.
library(dplyr)
library(tidyr)

# Cargar datos.
datos <- read.csv2("EP01 Datos Covid.csv")



# �Qu� variables se han cargado?

# Se han cargado 735 variables, donde la primera corresponde a la regi�n del
# pa�s (o al total nacional) y las restantes, a la cantidad de nuevos casos de
# Covid-19 registrados cada d�a entre el 3 de marzo de 2020 y el 6 de marzo de
# 2022.



# �Qu� tipo tiene cada una de estas variables?

# La primera variable (Region) es de tipo string, mientras que las restantes
# son de tipo entero.



# �Qu� escala parecen tener estas variables?

# Las variables parecen tener una escala entera no negativa.



# �Qu� d�a se produjo el mayor n�mero de casos con s�ntomas en la regi�n de
# O'Higgins entre el 01-abr-2020 y el 31-ago-2020?

# Seleccionar datos del periodo y regi�n solicitados.
datos <- datos %>% filter(Region == "O'Higgins")
datos <- datos %>% select(X01.04.2020:X31.08.2020)

# Reordenar datos como una matriz de datos con las columnas:
# dia, mes, agno, casos.
fecha <- as.Date(colnames(datos), tryFormats = c("X%d.%m.%y"))
casos <- as.integer(t(datos))
datos <- data.frame(fecha, casos)
datos <- datos %>% separate(fecha, sep="-", into = c("agno", "mes", "dia"),
                            convert = TRUE)

datos <- datos %>% select(dia, mes, agno, casos)

# Encontrar fecha con m�ximo de contagios.
print(datos %>% filter(casos == max(datos[["casos"]])))

# La mayor cantidad de nuevos casos sintom�ticos en la regi�n de O'Higgins entre
# el 01-abr-2020 y el 31-ago-2020 se produjo el 20 de junio, con 263 nuevos
# casos.



# �Cu�l fue el total de casos con s�ntomas para cada mes de este periodo?

print(datos %>% group_by(mes) %>%  summarise(casos = sum(casos)))

# Abril :    84 casos
# Mayo  :   527 casos
# Junio : 4.228 casos
# Julio : 3.616 casos
# Agosto: 2.055 casos