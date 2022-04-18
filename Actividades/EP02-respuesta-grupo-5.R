#Grupo 5

#EL objetivo de este script es comparar Los ingresos registrados en las distintas provincias de la RM.
#En el gráfico se puede observar la media del ingreso total, separado por cada provincia de la región. Se utilizó la media
#para poder tener un estimado del total de riquezas que se encontraba en cada provincia, y el gráfico de barras para poder
#notar de manera gráfica las diferencias que púeden surgir entre cada provincia.
#Al observar el gráfico de barras, se puede notar que santiago tiene una media de ingreso total mucho mayor con respecto
#al resto de provincias de la región, siendo casi el doble de la media de ingresos de cada otra provincia que hay.

library(ggpubr)
library(dplyr)

#Recordar colocar la ruta en donde está guardado el archivo en su equipo.
datos <- read.csv2("C:\\Users\\rodri\\OneDrive\\Escritorio\\USACH\\1-2022\\IME\\Lecturas\\Ejercicios\\EP02 Datos Casen 2017.csv")

region <- datos %>% filter(region == "RegiÃ³n Metropolitana de Santiago")

provincias <- region %>% filter(provincia != "")

ingresosprovincias <- group_by(provincias, provincia) %>%
  summarise (count = n(), ingresos = mean(ytot))

ingresos <- ingresosprovincias[["ingresos"]]
provincias <- ingresosprovincias[["provincia"]]
datos <- data.frame(provincias,ingresos)

print(datos)

g <- ggbarplot ( datos ,
                 x = "provincias",
                 y = "ingresos",
                 bins = 100000,
                 title = "Ingresos por provincia",
                 xlab = " Provincias ",
                 ylab = " Ingresos [clp]")

print (g)
print(ingresosprovincias)