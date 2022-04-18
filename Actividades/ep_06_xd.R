library(dplyr)
options(scipen = 100)

# Grupo 5:

#Wladimir Durán
#Rodrigo Hernández
#Manuel Villar

# Crear tabla de contingencia.
mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)
hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)
valorNulo <- 0.42

tabla <- as.table( rbind ( mujeres , hombres ) )

dimnames ( tabla ) <- list ( sexo = c(" mujeres ", " hombres ") ,
                               especialidad = c("Pediatría", "Obstetricia", "Dermatología",
                                                "Psiquiatría", "Medicina Interna", "Oncología",
                                                "Neurología", "Anestesiología", "Radiología") )

print ( tabla )


#---- Pregunta 1 ----
#   1. Estudios previos habían determinado que la proporción de autoras en la especialidad de medicina interna era 
# de 42%. ¿Respaldan estos datos tal estimación?

# Hipotesis nula: La proporción de autoras en medicina interna es de 42%
# Hipotesis alternativa: La proporción de autoras en medicina interna es distinta de 42%


mujeresMedInt <- tabla[1,"Medicina Interna"]
medicinaInterna <- sum(mujeresMedInt,tabla[2,"Medicina Interna"])

probExito <- round(mujeresMedInt / medicinaInterna,5)
probFracaso <- 1 - probExito
exitos <- probExito*medicinaInterna
alfa <- 0.05

pruebaWilson <- prop.test( exitos , n = medicinaInterna , p = valorNulo ,
                           alternative = "two.sided", conf.level = 1 - alfa )

print(pruebaWilson)
print("Mediante la prueba de wilson, nos indica que la proporción de autoras y autores en medicina interna es de 0.40909, además de indicar que p > alfa, es decir el 40,9% de la gente de medicina interna corresponden a las mujeres, por lo que se cumple la hipótesis alternativa y se indica que la proporción de autoras es distinta del 42% estípulado como hipótesis nula.")


#---- Pregunta 2 ----

#   2. Según estos datos, ¿es igual la proporción de autoras en las áreas de obstetricia y radiología?
#H0: La proporción de autoras en obstetricia es igual a la proporción de autoras en radiología.
#Ha: La proporción de autoras en obstetricia es distinta a la proporción de autoras en radiología.

#Valor nulo 0 debido a que la diferencia entre las proporciones debería ser 0 para que se cumpla la hipótesis nula.
valorNulo1 <- 0
alfa1 <- 0.5
mujeresObstetricia <- tabla[1,"Obstetricia"]
mujeresRadiologia <- tabla[1, "Radiología"]

nObstetricia <- sum(mujeresObstetricia,tabla[2,"Obstetricia"])
nRadiologia <- sum(mujeresRadiologia,tabla[2,"Radiología"])
  
probExitoObste <- round(mujeresObstetricia / nObstetricia,5)
probExitoRad <- round(mujeresRadiologia / nRadiologia,5)

diferencia <- probExitoObste - probExitoRad

errorObste <- (probExitoObste * (1 - probExitoObste)) / nObstetricia
errorRad <- (probExitoRad * (1 - probExitoRad)) / nRadiologia
errorEst <- sqrt (errorObste + errorRad)
zCritico <- qnorm (alfa1 / 2 , lower.tail = FALSE)
inferior <- diferencia - zCritico * errorEst
superior <- diferencia + zCritico * errorEst
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de hipótesis .

pAgrupada <- ( probExitoObste + probExitoRad ) / ( nObstetricia + nRadiologia)
errorObste1 <- ( pAgrupada * (1 - pAgrupada ) ) / nObstetricia
errorRad1 <- ( pAgrupada * (1 - pAgrupada ) ) / nRadiologia

errorEstHip <- sqrt ( errorObste + errorRad1 )
Z <- ( diferencia - valorNulo1 ) / errorEstHip
p <- 2 * pnorm(Z , lower.tail = FALSE)

cat (" Hipótesis alternativa bilateral \n")
cat ("Z =", Z , "\n")
cat ("p =", p )

print("Mediante la prueba de Wald para dos proporciones, se obtienen...")

#---- Pregunta 3 ----

#   3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de anestesiología y la de pediatría
# es de 0,28. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 95% y poder 
# estadístico de 80%, si se intenta mantener aproximadamente la misma proporción de gente estudiada en cada 
# caso?

