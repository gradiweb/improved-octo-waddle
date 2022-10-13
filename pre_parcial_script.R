# Read Data
install.packages("dplyr")
install.packages("arsenal")
library("dplyr")
library("janitor")
library("arsenal")

testDataFrame = read.csv("./test1.csv")
trainDataFrame = read.csv("./train1.csv")
View(trainDataFrame)
View(testDataFrame)

# 1. Hay dos cosas qué debe tener en cuenta para poder unir los data frames train1 y test 1? 
#Mencione estas dos cosas.

# Las dimensiones y las clases de objetos con los cuáles se hara el merge deben ser iguales

# 2. Escriba las instrucciones para solucionar este problema?

# 3. Introduzca en la base de datos de Test, 
#nuevas columnas, pues antes de unir las dos bases de datos es necesario que tengan el mismo número de columnas.

names()

testDataFrame$casual <- NA
testDataFrame$registered <- NA
testDataFrame$count <- NA

# 4 Es necesario ajustar el nombre de alguna de las variables? O los nombres en las dos bases de datos coinciden?

colnames(testDataFrame) <- c("datetime", "season","holiday","workingday","weather","temp","atemp","humidity","windspeed","casual","registered","count")


# 5 Cree con la función data.frame y con la función colnames, una tabla donde compare 
#los nombres de la base de datos train y test.

#setdiff(testDataFrame,trainDataFrame) => Comparar tipos de datos en las columnas
comparedf(testDataFrame, trainDataFrame)

# 6 Una las dos bases de datos

rm(mergedDataFrames)
mergedDataFrames = rbind(testDataFrame,trainDataFrame)
View(mergedDataFrames)

# 7 Identifique si hay filas repetidas y elimínelas

nrow(mergedDataFrames[duplicated(mergedDataFrames), ])
distinct(mergedDataFrames)

#8 Cree un Data frame donde compare las dimensiones de los data frames originales 
#con la dimensión del nuevo después de unirlas. Use la función dim.


dimensionsTable <- data.frame(columns=rep(c(ncol(testDataFrame),ncol(trainDataFrame),ncol(mergedDataFrames))),
                 rows=rep(c(nrow(testDataFrame),nrow(trainDataFrame),nrow(mergedDataFrames))))

View(mergedDataFrames)
lapply(mergedDataFrames, class)

# 9. Cuáles variables necesitan ser transformadas en factor?. 
#Como los nombres de los factores son poco ilustrativos, 
#reasigne estos valores con etiquetas más informativas
levels(factor(mergedDataFrames$season))
mergedDataFrames$season = factor(mergedDataFrames$season,levels = c("1", "2", "3", "4"),labels = c("spring","summer","fall","winter"))
mergedDataFrames$holiday = factor(mergedDataFrames$holiday,levels = c("1", "0"),labels = c("holiday","not-holiday"))
mergedDataFrames$workingday = factor(mergedDataFrames$workingday,levels = c("1", "0"),labels = c("weekend","not-weekend"))
mergedDataFrames$weather = factor(mergedDataFrames$weather,levels = c("1", "2","3","4"),labels = c("few-clouds","broken-clouds","light-rain","heavy-rain"))


lapply(mergedDataFrames, class)


# lapply(mergedDataFrames, class) con la anterior instrucción podemos validar que todos los datos son de tipo entero y numerico
# por lo cual no es necesario cambiar su tipo a factor

# 10 Cuántasvariables?¿cuántasobservaciones?conlafuncióndim

dim(mergedDataFrames)
# Observaciones:17379    Variables:12

# 11. Instrucción que selecciona los nombres de las variables.

colnames(mergedDataFrames)

# 12. Instrucción que me indica el número de filas e instrucción que me indica el número de columnas en un data frame.

ncol(mergedDataFrames)
nrow(mergedDataFrames)

# 13. El promedio del número de bicicletas rentadas (tengan en cuenta que hay datos faltantes)

mediaBikes = sum(trainDataFrame$count) / length(trainDataFrame) # Preguntar a la profe
mediaBikes

# 14. Hacer una tabla con las principales medidas descriptivas para responder la pregunta de 
#si la estación influye o no sobre el número de bicicletas rentadas.

tapply(mergedDataFrames$count,mergedDataFrames$season,mean,na.rm = TRUE)

#15 Hacer una tabla con las principales medidas descriptivas para responder 
#la pregunta de si el estado del clima influye o no sobre el número de bicicletas rentadas.

tapply(mergedDataFrames$count,mergedDataFrames$weather,mean,na.rm = TRUE)

#16 Instrucción para ver el número de objetos que hay en la memoria en este momento

ls()

#17 - 18 Vuelva la variable count un factor, con tres categorías, rentasbajas, rentasmedias, rentasaltas,
#llamela CountFactor. Es decir, escriba una instrucción que divida en tres partes los datos de la variable count.

mergedDataFrames$CountFactor <- cut(mergedDataFrames$count, breaks = 3, labels = c("Bajas","Medias","Altas"))
View(mergedDataFrames)

# 19 Elabore una tabla de porcentajes con el paquete base donde cuente cuántos días se
#registraron ventas Bajas, Medias, Altas

# Función para eliminar las filas con NA's
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

first(mergedDataFrames$datetime)
last(mergedDataFrames$datetime)

lowSales = delete.na(mergedDataFrames[mergedDataFrames$CountFactor == "Bajas",])
mediumSales = delete.na(mergedDataFrames[mergedDataFrames$CountFactor == "Medias",])
highSales = delete.na(mergedDataFrames[mergedDataFrames$CountFactor == "Altas",])

# Fecha inicial y final con ventas bajas
firstDateLowSales = as.Date(first(lowSales$datetime),format="%Y/%m/%d")
lastDateLowSales = as.Date(last(lowSales$datetime),format="%Y/%m/%d")
diff_in_days <- difftime(firstDateLowSales ,lastDateLowSales , units = c("days"))
diff_in_days
lastDateLowSales
firstDateLowSales - lastDateLowSales

# Fecha inicial y final con ventas medias
firstDateMediumSales = first(mediumSales$datetime)
lastDateMediumSales = last(mediumSales$datetime)


# Fecha inicial y final con ventas Altas
firstDateHighSales = first(highSales$datetime)
lastDateHighSales = last(highSales$datetime)


# Fecha inicial y final con ventas Medias
firstDateLowSales = first(lowSales$datetime)
lastDateLowSales = last(lowSales$datetime)

#20 Haga el mismo punto anterior con la función correspondiente del paquete janitor utilizando el 
#paquete Janitor 
#(es importante que la variable que categoriza las ventas ya haya sido agregada a la base de datos).



#21 Investigue con la ayuda de la función tabyl cómo eliminar la fila de datos faltantes en esta tabla.

any(is.na(mergedDataFrames))


# 22 Tabla cruzada de la variable CountFactor y Season. 
#Con la función table y con la función tabyl del paquete janitor.

round(prop.table(table(mergedDataFrames$CountFactor, mergedDataFrames$season),1),2) 
round(prop.table(table(mergedDataFrames$CountFactor, mergedDataFrames$season),2),2) 
round(prop.table(table(mergedDataFrames$CountFactor, mergedDataFrames$season)),2) 

# 23. Seleccione los datos que corresponden a los días festivos

holidays = mergedDataFrames %>% filter(holiday == 'holiday') 
View(holidays)

# 24 Haga una tabla donde sevisualice el promedio y el Coeficiente de variación para el número de bicicletas rentadas en 
#los días festivos y en los no festivos. Recuerde que hay datos faltantes use el argumento, na.rm = TRUE.

notHolidays = mergedDataFrames %>% filter(holiday == 'not-holiday') 
#cv <- sd(mergedDataFrames) / mean(mergedDataFrames) * 100
mean(mergedDataFrames$count)
tapply(mergedDataFrames$count , mergedDataFrames$holiday, mean,na.rm = TRUE)

# 25 En la tabla anterior observa diferencias significativas.Cómo la puede interpretar?

#El coeficiente de variación indica que dispersión en los días feriados es más alta

#26 Seleccione los datos que se refieren a los días festivo y al mes de verano y calcule un summary.
summerHolidays = mergedDataFrames %>% filter(holiday == 'holiday' & season == 'summer') 
summary(summerHolidays)


# 27 Interprete lo más importante de los resultados obtenidos en el summary del ejercicio anterior.
# Podemos visualizar que el clima imperante esta basado en algunas nubes, la temperatura media es de 26 grados, la humedad 
# promedio es de 64.50, el promedio de bicicletas rentadas es de 197.8

 # 28 Seleccione los días que corresponde a summer o spring y cuyo número de rentas fueron bajas.
lowSpringOrSummerSales = mergedDataFrames %>% filter((season == 'summer' | season == 'spring') & CountFactor == 'Bajas') 
summary(summerHolidays)

# 29 Calcule una nueva columna qué indique qué porcentaje de las rentas corresponde a Usuarios registrados y calcule el promedio.

# 31 Use la funcíon tabyl para hacer algunas tablas interesantes. Saque porcentajes del total fila, columna o del total general.
tabyl(mergedDataFrames, temp, humidity, count, show_na = FALSE, show_missing_levels = FALSE)



