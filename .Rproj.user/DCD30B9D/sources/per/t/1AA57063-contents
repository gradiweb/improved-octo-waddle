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

testDataFrame$casual <-c(0)
testDataFrame$registered <-c(0)
testDataFrame$count <-c(0)
View(testDataFrame)

# 4 Es necesario ajustar el nombre de alguna de las variables? O los nombres en las dos bases de datos coinciden?

colnames(testDataFrame) <- c("datetime", "season","holiday","workingday","weather","temp","atemp","humidity","windspeed","casual","registered","count")


# 5 Cree con la función data.frame y con la función colnames, una tabla donde compare 
#los nombres de la base de datos train y test.

#setdiff(testDataFrame,trainDataFrame) => Comparar tipos de datos en las columnas
comparedf(testDataFrame, trainDataFrame)

# 6 Una las dos bases de datos

rm(mergedDataFrames)
mergedDataFrames = rbind(testDataFrame,trainDataFrame)


# 7 Identifique si hay filas repetidas y elimínelas

nrow(mergedDataFrames[duplicated(mergedDataFrames), ])
distinct(mergedDataFrames)

#8 Cree un Data frame donde compare las dimensiones de los data frames originales 
#con la dimensión del nuevo después de unirlas. Use la función dim.


dimensionsTable <- data.frame(columns=rep(c(ncol(testDataFrame),ncol(trainDataFrame),ncol(mergedDataFrames))),
                 rows=rep(c(nrow(testDataFrame),nrow(trainDataFrame),nrow(mergedDataFrames))))


lapply(mergedDataFrames, class)

# 9. Cuáles variables necesitan ser transformadas en factor?. 
#Como los nombres de los factores son poco ilustrativos, 
#reasigne estos valores con etiquetas más informativas

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

media = sum(trainDataFrame$count) / nrow(trainDataFrame) # Preguntar a la profe

# 14. Hacer una tabla con las principales medidas descriptivas para responder la pregunta de 
#si la estación influye o no sobre el número de bicicletas rentadas.

summary(mergedDataFrames) ##

# 1 = spring, 2 = summer, 3 = fall, 4 = winter

countSpring = mergedDataFrames %>% filter(season == 1) 
sum(countSpring$count)
  
countSummer = mergedDataFrames %>% filter(season == 2) 
sum(countSummer$count)

countFall = mergedDataFrames %>% filter(season == 3) 
sum(countFall$count)

countWinter = mergedDataFrames %>% filter(season == 4) 
sum(countWinter$count)

descriptiveTable <- data.frame(season=rep(c("Spring","Summer","Fall","Winter")),
                              rentedbikes=rep(c(sum(countSpring$count),sum(countSummer$count),sum(countFall$count),sum(countWinter$count))))

summary(descriptiveTable)

#15 Hacer una tabla con las principales medidas descriptivas para responder 
#la pregunta de si el estado del clima influye o no sobre el número de bicicletas rentadas.

View(mergedDataFrames) # Preguntar a la profe

#16 Instrucción para ver el número de objetos que hay en la memoria en este momento

ls()

#17 - 18 Vuelva la variable count un factor, con tres categorías, rentasbajas, rentasmedias, rentasaltas,
#llamela CountFactor. Es decir, escriba una instrucción que divida en tres partes los datos de la variable count.

mergedDataFrames$count2 <- cut(mergedDataFrames$count, breaks = 3, labels = c("Bajas","Medias","Altas"))
View(mergedDataFrames)

# 19 Elabore una tabla de porcentajes con el paquete base donde cuente cuántos días se
#registraron ventas Bajas, Medias, Altas


lowP = mergedDataFrames %>% filter(count2 == "Bajas") 
sum(lowP$count)

mediaP = mergedDataFrames %>% filter(count2 == "Medias") 
sum(mediaP$count)

highP = mergedDataFrames %>% filter(count2 == "Altas") 
sum(highP$count)


totalRented = sum(mergedDataFrames$count)

as.numeric(mergedDataFrames$datetime)

lapply(mergedDataFrames, class) # Preguntar a la profe


# 20 Haga el mismo punto anterior con la función correspondiente del paquete janitor utilizando el paquete Janitor (es importante que 
#la variable que categoriza las ventas ya haya sido agregada a la base de datos).


# 21. Investigue con la ayuda de la función tabyl cómo eliminar la fila
#de datos faltantes en esta tabla.


any(is.na(mergedDataFrames)) # Preguntar a la profe

# 22 Tabla cruzada de la variable CountFactor y Season. 
#Con la función table y con la función tabyl del paquete janitor.

round(prop.table(table(mergedDataFrames$count2, mergedDataFrames$season),1),2) # 1 significa que saca porcentajes fila
round(prop.table(table(mergedDataFrames$count2, mergedDataFrames$season),2),2) # significa que saca porcentajes columna
round(prop.table(table(mergedDataFrames$count2, mergedDataFrames$season)),2) # significa que saca porcentajes columna
# Preguntar a la profe

# 23. Seleccione los datos que corresponden a los días festivos

holidays = mergedDataFrames %>% filter(holiday == 1) 

# 24 Haga una tabla donde sevisualice el promedio y el Coeficiente de variación para el número de bicicletas rentadas en 
#los días festivos y en los no festivos. Recuerde que hay datos faltantes use el argumento, na.rm = TRUE.





# ordered
#  3. (1.0)Haga dos tabla cruzadas donde combine dos variables 
#cualitativas - Laprimera de porcentajes fila
#y la segunda de porcenajes columna (coméntela brevemente)

round(prop.table(table(Datos$Preference_Color, Datos$Dissatisfied_Delivery),1),2) # 1 significa que saca porcentajes fila
round(prop.table(table(Datos$Preference_Color, Datos$Dissatisfied_Delivery),2),2) # significa que saca porcentajes columna
round(prop.table(table(Datos$Preference_Color, Datos$Dissatisfied_Delivery)),2) # significa que saca porcentajes columna
?prop.table

#4. (1.0)Compare las principales medidas descriptivas 
#de una variable cuantitativa en términos de una
#cualitativa. Intérprete y resalte lo más importante.
tapply(Datos$Cost_Flowers , Datos$Preference_Color, summary)


#Calcule el coeficiente de variación de la variable
#Costo en dólares para las flores rojas y blancas
#por separado y compare la variabilidad (INTERPRETE). 
#Recuerde CV = desviación/Media
cv_red<-sd(Datos$Cost_Flowers[Datos$Preference_Color=="Red"])/mean(Datos$Cost_Flowers[Datos$Preference_Color=="Red"])*100
cv_white<-sd(Datos$Cost_Flowers[Datos$Preference_Color=="White"])/mean(Datos$Cost_Flowers[Datos$Preference_Color=="White"])*100
cv_red;cv_white

