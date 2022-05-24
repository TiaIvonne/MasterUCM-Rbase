library("GLDEX")
library("xlsx") #Para leer archivos excel
library("openxlsx")

#Ejercicio 1
#**Pregunta 1**
  
#a) Generar todos los números que entran en el sorteo de la ONCE y mostrarlos con los cuatro dígitos.


#Se genera el set de datos
numeros <- c(formatC(0000:9999, width = 4, flag = '0' ))

#El set de datos se divide en digitos y se realiza la suma
resultado <- sapply(numeros, function(x) sum( as.numeric(unlist(strsplit(as.character(x), split=""))) ))


# b) ¿Cuál es la suma de los números de un boleto que más se repite?
  

mas_repetido <- tail(names(sort(table(resultado))), 1)
sprintf("La suma de los numeros de un boleto que mas se repite es %s. ", mas_repetido)




#Ejercicio 2

"Paso 1 leer los archivos"

cod_ccaa <- read.csv("CodCCAA.csv")
datos_provincias <- data.frame(read.csv("datos_provincias.csv"))
cod_provincias <- data.frame(read.table(file = "CodProv.txt", sep=",", header = TRUE, row.names = NULL))


"Paso 2 se elimina la ES de la columna Codigo del data frame cod_provincias, 
es para dejar el dato limpio "
cod_provincias$Código <- sub("^(\\w+)-", "", cod_provincias$Código)

"Con merge uno tanto el data frame cod_provincias como datos provincias para traer el codigo de la comunidad autonoma que corresponde"
base_graficos <- merge(cod_provincias, datos_provincias, by.x = "Código", by.y = "provincia_iso", all.y = TRUE)

"Paso 2 seleccionar los datos de la comunidad autonoma que me corresponde.
En mi caso no tengo numero de DNI o pasaporte de España por lo que utilice un 
número de un generador de DNI's"

dni <- 169728542
dni_provincias <- dni %% 17

"Segun este calculo la provincia asignada es PV, pais vasco, se filtra segun el codigo de la comunidad autonoma"
viscaya <- subset(base_graficos, Comunidad.autónoma == "PV")


"Desarrollo de graficos"

datos <- viscaya

# Con los datos de Viscaya se genera el gráfico
datos$fecha <- as.Date(datos$fecha)
datos <- datos[order(datos$fecha),]


x <- datos$fecha
y <- datos$num_casos


g <- aggregate(y, by=list(casos=x), FUN=sum)

"grafico 1"

# Color fondo aliceblue

plot(g, type ="l", xlab ="Fecha", ylab ="N°Casos", main ="Evolución Casos diarios", col = "blue")
polygon(g, col = "lightblue")
grid()
axis(side= 2, font=2)



"grafico 2"
plot(g, type = "s", 
     xtat = 'n', ylab = 'Numero casos', xlab = "Meses 2020",
     main = "Evolucion de casos COVID segun tipo de caso", beside = TRUE)


a <- aggregate(datos$num_casos_prueba_pcr, by=list(casos = datos$fecha), FUN = "sum")
b <- aggregate(datos$num_casos_prueba_otras, by= list(casos = datos$fecha), FUN = 'sum')
c <- aggregate(datos$num_casos_prueba_test_ac, by = list(casos = datos$fecha), FUN = 'sum')
d <- aggregate(datos$num_casos_prueba_desconocida, by = list(casos = datos$fecha), FUN = 'sum')
lines(a, col= 'red')
lines(b, col = 'blue')
lines(c, col = 'green')
lines(d,col = 'purple')

legend(x = "topleft",         # Posición
       title = "Tipo casos",
       legend = c("casos", "casos pcr", "casos otras", "casos ac", "desconocido"), # Textos de la leyenda
       lty = c(1),          # Tipo de líneas
       col = c(1, 'red', 'blue', 'green', 'purple'),          # Colores de las líneas
       lwd = 2)   
grid()
#Pregunta 3

"Paso 1 leer los datos de la tabla excel y del archivo txt"
tabla_excel <- read.xlsx("articulo.xlsx", sheetIndex = 1)
descuento <- read.delim("descuento_aplicar.txt")


"Paso 2 transformar la tabla excel y el archivo de texto a un data frame"
articulos <- data.frame(tabla_excel)
descuentos <- data.frame(descuento)

"Calculo de ingreso bruto"
articulos$ingreso_bruto <- articulos$PVP * articulos$CANTIDAD 

articulos[order(articulos$ingreso_bruto),]


"calcula deciles para utilizarlos en el calculo del tipo de descuento"

deciles <- quantile(ingreso_bruto,probs = seq(.1, .9, by = .1))

"Asigna tramo de descuento segun ingreso bruto y decil"
articulos$tipo <- with(articulos, ifelse(ingreso_bruto >= 6108000, 'A',
                                  ifelse(ingreso_bruto > 1797000 & ingreso_bruto < 6108000, "B",
                                  ifelse(ingreso_bruto <= 1797000, "C", 0))))

"Ordena el data frame de mayor a menor ingreso bruto"
articulos <- articulos[order(articulos$ingreso_bruto, decreasing = TRUE), ]



"Left join"
articulos2 <- articulos
articulos3 <- merge(articulos, descuento, by.x = "tipo", by.y = "tipo", all.x = TRUE)


"Crea el data frame clientes"

clientes <- articulos3

"Calcular la variable nuevo_pvp con el % de descuento correspondiente segun tramo"

clientes$nuevo_pvp <- with(clientes,ifelse(tipo == "A", clientes$PVP * 0.9,
                                             ifelse(tipo == "B", clientes$PVP * 0.85, 
                                            ifelse(tipo == "C", clientes$PVP * 0.8, 0))))

"Crea una nueva columna donde calcula el ingreso bruto por articulo con los valores actualizados"
clientes$n_ingreso_bruto <- clientes$CANTIDAD * clientes$nuevo_pvp

"calculo decremento"

decremento <- round(100 - ((sum(clientes$n_ingreso_bruto) / sum(clientes$ingreso_bruto)) * 100 ), digits = 2)

paste0("El porcentaje de decremento corresponde a un ", decremento, "%", ".")


sprintf("El porcentaje de decremento corresponde a un %s. ", decremento)
#write.xlsx(clientes, file= "clientes.xlsx")

#Ejercicio 4 


fichero <- source("matriz.R")
fichero <- fichero[[1]]

#El primer paso es limpiar y organizar los datos
#Para esto se toma el fichero y se realiza la cuenta de las ocurrencias por
#semana, del 1 al 4
base <- apply(fichero, 2, function(i) table(factor(i, levels = c(1:4))))

#Se quita la columna 1 pues no se utilizara para los calculos y ya esta la tabla limpia para trabajar
base <- base[,-1] # delete column 2


#En una variable se calcula el total de registros que tiene la tabla, son 16
# Es preferible calcularla en caso que se desee reutilizar este script
registros <- nrow(fichero)


#Calculo de frecuencias relativas para la matriz y se agregan nombres a las columnas 

matriz <- base / registros
colnames(matriz) <- c("s1", "s2", "s3", "s4", "s5", "s6", "s7")

# Tomando como base esta misma matriz, se crea una nueva para calcular
# 1−𝑓1 hasta 1−𝑓4, en el caso de operar con lapply esto convierte los datos
# en una lista por lo que se deben convertir a matrix nuevamente usando unlist


matriz2 <- matrix(unlist(lapply(1, `-`, matriz)), ncol = 7, nrow=4)
row.names(matriz2) <- c("1", "2", "3", "4")

#Una vez listas ambas matrices es momento de combinarlas
data <- rbind(matriz, matriz2)
data <- data[ order(as.numeric(row.names(data))), ]


original <- barplot(data,
        ylim=c(0,4),
        main="Evolución de la sensación de ardor con el tiempo",
        xlab='Semanas',
        ylab='Frecuencia relativa',
        col = c ("black", "white")
)


cambios <- barplot(data,
        ylim=c(0,4),
        xaxt = 'n',
        col = c ("red", "white"),
        col.axis = "blue",
        font.axis = 2
      
)

title("Evolución de la sensación de ardor con el tiempo", adj = 0.5, line = 3)
axis(3, at = seq(from = 0.7, to = 7.9,by = 1.2),labels = c("s1", "s2", "s3", "s4","s5", "s6", "s7"), 
     col.axis = "blue", font.axis = 2)

#cambios <- axis(side = 3, at = 1:7, labels = c(colnames(data)), tick = FALSE, cex.axis = 1)


