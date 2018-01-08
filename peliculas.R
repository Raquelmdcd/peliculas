install.packages("jsonlite")
install.packages("dplyr")
library(jsonlite)
library("dplyr")
install.packages("tidyverse")
library(tidyverse)
peliculas <- read_csv(file="C:\\Users\\Marta\\Documents\\tmdb_5000_movies.csv",guess_max = 4803)
peliculasCredits <- read_csv(file="C:\\Users\\Marta\\Documents\\tmdb_5000_credits.csv",guess_max= 4803)
sapply(peliculas, function(x)(sum(is.na(x)))) # NA counts
sapply(peliculasCredits, function(x)(sum(is.na(x)))) # NA counts

# parte 1
#Eliminación del registro 
posicion <-which(is.na(peliculas$release_date))
registro <-peliculas[posicion,]
peliculasCreditsRegistro <- which(peliculasCredits$movie_id ==registro$id)
peliculasCredits <- peliculasCredits[-c(peliculasCreditsRegistro),]
peliculas <- peliculas[-c(posicion),]

# Media de los valores vacios en runtime
summary(peliculas)
posicionRunTime <- which(is.na(peliculas$runtime))
peliculas[posicionRunTime[1],14] <- 106.9
peliculas[posicionRunTime[2],14] <- 106.9

peliculasFiltro <- peliculas[, c(1,4,9,12,13,14,19,20)]

peliculasFiltro <-mutate(peliculasFiltro, beneficios = peliculasFiltro$revenue - peliculasFiltro$budget)

#conBeneficios <-filter(peliculasFiltro, beneficios>0)
#sinBeneficios <-filter(peliculasFiltro, beneficios<=0)

plot(peliculasFiltro$vote_count)

#Parte 2. generación datasets para el estudio de los datos

keywords <- peliculas %>%    
  filter(nchar(keywords)>2) %>%    
  mutate(                           
    js = lapply(keywords, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, keyword=name)   
slice(keywords, sample(1:4800, 5))

generos <- peliculas %>%    
  filter(nchar(genres)>2) %>%    
  mutate(                           
    js = lapply(genres, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, genre=name)   
slice(generos, sample(1:4800, 5))

lenguas <- peliculas %>%    
  filter(nchar(spoken_languages)>2) %>%    
  mutate(                           
    js = lapply(spoken_languages, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, spoken_languages=name)   


productoras <- peliculas %>%    
  filter(nchar(production_companies)>2) %>%    
  mutate(                           
    js = lapply(production_companies, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, production_companies=name)   


paises <- peliculas %>%    
  filter(nchar(production_countries)>2) %>%    
  mutate(                           
    js = lapply(production_countries, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, production_countries=name)   

reparto <- peliculasCredits %>%    
  filter(nchar(cast)>2) %>%    
  mutate(                           
    js = lapply(cast, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, cast=name)   


equipo<- peliculasCredits %>%    
  filter(nchar(crew)>2) %>%    
  mutate(                           
    js = lapply(crew, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, job, crew=name)   



directoresPelicula <- equipo %>%         
  filter(job=="Director") %>%               
  mutate(director=crew) %>%                 
  left_join(                                
    peliculasFiltro,                         
    by=c("id" = "id")                
  ) 

directores <- directoresPelicula %>% 
  group_by(director) %>%                            
  filter(vote_count>10) %>%                         
  summarise(         
    n = n(),                                        
    weighted_mean_votos = 
      weighted.mean(vote_average, vote_count),
    weighted_mean_beneficios = weighted.mean(beneficios),
    weighted_mean_popularidad = weighted.mean(popularity)) %>%  
  filter(n>1) %>%                                  
  arrange(desc(weighted_mean_popularidad,weighted_mean_votos, weighted_mean_beneficios))                      

# actores más populares
actoresPelicula <- reparto %>%         
  mutate(actor=cast) %>%                 
  left_join(                                
    peliculasFiltro,                         
    by=c("id" = "id")                
  ) 

actores <- actoresPelicula %>% 
  group_by(actor) %>%                            
  filter(vote_count>10) %>%                         
  summarise(         
    n = n(),                                        
    weighted_mean_votos = 
      weighted.mean(vote_average, vote_count),
    weighted_mean_beneficios = weighted.mean(beneficios),
    weighted_mean_popularidad = weighted.mean(popularity)) %>%  # and the weighted average votes
  filter(n>1) %>%                                   # and filter out directors of 1 title
  arrange(desc(weighted_mean_popularidad,weighted_mean_votos, weighted_mean_beneficios))                      


# generos más populares
generosPelicula <- generos %>%         
  mutate(genero=genre) %>%                 
  left_join(                                
    peliculasFiltro,                         
    by=c("id" = "id")                
  ) 

generosPopulares <- generosPelicula %>% 
  group_by(genero) %>%                            
  filter(vote_count>10) %>%                         
  summarise(         
    n = n(),                                        
    weighted_mean_votos = 
      weighted.mean(vote_average, vote_count),
    weighted_mean_beneficios = weighted.mean(beneficios),
    weighted_mean_popularidad = weighted.mean(popularity)) %>%  # and the weighted average votes
  filter(n>1) %>%                                   # and filter out directors of 1 title
  arrange(desc(weighted_mean_popularidad,weighted_mean_votos, weighted_mean_beneficios))                      

productorasPelicula <- productoras %>%         
  mutate(productora=production_companies) %>%                 
  left_join(                                
    peliculasFiltro,                         
    by=c("id" = "id")                
  ) 

productorasPopulares <- productorasPelicula %>% 
  group_by(productora) %>%                            
  filter(vote_count>10) %>%                         
  summarise(         
    n = n(),                                        
    weighted_mean_votos = 
      weighted.mean(vote_average, vote_count),
    weighted_mean_beneficios = weighted.mean(beneficios),
    weighted_mean_popularidad = weighted.mean(popularity)) %>%  # and the weighted average votes
  filter(n>1) %>%                                   # and filter out directors of 1 title
  arrange(desc(weighted_mean_popularidad,weighted_mean_votos, weighted_mean_beneficios))                      

#guardamos los datos en un fichero de salida 
write.table(directores, file = "C:\\Users\\Marta\\Documents\\directores.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

#guardamos los datos en un fichero de salida 
write.table(actores, file = "C:\\Users\\Marta\\Documents\\actores.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

#guardamos los datos en un fichero de salida 
write.table(generosPopulares, file = "C:\\Users\\Marta\\Documents\\generos.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

#guardamos los datos en un fichero de salida 
write.table(productorasPopulares, file = "C:\\Users\\Marta\\Documents\\productoras.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))



# Comprobación normalidad
install.packages("nortest")
library(nortest)
require(nortest)

plot(density(directores$weighted_mean_votos))
# test Anderson-Darling
ad.test(directores$weighted_mean_votos)$p.value
# test Cramer-von Mises
cvm.test(directores$weighted_mean_votos)$p.value
# test Shapiro-Wilk
shapiro.test(directores$weighted_mean_votos)$p.value
#test de Lilliefors
lillie.test(directores$weighted_mean_votos)$p.value
# test de Pearson chi-square
pearson.test(directores$weighted_mean_votos)$p.value
# test de Shapiro-Francia
sf.test(directores$weighted_mean_votos)$p.value
# Todas tienen valores muy pequeños de p, por lo que rechazan la H0. No siguen distribución normal
plot(density(directores$weighted_mean_beneficios))
# test Anderson-Darling
ad.test(directores$weighted_mean_beneficios)$p.value
# Todas tienen valores muy pequeños de p, por lo que rechazan la H0. No siguen distribución normal
plot(density(directores$weighted_mean_popularidad))
# test Anderson-Darling
ad.test(directores$weighted_mean_popularidad)$p.value

plot(density(actores$weighted_mean_votos))
# test Anderson-Darling
pearson.test(actores$weighted_mean_votos)$p.value
plot(density(actores$weighted_mean_beneficios))
# test Anderson-Darling
ad.test(actores$weighted_mean_beneficios)$p.value
plot(density(actores$weighted_mean_popularidad))
# test Anderson-Darling
ad.test(directores$weighted_mean_popularidad)$p.value

plot(density(generosPopulares$weighted_mean_votos))
# test Anderson-Darling
cvm.test(generosPopulares$weighted_mean_votos)$p.value
plot(density(generosPopulares$weighted_mean_beneficios))
# test Anderson-Darling
cvm.test(generosPopulares$weighted_mean_beneficios)$p.value
ad.test(generosPopulares$weighted_mean_beneficios)$p.value
pearson.test(generosPopulares$weighted_mean_beneficios)$p.value

plot(density(generosPopulares$weighted_mean_popularidad))
# test Anderson-Darling
ad.test(generosPopulares$weighted_mean_popularidad)$p.value
pearson.test(generosPopulares$weighted_mean_popularidad)$p.value
cvm.test(generosPopulares$weighted_mean_popularidad)$p.value


plot(density(productorasPopulares$weighted_mean_votos))
# test Anderson-Darling
ad.test(productorasPopulares$weighted_mean_votos)$p.value
cvm.test(productorasPopulares$weighted_mean_votos)$p.value
plot(density(productorasPopulares$weighted_mean_beneficios))
# test Anderson-Darling
ad.test(productorasPopulares$weighted_mean_beneficios)$p.value
plot(density(productorasPopulares$weighted_mean_popularidad))
# test Anderson-Darling
ad.test(productorasPopulares$weighted_mean_popularidad)$p.value



# test Anderson-Darling
ad.test(peliculasFiltro$vote_count)$p.value
# test Cramer-von Mises
cvm.test(peliculasFiltro$vote_count)$p.value
# test Shapiro-Wilk
shapiro.test(peliculasFiltro$vote_count)$p.value
#test de Lilliefors
lillie.test(peliculasFiltro$vote_count)$p.value
# test de Pearson chi-square
pearson.test(peliculasFiltro$vote_count)$p.value
# test de Shapiro-Francia
sf.test(peliculasFiltro$vote_count)$p.value
# Todas tienen valores muy pequeños de p, por lo que rechazan la H0. No siguen distribución normal

plot(density(peliculasFiltro$beneficios))
# test Anderson-Darling
ad.test(peliculasFiltro$beneficios)$p.value
# test Cramer-von Mises
cvm.test(peliculasFiltro$beneficios)$p.value
# test Shapiro-Wilk
shapiro.test(peliculasFiltro$beneficios)$p.value
#test de Lilliefors
lillie.test(peliculasFiltro$beneficios)$p.value
# test de Pearson chi-square
pearson.test(peliculasFiltro$beneficios)$p.value
# test de Shapiro-Francia
sf.test(peliculasFiltro$beneficios)$p.value


# test Anderson-Darling
ad.test(peliculasFiltro$popularity)$p.value
# test Cramer-von Mises
cvm.test(peliculasFiltro$popularity)$p.value
# test Shapiro-Wilk
shapiro.test(peliculasFiltro$popularity)$p.value
#test de Lilliefors
lillie.test(peliculasFiltro$popularity)$p.value
# test de Pearson chi-square
pearson.test(peliculasFiltro$popularity)$p.value
# test de Shapiro-Francia
sf.test(peliculasFiltro$popularity)$p.value



#buscamos extreme scores en cuanto a popularidad,por la gráfica vemos 6
plot(peliculasFiltro$popularity)
# creamos un data frame auxiliar sin extremes scores de la popularidad
peliculasFiltroOrder <- arrange(peliculasFiltro, desc(peliculasFiltro$popularity))
peliculasFiltroOrderDel <- peliculasFiltroOrder[-c(1, 2, 3, 4, 5, 6), ]
# comprobamos si quitando estos valores, sigue una normal y vemos que tampoco
ad.test(peliculasFiltroOrderDel$popularity)$p.value

plot(peliculasFiltroOrderDel$popularity)


# hacemos los test de homogeneidad de varianzas
# vamos a separar por grupos con o sin beneficios
# añadimos una nueva columna que nos indique si hay o no beneficios en la película
peliculasFiltro <- mutate(peliculasFiltro, swBeneficios = peliculasFiltro$beneficios > 0)

#homogeneidad varianzas
install.packages("car")
require(car)
library("dplyr")
# hacemos los test de homogeneidad de varianzas
# vamos a separar por grupos con o sin beneficios
# añadimos una nueva columna que nos indique si hay o no beneficios en la película
peliculasFiltro <- mutate(peliculasFiltro, swBeneficios = peliculasFiltro$beneficios > 0)
# en este caso, el método que nos sirve es el fligner test, puesto que los datos no
# siguen una distribución normal
fligner.test(peliculasFiltro$vote_count~peliculasFiltro$swBeneficios, peliculasFiltro)
# p tiene un valor muy pequeño, así que tampoco hay homogeneidad en las varianzas
# probamos otros métodos y observamos que se ve el mismo resultado
bartlett.test(peliculasFiltro$vote_count, peliculasFiltro$swBeneficios, peliculasFiltro)
bartlett.test(peliculasFiltro$vote_count~peliculasFiltro$swBeneficios, peliculasFiltro)
leveneTest(peliculasFiltro$vote_count~peliculasFiltro$swBeneficios, peliculasFiltro)





#Parte 3
install.packages("ggplot2")
library(ggplot2)
#histogramas y QQ plot
hist(directores$weighted_mean_votos, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(directores$weighted_mean_votos, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(directores) + stat_qq(aes(sample=weighted_mean_votos))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de voto medio ponderado por director")

hist(directores$weighted_mean_beneficios, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(directores$weighted_mean_beneficios, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(directores) + stat_qq(aes(sample=weighted_mean_beneficios))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de beneficios medios ponderados por director")

hist(directores$weighted_mean_popularidad, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(directores$weighted_mean_popularidad, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(directores) + stat_qq(aes(sample=weighted_mean_popularidad))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de popularidad medios ponderados por director")


hist(actores$weighted_mean_votos, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(actores$weighted_mean_votos, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(actores) + stat_qq(aes(sample=weighted_mean_votos))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de voto medio ponderado por actor")

hist(actores$weighted_mean_beneficios, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(actores$weighted_mean_beneficios, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(actores) + stat_qq(aes(sample=weighted_mean_beneficios))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de beneficios medios ponderados por actor")

hist(actores$weighted_mean_popularidad, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(actores$weighted_mean_popularidad, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(actores) + stat_qq(aes(sample=weighted_mean_popularidad))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de popularidad media ponderada por actor")


hist(generosPopulares$weighted_mean_votos, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(generosPopulares$weighted_mean_votos, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(generosPopulares) + stat_qq(aes(sample=weighted_mean_votos))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de voto medio ponderado por género")

hist(generosPopulares$weighted_mean_beneficios, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(generosPopulares$weighted_mean_beneficios, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(generosPopulares) + stat_qq(aes(sample=weighted_mean_beneficios))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de beneficio medio ponderado por género")

hist(generosPopulares$weighted_mean_popularidad, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(generosPopulares$weighted_mean_popularidad, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(generosPopulares) + stat_qq(aes(sample=weighted_mean_popularidad))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de popularidad media ponderado por género")

hist(productorasPopulares$weighted_mean_votos, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(productorasPopulares$weighted_mean_votos, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(productorasPopulares) + stat_qq(aes(sample=weighted_mean_votos))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de voto medio ponderado por productora")

hist(productorasPopulares$weighted_mean_beneficios, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(productorasPopulares$weighted_mean_beneficios, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(productorasPopulares) + stat_qq(aes(sample=weighted_mean_beneficios))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de beneficio medio ponderado por productora")

hist(productorasPopulares$weighted_mean_popularidad, xlab="Peso", ylab="Frecuencia", las=1, main="")
y <- quantile(productorasPopulares$weighted_mean_popularidad, c(0.25, 0.75)) 
x     <- qnorm( c(0.25, 0.75))
slope <- diff(y) / diff(x) 
int   <- y[1] - slope * x[1]
ggplot(productorasPopulares) + stat_qq(aes(sample=weighted_mean_popularidad))  + 
  geom_abline(intercept=int, slope=slope, color="blue") + 
  labs(title="QQ plot de popularidad media ponderado por productora")


sapply(keywords, function(x)(sum(is.na(x)))) # NA counts
sapply(generos, function(x)(sum(is.na(x)))) # NA counts
sapply(lenguas, function(x)(sum(is.na(x)))) # NA counts
sapply(productoras, function(x)(sum(is.na(x)))) # NA counts
sapply(paises, function(x)(sum(is.na(x)))) # NA counts
sapply(reparto, function(x)(sum(is.na(x)))) # NA counts
sapply(equipo, function(x)(sum(is.na(x)))) # NA counts





## Correlacion actores

#Para mejorar la confianza del algoritmo vamos a quitar algunos extreme score
plot(actores$weighted_mean_beneficios, actores$weighted_mean_popularidad)
posicionBene <- which(actores$weighted_mean_beneficios >900000000)
registroAct <-actores[posicionBene,]
actores <- actores[-c(posicionBene),]
posicionPopu <- which(actores$weighted_mean_popularidad >200)
actores <- actores[-c(posicionPopu),]
plot(actores$weighted_mean_beneficios, actores$weighted_mean_popularidad)
datosactores <- actores[,-c(1)]
#podemos ver que existe correlacion de la popularidad con los beneficios de un 63%
cor(datosactores)
#la representacion de las correlaciones
pairs(datosactores)
plot(actores$weighted_mean_beneficios, actores$weighted_mean_popularidad)
regresion <- lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data = actores)
# Errores errores estimados: 186419 
par(mfrow=c(2,2))
plot(lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data =actores))
#residuals vs Fietted: muestra los datos aleatorios 
#Normal Q-Q se si siguen la raya es que son normales
# scale-Localtion: Busca elasticidad muestra la tendencia
#Residuals vs Leverage realiza una estimacion todos los datos que no estan cerca de la linea no estan dando valor

summary(regresion)
#Bondad de ajuste, esta es signidicativa H0 no hay correlacion, hay una correlacion diferende de 0
anova(regresion)
# si queremos la representaciÃ³n de los datos
plot(actores$weighted_mean_beneficios, actores$weighted_mean_popularidad)
abline(regresion)
datos <- data.frame(weighted_mean_popularidad =(c(80000,70000)))
predict(regresion,  datos)
predict(regresion, data.frame(datos), level = 0.95, interval = "confidence")



## Corelacion directores
#Para mejorar la confianza del algoritmo vamos a quitar algunos extreme score
plot(directores$weighted_mean_beneficios, directores$weighted_mean_popularidad)


datosdirectores <- directores[,-c(1)]
#podemos ver que existe correlacion de la popularidad con los beneficios de un 63%
cor(datosdirectores)
#la representacion de las correlaciones
pairs(datosactores)
plot(directores$weighted_mean_beneficios, directores$weighted_mean_popularidad)
regresion <- lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data = directores)
par(mfrow=c(2,2))
plot(lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data =directores))
#residuals vs Fietted: muestra los datos aleatorios 
#Normal Q-Q se si siguen la raya es que son normales
# scale-Localtion: Busca elasticidad muestra la tendencia
#Residuals vs Leverage realiza una estimacion todos los datos que no estan cerca de la linea no estan dando valor

summary(regresion)
#Bondad de ajuste, esta es signidicativa H0 no hay correlacion, hay una correlacion diferende de 0
anova(regresion)
# si queremos la representaciÃ³n de los datos
plot(actores$weighted_mean_beneficios, actores$weighted_mean_popularidad)
datos <- data.frame(weighted_mean_popularidad =(c(80000,70000)))
predict(regresion,  datos)
predict(regresion, data.frame(datos), level = 0.95, interval = "confidence")


## Corelacion genero
#Para mejorar la confianza del algoritmo vamos a quitar algunos extreme score
plot(generosPopulares$weighted_mean_beneficios, generosPopulares$weighted_mean_popularidad)
datosgeneros <- generosPopulares[,-c(1)]
cor(datosgeneros)
#la representacion de las correlaciones
pairs(datosactores)
plot(generosPopulares$weighted_mean_beneficios, generosPopulares$weighted_mean_popularidad)
regresion <- lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data = generosPopulares)
par(mfrow=c(2,2))
plot(lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data =generosPopulares))
#residuals vs Fietted: muestra los datos aleatorios 
#Normal Q-Q se si siguen la raya es que son normales
# scale-Localtion: Busca elasticidad muestra la tendencia
#Residuals vs Leverage realiza una estimacion todos los datos que no estan cerca de la linea no estan dando valor

summary(regresion)
#Bondad de ajuste, esta es signidicativa H0 no hay correlacion, hay una correlacion diferende de 0
anova(regresion)
# si queremos la representaciÃ³n de los datos
plot(generosPopulares$weighted_mean_beneficios, generosPopulares$weighted_mean_popularidad)
datos <- data.frame(weighted_mean_popularidad =(c(80000,70000)))
predict(regresion,  datos)
predict(regresion, data.frame(datos), level = 0.95, interval = "confidence")



## Corelacion Productora
#Para mejorar la confianza del algoritmo vamos a quitar algunos extreme score
plot(productorasPopulares$weighted_mean_beneficios, productorasPopulares$weighted_mean_popularidad)

posicionBene <- which(productorasPopulares$weighted_mean_beneficios >900000000)
productorasPopulares <- productorasPopulares[-c(posicionBene),]
posicionPopu <- which(productorasPopulares$weighted_mean_popularidad >142.46500)
productorasPopulares <- productorasPopulares[-c(posicionPopu),]
datosproductora <- productorasPopulares[,-c(1)]
cor(datosproductora)
#la representacion de las correlaciones
pairs(datosactores)
plot(generosPopulares$weighted_mean_beneficios, generosPopulares$weighted_mean_popularidad)
regresion <- lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data = generosPopulares)
par(mfrow=c(2,2))
plot(lm(weighted_mean_beneficios ~ weighted_mean_popularidad, data =generosPopulares))
#residuals vs Fietted: muestra los datos aleatorios 
#Normal Q-Q se si siguen la raya es que son normales
# scale-Localtion: Busca elasticidad muestra la tendencia
#Residuals vs Leverage realiza una estimacion todos los datos que no estan cerca de la linea no estan dando valor

summary(regresion)
#Bondad de ajuste, esta es signidicativa H0 no hay correlacion, hay una correlacion diferende de 0
anova(regresion)
# si queremos la representaciÃ³n de los datos
plot(generosPopulares$weighted_mean_beneficios, generosPopulares$weighted_mean_popularidad)
datos <- data.frame(weighted_mean_popularidad =(c(100000,90000)))
predict(regresion,  datos)
predict(regresion, data.frame(datos), level = 0.95, interval = "confidence")


