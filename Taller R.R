library(tidyverse)
library(lubridate)
library(stringr)
library(VIM)
library(mice)

hurto <- read.csv2("C:/Users/cmartinr/Desktop/Documentos personales/R/hurto_a_establecimiento_comercial.csv",encoding = 'UTF-8',na.strings = c("Sin dato","NaN"))
head(hurto)

attach(hurto)
nombres <- names(hurto)
nombres <- str_replace_all(nombres, "seguridad.", "")
names(hurto) <- nombres
str(hurto)

#Cambio de variables-----

hurto$fecha_hecho <- dmy_hm(hurto$fecha_hecho)
hurto$cantidad <- as.integer(hurto$cantidad)
hurto$latitud <- as.numeric(hurto$latitud)
hurto$longitud <- as.numeric(hurto$longitud)

plo <- aggr(hurto,numbers=T,sortVar=T)
mis <- plo$missings
class(mis)

mis <- mis %>% mutate(p = mis$Count/sum(nrow(hurto)))
b_variable <- mis %>% filter(p < 0.6) %>% select(Variable)
hurto <- hurto[,b_variable$Variable]
class(b_variable)
plo <- aggr(hurto,numbers=T,sortVar=T)


tipo <- sapply(hurto, class)
tipo <- (as.data.frame(tipo))
tipo <- cbind(tipo,variables = rownames(tipo))
rownames(tipo) <- NULL
index_entero <- which(tipo$tipo == 'integer')
entero <- tipo[index_entero, 2]
index_factor <- which(tipo$tipo == 'factor')
factor <- tipo[index_factor, 2]
index_numero <- which(tipo$tipo == 'numeric')
numero <- tipo[index_numero, 2]

imputed_data <- mice(hurto[, names(hurto) %in% entero], seed=2018,print = F,m = 1)
integer_complete<- mice::complete(imputed_data)

rand.imput <-function(x){
  missing <- (is.na(x)) #vector booleano
  n.missing <- sum(missing)#Numero de NA's
  x.obs <- x[!missing]#Datos no NA
  imputed <- x
  imputed[missing] <- sample(x.obs,n.missing,replace = T)
  #Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
  return(imputed)
}

factor_complete <- sapply(hurto[,names(hurto) %in% factor], rand.imput)
hurto_complete <- cbind(factor_complete, integer_complete, hurto$latitud, hurto$longitud)
head(hurto_complete)



hurto_complete$conducta <- NULL
hurto_complete$codigo_barrio <- NULL
hurto_complete$codigo_comuna <- NULL
hurto_complete$modelo <- NULL
hurto_complete$cantidad<- NULL


### gráfico ggplot para medio de transporte

table(hurto_complete$medio_transporte)



library(dplyr)
  seg <- hurto_complete %>%  group_by(medio_transporte = hurto_complete$medio_transporte) %>% count()


plot <- ggplot(seg, aes(x = medio_transporte,y  =n, label = scales::comma(n)))

plot + geom_bar(stat = "identity", show.legend = T) + 
  scale_alpha_continuous(labels = scales::comma) + 
  geom_text(size = 5 , position = position_stack(vjust = 0.5), col = 3) 

 ### Organizar gráfico


## eliminar un -1 en edad
hurto_complete$edad[hurto_complete$edad== -1] <- NA
head(hurto_complete)
 
table(hurto$edad)

##histograma de la edad
hist(hurto_complete$edad)
##ponerlo dinamico y en ggplot
ggplot(hurto_complete$edad), aes (x = edad), 


##relacion de edad y lugares
ggplot(hurto_complete, aes(x=sede_receptora, y =edad)) + geom_boxplot()


###grafico para relacion de categoria_bien 

table(hurto_complete$categoria_bien)

#diagrama de barras
### actualizar mi GITHUB

