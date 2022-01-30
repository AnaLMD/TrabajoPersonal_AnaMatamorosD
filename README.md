# Trabajopersonal_AnaMatamorosD

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

nycflights13 es un dataset que contiene información sobre vuelos de US. Si es necesario instala el paquete. Crear un nuevo proyecto de R con el nombre TrabajoPersonal. Recordar que debéis enviar tanto el fichero RMD y html comprimidos en un zip. Por favor, seguir el formato siguiente para entregar el trabajo: TrabajoPersonal_NombreApellido.zip.

```{r}
#install.packages("nycflights13")
library(nycflights13)
library(dplyr)
```

## 1. Encuentra todos los vuelos que llegaron más de una hora tarde de lo previsto.
Como el dataframe tiene una columna que contiene aquellos vuelos que fueron atrasados en la salida o en la llegada expresada en minutos, se puede hacer lo siguiente. 
(negativos (-)) significan early departure o early arrival. 

```{r}
nycflights13::flights
flights <- nycflights13::flights
?flights
arr_delay_60 <- flights[flights$arr_delay > 60, ]
View(arr_delay_60)
```

## 2. Encuentra todos los vuelos que volaron hacia San Francisco (aeropuertos SFO y OAK)

```{r}
toSF <- filter(flights, dest == "SFO" | dest == "OAK")
View(toSF)
```

## 3. Encuentra todos los vuelos operados por United American (UA) o por American Airlines (AA)

```{r}
operac <- filter(flights, carrier == "UA" | carrier == "AA")
View(operac)
```


## 4. Encuentra todos los vuelos que salieron los meses de primavera (Abril, Mayo y Junio)
Los meses de primavera son: 4, 5 y 6 
```{r}
primav <- filter(flights, month == 4 | month == 5 | month == 6)
View(primav)
```

## 5. Encuentra todos los vuelos que llegaron más de una hora tarde pero salieron con menos de una hora de retraso.

```{r}
delaybutok <- filter(flights, arr_delay > 60, dep_delay < 60)
View(delaybutok)
```

## 6. Encuentra todos los vuelos que salieron con más de una hora de retraso pero consiguieron llegar con menos de 30 minutos de retraso (el avión aceleró en el aire)

```{r}
delagain <- filter(flights, dep_delay > 60 & arr_delay < 30)
View(delagain)
```

## 7. Encuentra todos los vuelos que salen entre medianoche y las 7 de la mañana (vuelos nocturnos).

```{r}
nightflights <- filter(flights, dep_time <= 7 | dep_time == 24)
View(nightflights)
```

## 8. ¿Cuántos vuelos tienen un valor desconocido de dep_time?

```{r}
table(is.na(flights$dep_time))
```
8255 vuelos tienen un valor desconocido de departure time 

## 9. ¿Qué variables del dataset contienen valores desconocidos?

```{r}
summary(is.na(flights))
```


## 10. Ordena los vuelos de flights para encontrar los vuelos más retrasados en la salida. 

```{r}
arrange(flights,desc(dep_delay))
```

## ¿Qué vuelos fueron los que salieron los primeros antes de lo previsto?
```{r}
sort(flights$dep_delay, decreasing = TRUE, na.last = TRUE)
arrange(flights,(dep_delay))
```

## 11. Ordena los vuelos de flights para encontrar los vuelos más rápidos. Usa el concepto de rapidez que consideres.

```{r}
fasttrip <- filter(flights, air_time < 60)
View(fasttrip)
arrange(flights, (air_time))
```

## 12. ¿Qué vuelos tienen los trayectos más largos?

```{r}
sort(flights$distance, decreasing = TRUE)
```

## 13. ¿Qué vuelos tienen los trayectos más cortos?

```{r}
sort(flights$distance, decreasing = FALSE)
```


## 14. El dataset de vuelos tiene dos variables, dep_time y sched_dep_time muy útiles pero difíciles de usar por cómo vienen dadas al no ser variables continuas. Fíjate que cuando pone 559, se refiere a que el vuelo salió a las 5:59... Convierte este dato en otro más útil que represente el número de minutos que pasan desde media noche.

```{r}
flights$departime<-(flights$dep_time %/% 100 * 60) + (flights$dep_time %% 100)
flights$departdelay<-(flights$sched_dep_time %/% 100 * 60) + (flights$sched_dep_time %% 100)

flights_times <- mutate(flights,
  dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
  sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
    sched_dep_time %% 100) %% 1440
)

time2mins <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}

flights_times <- mutate(flights,
  dep_time_mins = time2mins(dep_time),
  sched_dep_time_mins = time2mins(sched_dep_time)
)

```

## 15. Compara los valores de dep_time, sched_dep_time y dep_delay. ¿Cómo deberían relacionarse estos tres números? Compruébalo y haz las correcciones numéricas que necesitas.
Dep_time es la hora real a la que el avión parte, Sched_dep_time es la hora a la que estaba planeado partir y Dep_delay son los minutos que se demora en salir el avion en función de la hora prevista. Por lo tanto, es la resta entre la hora de salida y la planeada.


## 16. Investiga si existe algún patrón del número de vuelos que se cancelan cada día.

```{r}
unknown <- filter(flights, is.na(dep_time))

unk_carrier <- table(unknown$carrier)
barplot(unk_carrier, main = "Cancelaciones según la Compañía Aérea", col = 1:20, legend.text = T)

unk_day <- table(unknown$day)
barplot(unk_day, main = "Cancelaciones segun el dia", col = 1:30)

unk_dest <- table(unknown$dest)
barplot(unk_dest, main = "cancelaciones por destino", col = 1:30)
```

La aerolinea que mas cancelaciones hace es la EV
El dia del mes que hay mas cancelaciones es el dia 8 del mes
El destino que mas cancelaciones tiene es ORF


## 17. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por día en los vuelos.

```{r}
probtardy_cancel <- 
  flights %>%
  mutate(canceled = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(canceled_prob = mean(canceled),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

ggplot(probtardy_cancel) +
  geom_point(aes(x = med_dep_delay, y = canceled_prob, col=canceled_prob))

ggplot(probtardy_cancel) +
  geom_point(aes(x = med_arr_delay, y = canceled_prob, col=canceled_prob))
```


## 18. Investiga si la proporción de vuelos cancelados está relacionada con el retraso promedio por aeropuerto en los vuelos.

```{r}
probtard_cancel_aer <- 
  flights %>%
  mutate(canceled = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(canceled_prob = mean(canceled),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()


ggplot(probtard_cancel_aer) +
  geom_point(aes(x = med_dep_delay, y = canceled_prob, col=canceled_prob))


ggplot(probtard_cancel_aer) +
  geom_point(aes(x = med_arr_delay, y = canceled_prob, col=canceled_prob))
```


## 19. ¿Qué compañía aérea sufre los peores retrasos?

```{r}
delays <- filter(flights, dep_delay < 0 | arr_delay < 0)
getmode <- function(v) {
     uniqv <- unique(v)
     uniqv[which.max(tabulate(match(v, uniqv)))]
 }
result <- getmode(delays$carrier)
print(result)
```


## 20. Queremos saber qué hora del día nos conviene volar si queremos evitar los retrasos en la salida.

```{r}
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```
Las horas que salen en negativo son aquellas horas que tuvieron menos delay, por lo que se recomienda volar entre las 5-9am. 

## 21. Queremos saber qué día de la semana nos conviene volar si queremos evitar los retrasos en la salida.

```{r}
library(lubridate)

make_datetime_100 <- function(year, month, day, time) {
   make_datetime(year, month, day, time %/% 100, time %% 100)
 }
flights_dt <- flights %>% 
   filter(!is.na(dep_time), !is.na(arr_time)) %>% 
   mutate(
     dep_time = make_datetime_100(year, month, day, dep_time),
     arr_time = make_datetime_100(year, month, day, arr_time),
     sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
     sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
   ) %>% 
   select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
   mutate(dow = wday(sched_dep_time)) %>%
   group_by(dow) %>%
   summarise(
     dep_delay = mean(dep_delay),
     arr_delay = mean(arr_delay, na.rm = TRUE)
   ) %>%
   print(n = Inf)
```

Se recomienda salir los dias 4, 6, 15 o 29 de cada mes. 

## 22. Para cada destino, calcula el total de minutos de retraso acumulado.

```{r}
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  
select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
arrange(dest, desc(arr_delay_prop))
```


## 23. Para cada uno de ellos, calcula la proporción del total de retraso para dicho destino.

```{r}
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop)
```


## 24. Es hora de aplicar todo lo que hemos aprendido para visualizar mejor los tiempos de salida para vuelos cancelados vs los no cancelados. Recuerda bien qué tipo de dato tenemos en cada caso. ¿Qué deduces acerca de los retrasos según la hora del día a la que está programada el vuelo de salida?

```{r}
flights_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()
```

## 25. Subir la carpeta a github y facilitar la url.
## 26. Al finalizar el documento agrega el comando sessionInfo()
```{r}
sessionInfo()
```


