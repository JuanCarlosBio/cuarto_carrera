################################################################################
###########################  PRÁCTICA DE LOS TENEBRIOS  ########################
################################################################################

# Hola :), no se si eres alumno de biolgía ULL, o hablo a la nada pero bienvenido
# a mis lares de GitHub, supongo que estarás interesado en aprender R, enhorabuena,
# y suerte!, espero que este código (aunque no soy un experto en la materia) te 
# ayude en ese sentido. He añadido más cosas de lo que hice en la memoria en su 
# momento, por probar cosas nuevas, así que no hace falta que hagas todo lo que
# he hecho aquí, sino lo más básico basta. El experimento se hizo de verdad 
# (una práctica chula) así que estás manipulando datos biológicos reales Saqué un
#  9.5 en la nota, creo que ningún grupo llegó al 10

library(tidyverse)
library(readxl)
library(ggthemes)
library(glue)

#### Nuestros resultados para el grupo 4 de Oscuridad.
url1 <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/tenebrios/scripts_tenebrios/datos/tenebrios.csv"
result_o4 <- read_csv(url1);colnames(result_o4) <- c("semana",        # Semana de estudio
                                                     "masa.corporal", # valores anotados cada semana del peso de las larvas
                                                     "tasa.crec.s",   # Lo que crecen los tenebrios al semana tras semana
                                                     "tasa.crec.d")   # # Lo que crecen los tenebrios día a día (en promedio de los 7 días de la semana realmente)


# Comencemos con lo más básico, un estudio del aumento de la masa corporal y posteriormenete de la tasa de crecimeinto.
# Voy a crear un data frame con los valores estos valore en único vector (columna/variable), y poder graficar ambas en 
# único gráfico, ya que a R no le gusta usar varias columnas a la vez, generalmente solo se utilizan 2 variables.

masa_tasa_o4 <- result_o4[,c("semana","masa.corporal","tasa.crec.s")] %>%       # Filtramos las variables que nos interesa en este caso 
  pivot_longer(cols = -semana,names_to = "masa_tasa", values_to = "valores")    # (y lo llamamos de esta manera, no se cuan inexperto 
                                                        # eres, pero en R puedes asignar un valor a un objeto con
                                                        # "<-" o "=", te recomiendo en este caso acostumbrarte al 
                                                        # primero en R). Pues ya tendríamos un data frame con los 
                                                        # valores de masa y tasa en una columna, y otra que nos diga 
                                                        # si es la tasa o masa, igual si vienes de excel estás un poco 
                                                        # perdido, pero ahora verás por qué he hecho esto en el gráfico 


# Para el gráfico usaremos ggplot2, uno de los mejores paquetes para gráficos,
# El mejor con diferencia en mi opinión (mejores que los de matplotlib en python
# o los gráficos de Excel), se me da bastante bien este paquete, puedes iniciarte 
# en gráficas elaboradas de todo tipo en https://r-graph-gallery.com/, no hace falta
# que el gráfico sea tan elaborado, pero a mí me flipa diseñarlos. He de decir que
# la interpretación de los gráfico lo que dejo en sus manos, yo ya la hice en su 
# momento xd, no es mi intención en estos scripts

masa_tasa_o4 %>% 
  mutate(masa_tasa = factor(masa_tasa,
                            levels = c("masa.corporal", "tasa.crec.s"),             # De esta manera cambio los nombres a los grupos
                            labels = c("Masa Corporal (g)", "Tasa Crecimiento (g)"))) %>%    # que los otros no eran estéticos
  ggplot(aes(semana, valores, col=masa_tasa)) +
  geom_line(size=1, show.legend = F) +
  geom_point(size=1.75, show.legend = F) +
  facet_wrap(~masa_tasa, ncol=1, scales = "free",
             strip.position = "left") +
  labs(title = "Variación masa corporal y tasa de crecimiento",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semanas",
       y=NULL,
       col="Variación") +
  scale_x_continuous(limits = c(0,13),
                     breaks = seq(0,12,1)) +
  scale_color_manual(values = c("black","orange")) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(size=1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size = 11)
  )

# ggsave("Rplot05.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\tenebrios\\scripts_tenebrios\\graficas",
#       width = 8, height = 5)


# Los reultados anteriores de son los valores promedio de cada semana, el cuál es 
# el que hay que subir al aula virtual. En el siguiente data frame tengo los valores 
# de cada tenebrio con el paso de las semanas, para ver qué nos permite esto con 
# respecto al anterior

url2 <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/tenebrios/scripts_tenebrios/datos/df_o4.csv"
df_o4 <- read.csv(url2, sep = ";") 
colnames(df_o4) <- c("semana1","semana2","semana3","semana4","semana5","semana6",
                     "semana7","semana8","semana9","semana10","semana11")
df_o4_2 <- df_o4 %>%  
  pivot_longer(cols=starts_with("semana"),
               names_to = "semana_name", 
               values_to = "masa.corporal") %>% 
  mutate(masa.corporal = str_replace(masa.corporal, pattern = ",", replacement = "."),
         masa.corporal = as.numeric(masa.corporal)) %>% 
  group_by(semana_name) %>% 
  summarise(media_mc = mean(masa.corporal,na.rm=T),
            sd = sd(masa.corporal,na.rm = T)) %>%  # de esta manera obtenemos la desviación típica
  mutate(semana = case_when(semana_name == "semana1" ~ 1,
                            semana_name == "semana2" ~ 2,
                            semana_name == "semana3" ~ 3,
                            semana_name == "semana4" ~ 4,
                            semana_name == "semana5" ~ 5,
                            semana_name == "semana6" ~ 6,
                            semana_name == "semana7" ~ 7,
                            semana_name == "semana8" ~ 8,
                            semana_name == "semana9" ~ 9,
                            semana_name == "semana10" ~ 10,
                            semana_name == "semana11" ~ 11,
                            semana_name == "semana12" ~ 12)) %>% 
  arrange(semana) 

# Para sacar la tasa de crecimeinto he tenido algún que otro problema pero se me 
# ha ocurrido (no es tan fácil como en exce xd, lo bueno sería una combinación de
# ambos, pero ocurre una cosa, he estado escuchando a bioinformáticos que lo ideal
# es no midificar las bases datos original (el df_o4 excel por ejemplo), ya que impide que
# tu estudio sea reproducible, así que vamos ha improvisar en R como podamos.

na <- data.frame(na=NA)
tasa_crec <- data.frame(tasa.crec.s=df_o4_2[2:11,2]-df_o4_2[1:10,2]) # Con esta formula sacamos la tasa de crecimiento
# no podía añadirla en un mutate(), ya que como el vector
# presenta 2 valores menos, no se puede meter. Seguramente
df_o4_completo <- df_o4_2 %>%                                        # haya formas de hacerlo más smooth como la geometría, pero  
  mutate(tasa.crec.s = c(tasa_crec$media_mc,na$na),                  # sigo verde :(. Pero bueno, lo importante al final en 
         tasa.crec.d = tasa.crec.s/7,                                # programación es llegar al resultado :)
         dia=seq(0,70,7))              

# Pero por que he hecho esto?, por eso para la reproducibilidad, no modificar la 
# baes de datos original, y tener la desviación típica de la media, que faltaba en 
# los resultados originales. De esta manera al volver a hacer el mismo gráfico,
# podemos añadir las barras de error de la media en la masa corporal para que quede 
# más profesional y que se note que sabemos nuestra movida en cuanto a estadística.
# También he hecho un modelo de regresión lineal simple, pero tampoco es necesario.

# Correlación lineal de Pearson
cor.test(x=df_o4_completo$semana,
         y=df_o4_completo$media_mc,
         method = "pearson")      # p < 0.01*** las variables están correlacionadas, estimación = 0.99 La correlación extremadamente muy alta, nos da luz verde al modelo.

# Modelo de regresión lineal simple
modelo_o4 <- lm(media_mc~semana, data = df_o4_completo)
summary(modelo_o4) # p < 0.001*** es predictivo el modelo a = 0.004; b = 0.0115933

# De esta manera siguiendo la función de la recta y = bx + a --> media_mc = 0.012*semana + 0.004

coeficientes <- as.data.frame(modelo_o4$coefficients)
row.names(coeficientes) <- c("Intercepto", "Pendiente")
colnames(coeficientes) <- c("Resultados")
abline <- c(glue(coeficientes[1,]),         # Estas líneas de código con glue() es 
            glue(coeficientes[2,]))         # para que si cambio los números de arriba, 
# no tenga que volver a cambiarlos en el gráfico
text_num <- round(as.numeric(abline),3)
text <- c(glue("Masa orporal = {text_num[2]}·Semana + {text_num[1]}")) 

df_o4_completo %>% 
  ggplot(aes(semana, media_mc)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=media_mc+sd,ymax=media_mc-sd),width=.3) +  # Esto es lo que añadiríamos
  geom_point(size=1.75, pch= 21, fill="white") +
  labs(title = "Variación masa corporal y tasa de crecimiento",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semanas",
       y="Masa Corporal (g)",
       col="Variación") +
  scale_x_continuous(limits = c(1,12),
                     breaks = seq(1,12,1)) +
  geom_abline(intercept = as.numeric(abline[1]), slope = as.numeric(abline[2]),
              size=.75, col = "red") +
  geom_text(data = tibble(x=5, y=.15), 
            aes(x=x,y=y,label=text), inherit.aes = FALSE) +
  scale_y_continuous(limits = c(0,.18),
                     breaks = seq(0,.18,.03)) +
  scale_color_manual(values = c("black","orange")) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = c(.25,.8),
    legend.background = element_rect(color = "black"),
    legend.key = element_rect(fill="white"),
    legend.title = element_text(hjust = .5, size=13, face = "bold"),
    legend.text = element_text(size = 12)
  )

# ggsave("Rplot07.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\tenebrios\\scripts_tenebrios\\graficas",
#       width = 7, height = 4)


#### ¿Y existe correlación entre las variables masa y tasa? 

cor.test(x=df_o4_completo$media_mc,
         y=df_o4_completo$tasa.crec.s,
         method = "pearson")           # p > 0.05, las variables no están correlacionadas linealmente, estimación = -0.04, nula correlación lineal negativa








