###############################################################################
############################ Tenebrios Todos los grupos ########################
################################################################################

# Esta función me abre corre un script que tengo en GitHub con funciones de inferencia estadística
source("https://raw.githubusercontent.com/Juankkar/mis_cosas/main/funciones_propias/inferencia.R")

library(readxl)      
library(readr)
library(tidytext)
library(ggtext)
library(glue)

url <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/tenebrios/scripts_tenebrios/datos/tene_todos_crudos.csv"
tene_todos_crudos <- read.csv(url,sep = ";") # madre de dios la limpieza lloro ); sea todo por la ciencia
ten_tod2 <- tene_todos_crudos[1:42,]

ten_tod3 <- ten_tod2[-c(15,30), -seq(0,35,6)] 

separar <- function(df,x,y) {
  cuerpo <- df[,c(x:y)]
  return(cuerpo)
}

nombres_grupos <- c("grupo","semana","masa_corp","d","dx")

uno <- separar(ten_tod3,1,5)
colnames(uno) <- nombres_grupos
dos <- separar(ten_tod3,6,10)
colnames(dos) <- nombres_grupos
tre <- separar(ten_tod3,11,15)
colnames(tre) <- nombres_grupos
cua <- separar(ten_tod3,16,20)
colnames(cua) <- nombres_grupos
cin <- separar(ten_tod3,21,25)
colnames(cin) <- nombres_grupos
sei <- separar(ten_tod3,25,30)
sei <- sei[,-1]
colnames(sei) <- nombres_grupos



df_tenebrios <- as_tibble(bind_rows(uno,dos, tre, cua, cin, sei)) %>% 
  mutate(masa_corp=str_replace(masa_corp, pattern = ",", replacement = "."),
         masa_corp=as.numeric(masa_corp),
         d=str_replace(d, pattern = ",", replacement = "."),
         d=as.numeric(d),
         dx=str_replace(dx, pattern = ",", replacement = "."),
         dx=as.numeric(dx),
         semana=as.numeric(semana)) %>%
  filter(!(semana %in% NA)) %>% 
  mutate(experimento=case_when(grepl("C",grupo)~"Control",
                               grepl("O",grupo)~"Oscuridad",
                               grepl("T",grupo)~"Temperatura")) 

# seq(12,216,12)
# df_tenebrios[c(12,24,36,48,60,72,84,96,108,120,132,144,156,168,180,192,204,216),c("d","dx")] <- NA 
df_tenebrios[seq(12,216,12),c("d","dx")] <- NA # Por alguna extraña razón algunas personas decidieron poner el promedio de d y dx al final


df_tenebrios %>% 
  group_by(semana,experimento) %>% 
  summarise(media=mean(masa_corp, na.rm=T),
            sd=sd(masa_corp, na.rm=T)) %>% 
  ggplot(aes(as.numeric(semana),media, col=experimento)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=media+sd,ymax=media-sd),width=.3,size=.65) +  
  geom_point(size=2) +
  scale_color_manual(values = c("blue","black","red")) +
  labs(title = "Variación masa corporal",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semana",
       y="Masa Corporal (g)",
       col="Variación") +
  # geom_text(data = tibble(x=4, y =0.09),
  #           aes(x=x,y=y, label="*"), size = 8, inherit.aes = F) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(1,12,1)) +
  scale_y_continuous(limits = c(0,.18),
                     breaks = seq(0,.18,.03)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
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

# ggsave("masa_corporal_todos.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\tenebrios\\scripts_tenebrios\\graficas",
#       width=6, height=4)

# Existen diferencias significativas en el aumento de la masa corporal según las semanas en cada grupo?
# Utilizamos la función que he hecho en el link de la línea "6", seguramente haya alguna manera más 
# automática de hacer esto, pero bueno, esto servirá

semana_1 <- subset(df_tenebrios, as.character(semana) == "1")
th.groups(semana_1,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad") 

semana_2 <- subset(df_tenebrios, as.character(semana) == "2")
th.groups(semana_2,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_3 <- subset(df_tenebrios, as.character(semana) == "3")
th.groups(semana_3,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_4 <- subset(df_tenebrios, as.character(semana) == "4")
th.groups(semana_4,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_5 <- subset(df_tenebrios, as.character(semana) == "5")
th.groups(semana_5,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_6 <- subset(df_tenebrios, as.character(semana) == "6")
th.groups(semana_6,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_7 <- subset(df_tenebrios, as.character(semana) == "7")
th.groups(semana_7,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_8<- subset(df_tenebrios, as.character(semana) == "8")
th.groups(semana_8,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")

semana_9 <- subset(df_tenebrios, as.character(semana) == "9")
th.groups(semana_9,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad")


#### Estduio de la tasa de crecimiento semanal
df_tenebrios %>% 
  group_by(semana,experimento) %>% 
  summarise(media=mean(d, na.rm=T),
            sd=sd(d, na.rm=T)) %>% 
  ggplot(aes(as.numeric(semana),media, col=experimento)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=media+sd,ymax=media-sd),width=.3,size=.65) +  
  geom_point(size=2) +
  scale_color_manual(values = c("blue","black","red")) +
  labs(title = "Variación tasa de crecimiento",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semana",
       y="Tasa de crecimiento (g)",
       col="Variación") +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(1,12,1)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = c(.25,.2),
    legend.background = element_rect(color = "black"),
    legend.key = element_rect(fill="white"),
    legend.title = element_text(hjust = .5, size=13, face = "bold"),
    legend.text = element_text(size = 12)
  )

# ggsave("Rplot04.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\tenebrios\\scripts_tenebrios\\graficas",
#        width=6, height=4)

# ¿existen diferencias significativas con el paso de las semanas? 
semana_1 <- subset(df_tenebrios, as.character(semana) == "1")
th.groups(semana_1,d, "d", experimento, "Control", "Temperatura", "Oscuridad") 

semana_2 <- subset(df_tenebrios, as.character(semana) == "2")
th.groups(semana_2,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_3 <- subset(df_tenebrios, as.character(semana) == "3")
th.groups(semana_3,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_4 <- subset(df_tenebrios, as.character(semana) == "4")
th.groups(semana_4,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_5 <- subset(df_tenebrios, as.character(semana) == "5")
th.groups(semana_5,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_6 <- subset(df_tenebrios, as.character(semana) == "6")
th.groups(semana_6,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_7 <- subset(df_tenebrios, as.character(semana) == "7")
th.groups(semana_7,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

semana_8<- subset(df_tenebrios, as.character(semana) == "8")
th.groups(semana_8,d, "d", experimento, "Control", "Temperatura", "Oscuridad")

# Obviamente las Últimas semanas no se pueden ver si ha diferencias significativas
# ya que muchos de los tnebrios han entrado en estado de pupa, lo que impide hacer
# el test de shapiro test ya que normalmente solo queda uno o dos grupo por terminar
# Esto nos planteó a mi grupo y a mí que podríamos ver si existen diferencias 
# significativas en el tiempo que tardan cada gurpo en terminar el experimento.
# Viendo que no hay diferencias significativas en las tasas de crecimeinto con el
# paso de las semanas así como con la masa corporal. En tema comercial, igual nos renta
# Utilizar el tratameinto que más rápido haga crecer a los tenebrios. Aunque esto también
# podría funcionar al contrario ahora que me fijo en las gráficas, si los quieres
# con más masa corporal, igual te interesan que tarden más para que aumenten en este parámetro.
# Aunque por otra parte es cierto que conforme más aumentan las semanas, menor tasa de 
# crecimeinto tienen en las últimas.

# Al parecer el grupo de temperatura parece entrar más rápido en estado de pupa, ya 
# que en las gráficas vemos que este tratameito pierde en las últimas semanas más rápido
# las barras de error, lo que significa eso, las larvas entran en pupa más rápido.
# Pero... ¿son estas diferencias significativas?


df_temporal <- tibble(
  semanas = c(12,11,10,10,10,10,
              9,9,12,9,9,10,
              12,9,11,11,11,10),
  experimento = c(rep("Control",6),
                  rep("Temperatura",6),
                  rep("Oscuridad",6))
)

# No existen diferencias significativas entre los grupos, p > 0.05
test_temporal <- th.groups(df_temporal, semanas, "semanas", experimento, "Control", "Temperatura","Oscuridad")
estadistico_temporal <- test_temporal$statistic
p.valor <- round(test_temporal$p.value,2)

df_temporal %>% 
  mutate(experimento=factor(experimento,
                            levels = c("Control","Temperatura","Oscuridad"))) %>% 
  ggplot(aes(experimento, semanas, fill=experimento)) +
  geom_jitter(pch=21, position = position_jitter(.25), show.legend = F) +
  geom_boxplot(alpha=.35, width=.5, show.legend = F) +
  scale_y_continuous(limits = c(5,15),
                     breaks = seq(4,16,2)) +
  scale_fill_manual(breaks = c("Control", "Temperatura", "Oscuridad"),
                    values = c("blue","red","black")) +
  labs(
    title = "Tiempo de experimentación de los grupos",
    subtitle = glue("*X\u00B2 Kruskal-Wallis* = {estadistico_temporal}, *p* = {p.valor}"),
    y="Semanas de experimentación",
    x=NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face="bold", size = 16, hjust = .5),
    plot.subtitle = element_markdown(size = 13, hjust = .5),
    axis.title.y = element_markdown(size = 12, face = "bold"),
    axis.text.x = element_markdown(size = 12, face = "bold", color="black")
  )
# ggsave("Rplot03.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\tenebrios\\scripts_tenebrios\\graficas",
#       width=8.5, height=4)
