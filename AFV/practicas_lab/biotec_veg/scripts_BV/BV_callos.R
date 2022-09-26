################################################################################
#               Prácticas de Aplicaciones de la Fisiología Vegetal             #
################################################################################

library(tidyverse)
library(ggthemes)
library(lubridate)
library(xlsx)

               ##############################################
               # Práctica 2 explantes y siembra de Zanhoria #
               ##############################################

#------------------------------------------------------------------------------#
#                         Contaminaciones de zanahoria                         #
#------------------------------------------------------------------------------#

url_callos <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/AFV/practicas_lab/biotec_veg/bases_datos/data_zanahoria.csv"
data_zanahoria <- read_csv(url_callos)

#### DATA DE CALLOS DE ZANAHORIA MANUAL ##### --> si no te va el link de arriba puedes usar esto
# data_zanahoria <- data.frame(
#   cont=c(24,18,rep(3,4),rep(2,5)),
#   callos=c(rep(0,4),rep(2,7)),
#   fecha=seq(date("2021-10-07"),date("2021-12-16"),7)
# )
# write_csv(data_zanahoria, "data_zanahoria.csv")

data_zanahoria %>% 
  ggplot(aes(fecha, cont)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_line(size = 1) +
  geom_point(pch = 21, size = 3, fill = "white") +
  geom_text(data = data.frame(x=date("2021-11-25"), y = 20),
            aes(x=x,y=y, label="Cultivos iniciales: 24 cultivos\nCultivos viables: 2"),
            inherit.aes = F, size=6) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,30),
                     breaks = seq(0,28,3)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "7 days") +
  labs(title = "Contaminaciones, callos de zanahoria",
       subtitle = "Informe AFV, Grupo 6, Bloque de Biotecnología, Biología ULL",
       y = "Nº de medios de cultivo",
       x = "Fecha de la contaminación (2021)") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = .5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 10)
    ) 

# ggsave("Rplot02.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\AFV\\practicas_lab\\graficas",
#         width = 8, height = 4)

data_zanahoria %>% 
  ggplot(aes(fecha, callos)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  geom_line(size = 1) +
  geom_point(pch = 21, size = 3, fill = "white") +
  geom_text(data = data.frame(x=date("2021-10-28"), y = 3),
            aes(x=x,y=y, label="Cultivos iniciales: 24 cultivos\nCallos formados: 2"),
            inherit.aes = F, size=6) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,4),
                     breaks = seq(0,4,1)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "7 days") +
  labs(title = "Callos formados (zanahoria)",
       subtitle = "Informe AFV, Grupo 6, Bloque de Biotecnología, Biología ULL",
       y = "Nº de medios de cultivo",
       x = "Fecha de la contaminación (2021)") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = .5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 10)
  )

# ggsave("Rplot03.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\AFV\\practicas_lab\\graficas",
#        width = 8, height = 4)




 
 
  
  
