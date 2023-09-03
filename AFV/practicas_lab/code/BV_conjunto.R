################################################################################
#               Prácticas de Aplicaciones de la Fisiología Vegetal             #
################################################################################

library(tidyverse)
library(ggthemes)
library(lubridate)
library(xlsx)

                  ##############################################
                  #   Gráfico que se utilizó para el trabajo   #
                  ##############################################

#                 Gráfico conjunto en conjunto que subimos al trabajo
#   mirando hacia atrás no me siento muy orgulloso de mi obra, en la actualidad
#   habría hecho los anteriores, en qué estaba pensando?, servir sirve de todas
#   formas, lo he mejorado de todas formas, que si que habían cosas que no me gustan
#   what a failure


url_conjunto <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/AFV/practicas_lab/biotec_veg/bases_datos/data_conjunto_biotecv.csv"
data_conjunto <- read_csv(url_conjunto)

#### Data Manual de los datos en su conjunto ##### 
# cont_callos=-c(0,6,rep(21,4),rep(22,5))
# callos=c(rep(0,4),rep(2,7))
# fecha=seq(date("2021-10-07"),date("2021-12-16"),7)
# cont_brotes=-c(NA,NA,rep(0,3),3,rep(4,5))
# brotes=c(NA,NA,0,rep(23,2),20,rep(19,5))
# enraizadas=c(NA,NA,rep(0,5),rep(7,2),rep(8,2))
# 
# data_conjunto <- data.frame(brotes, callos, cont_brotes,cont_callos,enraizadas,fecha)
# write_csv(data_conjunto, "data_conjunto_biotecv.csv")


data_conjunto %>%
  pivot_longer(-fecha) %>% 
  ggplot(aes(fecha, value, col = name, shape=name)) +
  geom_line(size = 1) + 
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0) +
  geom_text(data = tibble(x=date(x="2021-11-12"), y=-18),
            aes(x=x,y=y, label="LOL... ¿Éxito sin precedentes en cultivo in vitro?"), 
            inherit.aes = F, size=3.5) +
  scale_y_continuous(limits = c(-25,30),
                     breaks = seq(-25,28,10)) +
  scale_color_manual(breaks = c("cont_brotes","brotes","enraizadas","cont_callos","callos"),
                     labels = c("Seg.cont.", "Seg.form.","rai.form","Cal.cont","Cal.form"),
                     values = c("forestgreen","green","brown","orange","orangered3")) +
  scale_shape_manual(breaks = c("cont_brotes","brotes","enraizadas","cont_callos","callos"),
                     labels = c("Seg.cont.", "Seg.form.","rai.form","Cal.cont","Cal.form"),
                     values = c(1,2,3,4,5)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "7 days") +
  labs(title = "Atrocidad prácticas de biotecnología",
       subtitle = "Informe AFV, Grupo 6, Bloque de Biotecnología, Biología ULL",
       y = "Cultivos",
       x = "Fecha de revisión",
       shape = NULL,
       col = NULL) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = .5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    legend.key = element_rect(fill = "white"),
    legend.position = "top"
  ) 

# ggsave("Rplot04.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\AFV\\practicas_lab\\graficas",
#       width = 8, height = 5)
