################################################################################
#               Prácticas de Aplicaciones de la Fisiología Vegetal             #
################################################################################

library(tidyverse)
library(lubridate)

        #######################################################
        # Práctica 3 explantes y siembra de segmentos nodales #
        #######################################################

data_brotes <- read_csv("data/data_brotes.csv")

##### DATA DE BROTES MANUAL ##### 
# data_brotes <- data.frame(
#   cont=c(rep(24,3),21,rep(20,5)),
#   brotes=c(0,rep(23,2),20,rep(19,5)),
#   enraizadas=c(rep(0,5),rep(7,2),rep(8,2)),
#   fecha=seq(date("2021-10-21"),date("2021-12-16"),7)
# )
# write_csv(data_brotes, "data_brotes.csv")


data_brotes %>% ggplot(aes(fecha, cont)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black") +
  geom_line(size = 1) +
  geom_point(pch = 21, size = 3, fill = "white") +
  geom_text(data = data.frame(x=date("2021-11-25")+5, y = 30),
            aes(x=x,y=y, 
                label="Cultivos iniciales: 24 cultivos\nCultivos viables: 20"),
            inherit.aes = F, size=6) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40),
                     breaks = seq(0,40,4)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "7 days") +
  labs(title = "Contaminaciones, segmentos nodales y ápices",
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

ggsave("results/plots/brotes/03contaminaciones.png",
       width = 8, 
       height = 4)

brotes_enr <- data_brotes %>%
  select(fecha, brotes, enraizadas) %>% 
  mutate(
    brotes_trans =  (brotes - min(brotes))/(max(brotes)-min(brotes)),

    raices_trans =  (enraizadas - min(enraizadas))/(max(enraizadas)-min(enraizadas)),
  )
# Vemos los valores máximos y mínimos para ajustar los scales:
brotes_enr %>% summarise(max_brotes =max(brotes), min_brotes =min(brotes),
                         max_reaices =max(enraizadas), min_raices =min(enraizadas))

brotes_enr %>% 
  ggplot(aes(fecha, brotes_trans)) +
  geom_line(size=1, color="forestgreen") +
  geom_point(size=3, color="forestgreen") +
  geom_line(aes(y = raices_trans), color="brown", size=1) +
  geom_point(aes(y = raices_trans), color="brown", size=3) +
  labs(
    title="Formación de segmentos nodales y raíces",
    subtitle = "Informe AFV, Grupo 6, Bloque de Biotecnología, Biología ULL",
    x="Fecha (2021)",
    y="Segmentos nodales\nformados"
  ) +
  scale_color_manual(breaks = c("brotes","enraizadas"),
                     values = c("forestgreen", "brown")) +
  scale_y_continuous(labels = seq(0,24, 3),
                     breaks = (seq(0,24,3) - 0)/(23-0),
                     sec.axis = sec_axis(trans = ~.,
                                         labels = seq(0,8, 2),
                                         breaks = (seq(0,8,2) - 0)/(8-0),
                                         name = "Raíces formadas")) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "7 days") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 13, face = "italic", hjust = .5),
    axis.title.y.left = element_text(color = "forestgreen",
                                     size = 12, face = "bold",
                                     margin=margin(r=10)),
    axis.title.y.right = element_text(color = "brown", 
                                      size = 12, face = "bold",
                                      margin = margin(l=10)),
    axis.text.y.left = element_text(color = "forestgreen", size = 11),
    axis.text.y.right = element_text(color = "brown", size = 11),
    axis.text.x  = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 12),
    
  )

ggsave("results/plots/brotes/04brotes.png",
      width = 8, 
      height = 4)

