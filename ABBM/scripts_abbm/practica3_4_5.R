library(tidyverse)
library(readxl)
library(rstatix)
library(glue)
library(ggtext)

# Vamos a crear otra funcioncita facilita, la del rf
rf <- function(dist_frente, dist_prot){
  cuerpo = (dist_prot)/dist_frente
  return(cuerpo)
}

resultados_clase <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/resultados_clase.csv") %>% 
  mutate(rf=rf(dist_frente = dist_frente, dist_prot = dist_prot)) %>% 
  select(-dist_prot, -dist_frente)

parametros <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/parametros.csv") %>% 
  mutate(rf_gel1=rf(dist_frente1,dist_gel1),
         rf_gel2=rf(dist_frente1,dist_gel2),
         rf_gel3=rf(dist_frente1,dist_gel3),
         rf_gel4=rf(dist_frente1,dist_gel4),
         log_pm=log10(kd)) %>% 
  select(marcador, log_pm, rf_gel1, rf_gel2, rf_gel3,rf_gel4)


parametros_long <- parametros %>% 
  pivot_longer(-c(marcador, log_pm), names_to = "grupo", values_to = "rf") 


gel_1 <- resultados_clase %>% filter(gel == "Gel 1") %>% 
  summarise(rf=mean(rf))

gel_2 <- resultados_clase %>% filter(gel == "Gel 2") %>% 
  summarise(rf=mean(rf))

gel_3 <- resultados_clase %>% filter(gel == "Gel 3") %>% 
  summarise(rf=mean(rf))

gel_4 <- resultados_clase %>% filter(gel == "Gel 4") %>% 
  summarise(rf=mean(rf))

intrepol_gel1 <- 1.955
intrepol_gel2 <- 1.872
intrepol_gel3 <- 1.64
intrepol_gel4 <- 1.735

peso_prot_1 <- round(10^intrepol_gel1,2)
peso_prot_2 <- round(10^intrepol_gel2,2)
peso_prot_3 <- round(10^intrepol_gel3,2)
peso_prot_4 <- round(10^intrepol_gel4,2)


parametros_long %>%   
  ggplot(aes(rf, log_pm, group=grupo, color=grupo)) +
  #geom_line(size=1) +
  geom_point() +
  # ggplot2 presenta una funcion llamada geom_smooth que te hace la función de la recta automática, 
  # sin necesidad de calcularla para cada grupo. NICE. Si es cierto que tiene ciertos inconvenientes...
  # No es completa, no llega a cortar en el origen (y = log(PM)). Pero bueno, en nuestro caso servira.
  geom_smooth(method = "lm", se=F) +
  geom_vline(xintercept = gel_1$rf, color="darkgray", linetype="dashed",size=.8) +
  geom_vline(xintercept = gel_2$rf, color="red", linetype="twodash",size=.8) +
  geom_vline(xintercept = gel_3$rf, color="blue", linetype="dashed",size=.8) +
  geom_vline(xintercept = gel_4$rf, color="forestgreen", linetype="dashed",size=.8) +
  geom_hline(yintercept = intrepol_gel1, color="darkgray", linetype="dashed",size=.8) +
  geom_hline(yintercept = intrepol_gel2, color="red", linetype="twodash",size=.8) +
  geom_hline(yintercept = intrepol_gel3, color="blue", linetype="dashed",size=.8) +
  geom_hline(yintercept = intrepol_gel4, color="forestgreen", linetype="dashed",size=.8) +
  scale_y_continuous(limits = c(1,3),
                     breaks = seq(1,2.8,.35)) +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0,1,.15)) +
  scale_color_manual(
    name=NULL,
    breaks = c("rf_gel1","rf_gel2","rf_gel3","rf_gel4"),
    labels = c("Gel 1","Gel 2","Gel 3","Gel 4"),
    values = c("gray45", "red3", "blue3", "forestgreen")) +
  labs(
    title = "Interpolación entre el log(PM) y la movilidad relativa",
    x = "Movilidad relativa (RF)",
    y = "log(PM)",
    caption = glue("Resultados:<br><span style = 'color: gray45'>Gel 1 = {peso_prot_1} Kd</span> <span style = 'color: red'>Gel 2 = {peso_prot_2} Kd</span> <span style = 'color: blue'>Gel 3 = {peso_prot_3} Kd</span> <span style = 'color: forestgreen'>Gel 3 = {peso_prot_4} Kd</span>")
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size=13, color = "black", 
                              face = "bold", margin = margin(b=30)),
    plot.caption = element_markdown(face = "italic", hjust = 0, size = 11),
    axis.line = element_line(color = "black", size=.75), 
    axis.text = element_text(color = "black"),
    axis.title = element_text(color="black", face = "bold"),
    panel.background = element_rect(fill="white", color = "white"),
    plot.background = element_rect(fill="white", color = "white"),
    legend.background = element_rect(fill="white", color = "black"),
    legend.text = element_text(color = "black"),
    legend.key = element_rect(fill = "white", color="white"),
    legend.position = c(.8,.7),
    panel.grid = element_blank()
  )

ggsave("rf.png", path="C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\ABBM\\graficas",
       width=7, height=5)

# Como va la correlación para cada uno?, en las diapos de la tutoría parece ser que es mejor la 
# función de la recta para 

parametros_long %>% 
  group_by(grupo) %>% 
  cor_test(rf, log_pm) # En todas las lineas es alta. estimacion de Pearson  menos alta 
                              # (negativamente hablando) es de 0.94, interpolaremos en cada caso.









