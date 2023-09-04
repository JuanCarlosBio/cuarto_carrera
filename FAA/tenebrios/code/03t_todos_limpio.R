###############################################################################
############################ Tenebrios Todos los grupos ########################
################################################################################

# Esta función me abre corre un script que tengo en GitHub con funciones de inferencia estadística
source("https://raw.githubusercontent.com/Juankkar/cosas_mias/main/funciones_propias/funR/contraste_hip.R")

library(tidyverse)
library(ggtext)
library(glue)

df_tenebrios <- read_csv("tenebrios/data/procesada/tenebrios_todos_process.csv") %>%
    mutate(experimento=case_when(grepl("C",grupo)~"Control",
                                 grepl("O",grupo)~"Oscuridad",
                                 grepl("T",grupo)~"Temperatura"),
           masa_corp = as.double(masa_corp),
           d = as.double(d),
           dx = as.double(dx))

df_tenebrios %>% head()

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

ggsave("tenebrios/results/plots/masa_corporal_todos.png",
       width=6, 
       height=4)

# Existen diferencias significativas en el aumento de la masa corporal según las semanas en cada grupo?
# Utilizamos la función que he hecho en el link de la línea "6", seguramente haya alguna manera más 
# automática de hacer esto, pero bueno, esto servirá

for(i in c(1:9)){
  print(glue(">>> Inferencia en la semana: {i}"))
  semana_n <- subset(df_tenebrios, as.character(semana) == i)
  print(th.groups(semana_n,masa_corp, "masa_corp", experimento, "Control", "Temperatura", "Oscuridad"))
}

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

ggsave("tenebrios/results/plots/tasa_crecimiento_todos.png",
       width=6, 
       height=4)

## Inferencia estadística de las 12 semanas de la tasa de crecimiento.
for(i in c(1:8)){
  print(glue(">>> Inferencia en la semana: {i}"))
  semana_n <- subset(df_tenebrios, as.character(semana) == i)
  print(th.groups(semana_n,d, "d", experimento, "Control", "Temperatura", "Oscuridad"))
}

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
  geom_jitter(pch=21, position = position_jitter(.15), show.legend = F) +
  geom_boxplot(alpha=.35, width=.35, show.legend = F) +
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
ggsave("tenebrios/results/plots/comparacion_semanas_crecimeinto.png",
       width=7, 
       height=6)
