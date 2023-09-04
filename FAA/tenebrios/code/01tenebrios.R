################################################################################
###########################  PRÁCTICA DE LOS TENEBRIOS  ########################
################################################################################

library(tidyverse)
library(glue)

#### Nuestros resultados para el grupo 4 de Oscuridad.
result_o4 <- read_csv("tenebrios/data/tenebrios.csv")

colnames(result_o4) <- c("semana", "masa.corporal", "tasa.crec.s","tasa.crec.d")   

masa_tasa_o4 <- result_o4[,c("semana","masa.corporal","tasa.crec.s")] %>%      
  pivot_longer(cols = -semana,names_to = "masa_tasa", values_to = "valores")   

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

ggsave("tenebrios/results/plots/masa_corporal_grupo.png",
       width = 8, 
       height = 5)

df_o4 <- read.csv("tenebrios/data/df_o4.csv", sep = ";") 
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


na <- data.frame(na=NA)
tasa_crec <- data.frame(tasa.crec.s=df_o4_2[2:11,2]-df_o4_2[1:10,2]) 

df_o4_completo <- df_o4_2 %>% 
  mutate(tasa.crec.s = c(tasa_crec$media_mc,na$na),
         tasa.crec.d = tasa.crec.s/7,
         dia=seq(0,70,7))

cor.test(x=df_o4_completo$semana,
         y=df_o4_completo$media_mc,
         method = "pearson")

modelo_o4 <- lm(media_mc~semana, data = df_o4_completo)
summary(modelo_o4) 

coeficientes <- as.data.frame(modelo_o4$coefficients)
row.names(coeficientes) <- c("Intercepto", "Pendiente")
colnames(coeficientes) <- c("Resultados")
abline <- c(glue(coeficientes[1,]),
            glue(coeficientes[2,]))

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

ggsave("tenebrios/results/plots/masa_corporal_tasa_crec_grupo.png",
       width = 7, 
       height = 4)

cor.test(x=df_o4_completo$media_mc,
         y=df_o4_completo$tasa.crec.s,
         method = "pearson")

