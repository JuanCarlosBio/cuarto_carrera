################################################################################
#########################    Intercambio gaseoso             ###################
################################################################################
library(tidyverse)
library(readxl)
library(ggthemes)
library(DescTools)

#------------------------------------------------------------------------------#
#                       Estudio de la Tª con/sin vaselina                      #
#------------------------------------------------------------------------------#

url1 <- "https://raw.githubusercontent.com/Juankkar/afv_practicas_campo_ecofiosiologia/main/bases_datos/fvfm.csv"
fvfm <- read_csv(url1) %>% 
  mutate(hora=as.character(hora))
 
temp_sol <- fvfm %>% filter(exposicion %in% "Sol");temp_sol
temp_sombra <- fvfm %>% filter(exposicion %in% "Sombra");temp_sombra


temp_sol %>% 
  group_by(hora,vaselina) %>% 
  mutate(media = mean(temperatura), sd = sd(temperatura)) %>% 
  ggplot(aes(hora, media, fill = vaselina)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.9)) +
  geom_text(data = tibble(x=c(1,3,5), y=c(20,19,37)),
            aes(x=x,y=y,label=c(rep("*",3))), inherit.aes = F, size=8) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(title = "Tª de la hoja con/sin vaselina, Rosalillo al sol",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "T leaf (ºC)",
       fill = "Con/sin vaselina") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.3,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

temp_sombra %>% 
  group_by(hora,vaselina) %>% 
  summarise(media = mean(temperatura), sd = sd(temperatura)) %>% 
  ggplot(aes(hora, media, fill = vaselina)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.9)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(title = "Tª de la hoja con/sin vaselina, Rosalillo a la sombra",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "T leaf (ºC)",
       fill = "Con/sin vaselina") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.7,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

#####################################
# Análisis estadístico más profundo # Se debería buscar alguna forma de hacer este proceso más auto_
##################################### mático, tiempo al tiempo supongo.

temp_sol2 <- temp_sol[,c("hora","temperatura","vaselina")]

sol_10_00 <- temp_sol2 %>% filter(hora %in% "10:00:00")
tapply(sol_10_00$temperatura,sol_10_00$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sol_10_00)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sol_10_00)                 # p < 0.05*

sol_10_30 <- temp_sol2 %>% filter(hora %in% "10:30:00")
tapply(sol_10_30$temperatura,sol_10_30$vaselina, shapiro.test) # No se cumple normalidad
wilcox.test(temperatura~vaselina, data = sol_10_30)            # p > 0.05

sol_11_00 <- temp_sol2 %>% filter(hora %in% "11:00:00")           
tapply(sol_11_00$temperatura,sol_11_00$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sol_11_00)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sol_11_00)                 # p < 0.05*

sol_11_30 <- temp_sol2 %>% filter(hora %in% "11:30:00")
tapply(sol_11_30$temperatura,sol_11_30$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sol_11_30)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sol_11_30)                 # p > 0.05 

sol_12_00 <- temp_sol2 %>% filter(hora %in% "12:00:00")
tapply(sol_12_00$temperatura,sol_12_00$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sol_12_00)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sol_12_00)                 # p < 0.05*

sol_12_30 <- temp_sol2 %>% filter(hora %in% "12:30:00")
tapply(sol_12_30$temperatura,sol_12_30$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sol_12_30)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sol_12_30)                 # p > 0.05 


temp_sombra2 <- temp_sombra[,c("hora","temperatura","vaselina")]

sombra_10_00 <- temp_sombra2 %>% filter(hora %in% "10:00:00")
tapply(sombra_10_00$temperatura,sombra_10_00$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sombra_10_00)                # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sombra_10_00)                    # p > 0.05 

sombra_10_30 <- temp_sombra2 %>% filter(hora %in% "10:30:00")
tapply(sombra_10_30$temperatura,sombra_10_30$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sombra_10_30)                # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sombra_10_30)                    # p > 0.05

sombra_11_00 <- temp_sombra2 %>% filter(hora %in% "11:00:00")           
tapply(sombra_11_00$temperatura,sombra_11_00$vaselina, shapiro.test) # no se cumple normalidad
wilcox.test(temperatura~vaselina, data = sombra_11_00)               # p > 0.05 

sombra_11_30 <- temp_sombra2 %>% filter(hora %in% "11:30:00")
tapply(sombra_11_30$temperatura,sombra_11_30$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sombra_11_30)                # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sombra_11_30)                    # p > 0.05 

sombra_12_00 <- temp_sombra2 %>% filter(hora %in% "12:00:00")
tapply(sombra_12_00$temperatura,sombra_12_00$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sombra_12_00)             # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sombra_12_00)                 # p > 0.05 

sombra_12_30 <- temp_sombra2 %>% filter(hora %in% "12:30:00")
tapply(sombra_12_30$temperatura,sombra_12_30$vaselina, shapiro.test) # se cumple normalidad
LeveneTest(temperatura~vaselina, data = sombra_12_30)                # se cumple homocedasticidad
t.test(temperatura~vaselina, data = sombra_12_30)                    # p > 0.05

#------------------------------------------------------------------------------#
#                         Eficiencia fotoquímica del                           #
#                             Pino y el rosalillo                              #
#------------------------------------------------------------------------------#

url2 <- "https://raw.githubusercontent.com/Juankkar/afv_practicas_campo_ecofiosiologia/main/bases_datos/fvfm2.csv"
fvfm2 <- read_csv(url2) %>% 
  mutate(hora=as.character(hora))

fvfm2 %>%
  group_by(hora, exposicion) %>% 
  summarise(media = mean(fvfm), sd = sd(fvfm)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.6), col = "black", width = .5) +
  geom_errorbar(aes(ymin = media-sd, ymax = media + sd), width = .2,
                position = position_dodge(.6)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.3),
                     breaks = seq(0,1.3, .2)) +
  labs(title = "Eficiencia fotoquímica Pino/Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "Fv/Fm",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.2,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))

#------------------------------------------------------#
# Son las diferencias estadísticamente significativas? #
#------------------------------------------------------#

pino <- fvfm2 %>% filter(especie %in% "Pino")
tapply(pino$fvfm, pino$exposicion, shapiro.test)    # se cumple normalidad
LeveneTest(fvfm~exposicion, data = pino)            # se cumple homocedasticidad
t.test(fvfm~exposicion, data = pino)                # p > 0.05

rosalillo <- fvfm2 %>% filter(especie %in% "Rosalillo")
tapply(rosalillo$fvfm, rosalillo$exposicion, shapiro.test)    # se cumple normalidad
LeveneTest(fvfm~exposicion, data = rosalillo)                 # se cumple homocedasticidad
t.test(fvfm~exposicion, data = rosalillo)                     # p > 0.05


