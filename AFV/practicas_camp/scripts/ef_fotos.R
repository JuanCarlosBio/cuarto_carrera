################################################################################
#########################   Eficiencia fotosintética         ###################
################################################################################
library(tidyverse)
library(ggthemes)
library(readxl)

#------------------------------------------------------------------------------#
#                                Intercambio Gaseoso                           #
#------------------------------------------------------------------------------#
url1 <- "https://raw.githubusercontent.com/Juankkar/afv_practicas_campo_ecofiosiologia/main/bases_datos/inter_gas.csv"
inter_gas <- read_csv(url1) %>% 
  mutate(hora=as.character(hora))

url2 <- "https://raw.githubusercontent.com/Juankkar/afv_practicas_campo_ecofiosiologia/main/bases_datos/inter_gas_2.csv"
inter_gas2 <- read_csv(url2) %>% 
  rename_all(tolower) %>% 
  mutate(hora=as.character(hora))


# Evolución PAR a lo largo de la mañana

inter_gas2 %>% 
  group_by(sol_sombra) %>% 
  filter(experimento %in% "PAR") %>% 
  ggplot(aes(hora, sol, fill = sol_sombra, group = sol_sombra)) +
  geom_line(size = 1, aes(col = sol_sombra), show.legend = F) +
  geom_point(pch = 21, size = 2) +
  labs(title = "Variación PAR en la mañana, luz saturante",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "PAR (umol·m^-2·s^-1)",
       fill = "SOL/SOMBRA") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,2000),
                     breaks = seq(0,2000, 200)) +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"),
        legend.position = c(.2,.4),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("gray", "black")) +
  scale_color_manual(values = c("gray", "black"))
  
# Temperatura
inter_gas %>% 
  group_by(hora, exposicion) %>%
  summarise(media = mean(tleaf, na.rm = T), sd = sd(tleaf, na.rm = T)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .2,
                position = position_dodge(.9)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,50),
                     breaks = seq(0,50, 10)) +
  labs(title = "Variación en la Tª de la hoja durante la mañana",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "T leaf (ºC)",
       fill = "SOL/SOMBRA") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"),
        legend.position = c(.5,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))

# Fotosíntesis (A)
inter_gas %>% 
  group_by(hora, exposicion) %>%
  summarise(media = mean(a, na.rm = T), sd = sd(a, na.rm = T)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(),
           col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .2,
                position = position_dodge(.9)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(-2.5,10),
                     breaks = seq(-2.5,10, 2)) +
  labs(title = "Variación fotosíntesis durante la mañana",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "A (umol CO2·m^-2·s^-1)",
       fill = "SOL/SOMBRA") +
  theme_tufte() +
  theme(axis.line.y = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"),
        legend.position = c(.7,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11),
        axis.ticks.x =element_blank()) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))

# Transpiración

inter_gas %>% 
  group_by(hora, exposicion) %>%
  summarise(media = mean(e, na.rm = T), sd = sd(e, na.rm = T)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .2,
                position = position_dodge(.9)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(-.3,4),
                     breaks = seq(0,4, .5)) +
  labs(title = "Variación Transpiración (E) durante la mañana",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "E (mol H2O·m^-2·s^-1)",
       fill = "SOL/SOMBRA") +
  theme_tufte() +
  theme(axis.line.y = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))

# Conductancia estomática

inter_gas %>% 
  group_by(exposicion, hora) %>%
  summarise(media = mean(gs, na.rm = T), sd = sd(gs, na.rm = T)) %>% 
  ggplot(aes(hora, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .2,
                position = position_dodge(.9)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(-.01,.15),
                     breaks = seq(0,.15, .05)) +
  labs(title = "Variación en la Conductancia estomática durante la mañana",
       subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
       x = "Hora",
       y = "Gs (mol·m^-2·s^-1)",
       fill = "SOL/SOMBRA") +
  theme_tufte() +
  theme(axis.line.y = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 13, face = "bold"),
        legend.position = c(.5,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray")) +
  scale_color_manual(values = c("white", "gray"))


#------------------------------------------------------------------------------#
#                        Esto de aquí no tiene que ver con
#                   lo que Agueda quiere, pero vamos a hacer un 
#                             Análisis multivariante: 
#------------------------------------------------------------------------------#


### Variables numéricas
# tleaf = Temperatura de la hoja
# a = eficiencia fotosintética (o algo así xd)
# gs = Conductancia estomática
# e = transiracion
# q = Ni me acuerdo xd

preprocesado <- inter_gas %>% select(exposicion,tleaf, a, gs, e, q) %>% 
  filter(!(tleaf %in% NA) &
           !(a %in% NA) &
           !(gs %in% NA) &
           !(e %in% NA) &
           !(q %in% NA)
         );preprocesado # Tendriamos 24 muestras, ok ok 

matrix_inter_gas <- as.matrix(preprocesado[,-1]) # nos quedamos solo con las variables cuantitativas

pca_inter_gas <- prcomp(matrix_inter_gas, center = T, scale = T)

summary(pca_inter_gas) # PC1 = 0.49, PC2 = 0.33, entre los dos explican un 82.48 % de la varianza total
componentes <- as.data.frame(pca_inter_gas$x)
componentes$PC1



componentes_principales <- data.frame(
  exposicion=preprocesado[,1],
  PC1=componentes$PC1,
  PC2=componentes$PC2,
  PC3=componentes$PC3
)


# Corrlacion entre las componentes y las variables, importante
round(cor(as.data.frame(matrix_inter_gas), componentes_principales[,c("PC1","PC2")]),2)

componentes_principales %>% 
  ggplot(aes(PC1,PC2, fill=exposicion)) +
  geom_point(pch=21) +
  stat_ellipse(geom = "polygon", alpha=.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("yellow", "black")) +
  labs(
    title = "PCA de las variables de intercambio gaseoso",
    subtitle = "Informe A.F.V,García-Estupiñán, J.C., Biología ULL",
    x = "PC1 (49% de la varianza explicada)",
    y = "PC2 (33% de la varianza explicada)",
    fill= "Tipo de hoja"
  ) +
  theme_classic() +
  theme(
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x  = element_text(size = 11, face = "bold"),
        legend.position = c(.2,.8),
        legend.background = element_rect(color = "black", fill = NULL),
        legend.text = element_text(size = 11)) 

#-------------------------------------------------------------------------------------------------#
#                                     RESULTADO DEL PCA TEÓRICAMENTE                              #
#-------------------------------------------------------------------------------------------------#


### inferencia de la PC1
# 1) Normalidad
tapply(componentes_principales$PC1, componentes_principales$exposicion, shapiro.test) # p<0.05 en sol, no se cumple la asunción de normalidad
# Test de Wilcoxon Mann-Whithney (mediana)
wilcox.test(PC1~exposicion, data = componentes_principales) # p<0.05 las diferencias son estadísticamente
                                                            # significativas para la PC1 

### inferencia de la PC2
# 1) Normalidad
tapply(componentes_principales$PC2, componentes_principales$exposicion, shapiro.test) # p>0.05 asunción de normalidad cumplida
# 2) Homocedasticidad
library(DescTools)
LeveneTest(PC2~exposicion, data = componentes_principales, center="mean") # p > 0.05 las varianzas son iguales
# T de student o T-test
t.test(PC1~exposicion, data = componentes_principales,
       var.eq=TRUE) # p<0.05




