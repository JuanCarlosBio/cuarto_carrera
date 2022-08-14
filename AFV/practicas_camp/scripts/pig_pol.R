################################################################################
#########################       INFORME DE PRÁCTICAS           #################
#########################      Pigmentos polifenoles           #################
################################################################################

library(tidyverse)
library(ggthemes)
library(DescTools)
library(readxl)
library(rgl)
library(tidytext)
library(xlsx)

url1 <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/AFV/practicas_camp/bases_datos/pig_polifenoles_pino.csv"
pig_pol_pino <- read_csv(url1)

##### NBI Pino #####
pig_pol_pino %>% 
  group_by(hoja, exposicion) %>% 
  summarise(media = mean(nbi), sd = sd(nbi)) %>%
  ggplot(aes(hoja, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,50),
                     breaks = seq(0,59, 10)) +
  labs(title = "NBI del Pino",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Estadío de la hoja",
       y = "NBI",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

#-------------------------- Diferenicias NBI? ---------------------------#
h_adulta <- pig_pol_pino %>% filter(hoja %in% "Adulta")
tapply(h_adulta$nbi, h_adulta$exposicion, shapiro.test)          # Se cumple la normaliddad
LeveneTest(nbi~exposicion, data = h_adulta, center = "mean")     # Se cumple la homocedasticidad
t.test(nbi~exposicion, data = h_adulta)                          # p > 0.05

h_joven <- pig_pol_pino %>% filter(hoja %in% "Joven")
tapply(h_joven$nbi, h_joven$exposicion, shapiro.test)            # Se cumple la normaliddad
LeveneTest(nbi~exposicion, data = h_joven, center="mean")        # Se cumple la homocedasticidad
t.test(nbi~exposicion, data = h_joven)                           # p > 0.05

h_senescente <- pig_pol_pino %>% filter(hoja %in% "Senescente")
tapply(h_senescente$nbi, h_senescente$exposicion, shapiro.test)  # No se cumple la normaliddad
wilcox.test(nbi~exposicion, data = h_senescente)                 # p > 0.05
#### Clorofila Pino

pig_pol_pino %>% 
  group_by(hoja, exposicion) %>% 
  summarise(media = mean(chl), sd = sd(chl)) %>%
  ggplot(aes(hoja, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,90),
                     breaks = seq(0,90, 10)) +
  labs(title = "Nivel de clorofila de la hoja de Pino",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Estadío de la hoja",
       y = "Clorofila",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.15,.85),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#----------------------- Diferencias Chl Pino? -----------------------------#

tapply(h_adulta$chl, h_adulta$exposicion, shapiro.test)          # Se cumple la normaliddad
LeveneTest(chl~exposicion, data = h_adulta, center = "mean")     # Se cumple la homocedasticidad
t.test(chl~exposicion, data = h_adulta)                          # p > 0.05

tapply(h_joven$chl, h_joven$exposicion, shapiro.test)            # Se cumple la normaliddad
LeveneTest(chl~exposicion, data = h_joven, center="mean")        # No se cumple la homocedasticidad
t.test(chl~exposicion, data = h_joven, var.eq=FALSE)             # p > 0.05

tapply(h_senescente$chl, h_senescente$exposicion, shapiro.test)  # No se cumple la normaliddad
wilcox.test(chl~exposicion, data = h_senescente)                 # p > 0.05

# Flavonoides Pino

pig_pol_pino %>% 
  group_by(hoja, exposicion) %>% 
  summarise(media = mean(flav), sd = sd(flav)) %>%
  ggplot(aes(hoja, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,5),
                     breaks = seq(0,5, 1)) +
  labs(title = "Nivel flavonoides de la hoja Pino",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Estadío de la hoja",
       y = "Flavonoides",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#----------------- Diferencias significativas Flavonoides Pino?----------------#
tapply(h_adulta$flav, h_adulta$exposicion, shapiro.test)          # No se cumple la normaliddad
wilcox.test(flav~exposicion, data = h_adulta)                     # p > 0.05

tapply(h_joven$flav, h_joven$exposicion, shapiro.test)            # Se cumple la normaliddad
LeveneTest(flav~exposicion, data = h_joven, center="mean")        # Se cumple la homocedasticidad
t.test(flav~exposicion, data = h_joven)                           # p > 0.05

tapply(h_senescente$flav, h_senescente$exposicion, shapiro.test)  # Se cumple la normaliddad
LeveneTest(flav~exposicion, data = h_senescente, center="mean")   # Se cumple la homocedasticidad
t.test(flav~exposicion, data = h_senescente)                      # p > 0.05


# Antocianinas

pig_pol_pino %>% 
  group_by(hoja, exposicion) %>% 
  summarise(media = mean(antocianinas), sd = sd(antocianinas)) %>%
  ggplot(aes(hoja, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  geom_text(data = tibble(x=1, y=.3 ),
            aes(x=x, y=y, label="*"), inherit.aes = FALSE, size=14) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,.5),
                     breaks = seq(0,.5, .1)) +
  labs(title = "Nivel de antocianinas de la hoja de Pino",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Estadío de la hoja",
       y = "Antocianinas",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.5,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))

#------------------ Diferencias significativas antocianinas Pino?--------------#
tapply(h_adulta$antocianinas, h_adulta$exposicion, shapiro.test)          # Se cumple la normaliddad
LeveneTest(antocianinas~exposicion, data = h_adulta, center="mean")       # No se cumple la homocedasticidad
t.test(antocianinas~exposicion, data = h_adulta)                          # p > 0.05

tapply(h_joven$antocianinas, h_joven$exposicion, shapiro.test)            # Se cumple la normaliddad
LeveneTest(antocianinas~exposicion, data = h_joven, center="mean")        # Se cumple la homocedasticidad
t.test(antocianinas~exposicion, data = h_joven)                           # p < 0.05*

tapply(h_senescente$antocianinas, h_senescente$exposicion, shapiro.test)  # Se cumple la normaliddad
LeveneTest(antocianinas~exposicion, data = h_senescente, center="mean")   # Se cumple la homocedasticidad
t.test(antocianinas~exposicion, data = h_senescente)                      # p > 0.05


####### Análisis multivariante PCA de Pigmentos y polifenoles de Pino  #########

matrix_pp_pino <- pig_pol_pino[,c("nbi","chl","flav","antocianinas")] 
pca_pp_pino <- prcomp(matrix_pp_pino, center=T,scale=T)
summary(pca_pp_pino) # PC1 = 55.54% de la varianza explicada, PC2 = 29.38% --> PC1 + PC2 = 84.92 Vamos a ver

componentes <- as.data.frame(pca_pp_pino$x)

df_comp_prin_pino <- data.frame(
  PC1=componentes$PC1,
  PC2=componentes$PC2,
  PC3=componentes$PC3,
  hoja=pig_pol_pino$hoja,
  exposicion=pig_pol_pino$exposicion
)

# Correlacion entre las componentes y el pino
cor(componentes[,-c(3,4)],matrix_pp_pino)

# Dejo la libre interpretación en estos PCAs
df_comp_prin_pino %>% 
  ggplot(aes(PC1,PC2, fill=hoja, col=hoja)) +
  geom_point(pch=21) +
  stat_ellipse(geom="polygon", alpha=.15, size=1) +
  scale_fill_manual(values = c("forestgreen","red","blue")) +
  scale_color_manual(values = c("forestgreen","red","blue")) +
  labs(
    title = "PCA, variables pigmentos y fenoles, Pino",
    subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
    x="PC1 (55.54% de la varianza explicada)",
    y="PC1 (29.38% de la varianza explicada)"
  ) +
  theme_classic()

## Y las diferencias entre las hojas?
# PC1:
tapply(df_comp_prin_pino$PC1, df_comp_prin_pino$hoja, shapiro.test)               # Hay normalidad en los datos
LeveneTest(PC1~hoja, data = df_comp_prin_pino, center="mean")                     # Hay homocedasticidad en los datos
anova_pc1_pino <- aov(PC1~hoja, data = df_comp_prin_pino);summary(anova_pc1_pino) # p < 0.05*
TukeyHSD(anova_pc1_pino)                                                          # p > 0.05 Senescentes-Adultas

#PC2
tapply(df_comp_prin_pino$PC2, df_comp_prin_pino$hoja, shapiro.test)                               # Hay normalidad en los datos
LeveneTest(PC2~hoja, data = df_comp_prin_pino, center="mean")                                     # No hay homocedasticidad en los datos
library(rstatix)
anova.welch_pc2_pino <- welch_anova_test(PC2~hoja, data = df_comp_prin_pino);anova.welch_pc2_pino # p < 0.05*
games_howell_test(PC2~hoja, data = df_comp_prin_pino)                                             # p > 0.05 solo en Joven-Adulta (se veía venir en ambas componentes)

#### Como con la tercera componentes se explica cerca del 99% de la varianza vamos a 
#### echar un vistazo mediante un grafico 3D

df_cp_pino_colores <- df_comp_prin_pino %>% 
  mutate(colores=case_when(hoja == "Joven" ~ "forestgreen",
                           hoja == "Adulta" ~"red",
                           hoja == "Senescente" ~ "blue"))

plot3d(x=df_cp_pino_colores$PC1, y=df_cp_pino_colores$PC2, z=df_cp_pino_colores$PC3,
       col = df_cp_pino_colores$colores, type = "s", size = 1,
       xlab="PC1 (55.54%", ylab="PC2 (29.38%", zlab="14.31%")

#------------------------------------------------------------------------------#
#                     Estudio de la hoja del Rosalillo                         #
#------------------------------------------------------------------------------#

url2 <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/AFV/practicas_camp/bases_datos/pig_polifenoles_rosalillo.csv"
pig_pol_rosalillo <- read_csv(url2) 
 
#### NBI Rosalillo

pig_pol_rosalillo %>% 
  group_by(h_e, exposicion) %>% 
  summarise(media = mean(nbi), sd = sd(nbi)) %>%
  ggplot(aes(h_e, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,30),
                     breaks = seq(0,30, 5)) +
  labs(title = "NBI de la hoja de Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hoja adulta",
       y = "NBI",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#-------- Diferencias entre NBI Haz y envés / sombra y Sol del Rosalillo? -----#
tapply(pig_pol_rosalillo$nbi, pig_pol_rosalillo$h_e, shapiro.test)                     # Se cumple Normalidad
LeveneTest(nbi~h_e, data = pig_pol_rosalillo, center="mean")                           # Se cumple homocedasticidad
anova_pp_rosa.nbi <- aov(nbi~h_e, data = pig_pol_rosalillo);summary(anova_pp_rosa.nbi) # p > 0.05 

# Clorofilas rosalillo

pig_pol_rosalillo %>% 
  group_by(h_e, exposicion) %>% 
  summarise(media = mean(chl), sd = sd(chl)) %>%
  ggplot(aes(h_e, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,50),
                     breaks = seq(0,50, 10)) +
  labs(title = "Nivel de clorofílas de la hoja de Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hoja adulta",
       y = "Clorofilas",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#------------------------ Diferencias Clorofilas rosalillo? -------------------#
tapply(pig_pol_rosalillo$chl, pig_pol_rosalillo$haz_enves, shapiro.test)                     # Se cumple Normalidad
LeveneTest(chl~h_e, data = pig_pol_rosalillo, center="mean")                           # No se cumple homocedasticidad
welch_anova_test(chl~h_e, data = pig_pol_rosalillo)                                    # p > 0.05

# Flavonoides rosalillo

pig_pol_rosalillo %>% 
  group_by(h_e, exposicion) %>% 
  summarise(media = mean(flav), sd = sd(flav)) %>%
  ggplot(aes(h_e, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,4),
                     breaks = seq(0,4, .5)) +
  labs(title = "Nivel de flavonoides de la hoja de Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hoja adulta",
       y = "Flavonoides",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#----------------------- Diferencias flavinas Rosalillo? ----------------------#
tapply(pig_pol_rosalillo$flav, pig_pol_rosalillo$h_e, shapiro.test)     # No se cumple Normalidad
kruskal.test(flav~h_e, data = pig_pol_rosalillo)                        # p > 0.05

# Antocianinas

pig_pol_rosalillo %>% 
  group_by(h_e, exposicion) %>% 
  summarise(media = mean(antocianinas), sd = sd(antocianinas)) %>%
  ggplot(aes(h_e, media, fill = exposicion)) +
  geom_bar(stat = "identity", position = position_dodge(.7),
           col = "black", width = .6) +
  geom_errorbar(aes(ymin = media+sd, ymax = media-sd), width = .3,
                position = position_dodge(.7)) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,.4),
                     breaks = seq(0,.4, .05)) +
  labs(title = "Nivel de antocianinas de la hoja de Rosalillo",
       subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
       x = "Hoja adulta",
       y = "Antocianinas",
       fill = "Sol/Sombra") +
  theme_tufte() +
  theme(axis.line = element_line(),
        title = element_text(size = 13, face = "bold"),
        axis.title = element_text(size = 17, face = "bold"),
        axis.text.x  = element_text(size = 15, face = "bold"),
        legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("white", "gray"))
#------------------- Diferencias antocianinas Rosalillo? ----------------------#
tapply(pig_pol_rosalillo$antocianinas, pig_pol_rosalillo$h_e, shapiro.test)                       # Se cumple Normalidad
LeveneTest(antocianinas~h_e, data = pig_pol_rosalillo, center="mean")                             # Se cumple la Homocedasticidad
anova_pp_rosa_anto <- aov(antocianinas~haz_enves, data = pig_pol_rosalillo);summary(anova_pp_rosa.nbi)  # p > 0.05

                                                           #           haz sombra-envés sombra; haz sombra-haz-sol   

#######################################################################################
####################### PCA DE LAS ESPECIES DE ROSALILLO y Pino #######################
#######################################################################################

vec_pino <- pig_pol_pino %>%
  mutate(pino = case_when(exposicion == "Sol" ~ "Pino sol",
                          exposicion == "Sombra" ~"Pino sombra")) 

vec_rosalillo <- pig_pol_rosalillo %>%
  mutate(rosalillo = case_when(exposicion == "Sol" ~ "Rosalillo sol",
                               exposicion == "Sombra" ~"Rosalillo sombra")) 

df_total <- data.frame(
  nbi = c(pig_pol_pino$nbi,pig_pol_rosalillo$nbi),
  chl = c(pig_pol_pino$chl, pig_pol_rosalillo$chl),
  flav = c(pig_pol_pino$flav, pig_pol_rosalillo$flav),
  antocianinas = c(pig_pol_pino$flav, pig_pol_rosalillo$flav),
  especie = c(vec_pino$pino, vec_rosalillo$rosalillo)
)

matrix_total <- df_total[,-5]
pca_total <- prcomp(matrix_total, center = T, scale = T)
summary(pca_total) # PC1 = 71.57%, PC2 = 27.94% --> PC1 + PC2 = 99.52% 

componentes_total <- as.data.frame(pca_total$x)

df_pc_total <- data.frame(
  PC1 = componentes_total$PC1,
  PC2 = componentes_total$PC2,
  especie=df_total$especie
) %>% 
  mutate(especie.2 = case_when(especie == "Pino sol" ~ "Pino",
                               especie == "Pino sombra" ~ "Pino",
                               especie == "Rosalillo sol" ~ "Rosalillo",
                               especie == "Rosalillo sombra" ~ "Rosalillo"))

df_pc_total %>% 
  ggplot(aes(PC1,PC2, fill=especie.2, col=especie.2)) +
  geom_point(pch=21) +
  stat_ellipse(geom = "polygon", alpha =.5) +
  scale_fill_manual(values = c("red","blue")) +
  scale_color_manual(values = c("red","blue")) +
  labs(
    title = "PCA, variables pigmentos y fenoles, Pino vs Rosalillo",
    subtitle = "Informe A.F.V.,García-Estupiñán, J.C., Biología ULL",
    x="PC1 (66.57% de la varianza explicada)",
    y="PC1 (26.39% de la varianza explicada)",
    fill="Especie",
    color="Especie"
  ) +
  theme_classic() +
  theme(legend.position = c(.8,.8),
        legend.background = element_rect(color = "black"))

# ¿Existen diferencias significativas entre los grupos? 

# PC1
tapply(df_pc_total$PC1, df_pc_total$especie.2, shapiro.test) # p < 0.05 en ambos, los datos no son normales
wilcox.test(PC1~especie.2, data = df_pc_total) # p < 0.05 en la PC1, existen diferencias significativas

# PC2
tapply(df_pc_total$PC2, df_pc_total$especie.2, shapiro.test)   # p > 0.05 en ambos, los datos son normales
LeveneTest(PC2~especie.2, data = df_pc_total, center = "mean") # p > 0.05 los datos son homocedásticos 
t.test(PC1~especie.2, data = df_pc_total)                      # p < 0.05 en la PC2 también, existen diferencias significativas

 

