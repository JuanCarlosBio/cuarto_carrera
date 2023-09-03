################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(rstatix)
library(glue)
library(ggtext)
library(plotly)

if(!(require(psych))){
  isntall.packages("psych")
} else {
  library(psych)
}

#------------------------------------------------------------------------------#
#                      ANÁLISIS DE COMPONENTES PRINCIPALES
#                                       (PCA)        
#                 ANOVA DE UNA VÍA; ANOVA DE WELCH; KRUSKAL-WALLIS
#------------------------------------------------------------------------------#

# Para este caso no nos interesa las variables como ters columnas, si no en versión
# ancha, o al menos por el momento.
matrix_artemias <- read_csv("artemias/data/matrix_artemias.csv") %>% 
  mutate(tratamiento = Tratamiento,
         tratamiento = case_when(tratamiento == 1 ~ "Levadura",
                                 tratamiento == 2 ~ "Lectina marina",
                                 tratamiento == 3 ~ "Echium/Bacalao",
                                 tratamiento == 4 ~ "Enriquecedor comercial")) %>% 
  select(-Tratamiento) %>%  view()

ncol(matrix_artemias)

# Lo primero es deshacernos del la variable de tratamientos y convertirla en una 
# matriz numérica.

matrix.artem.num <- matrix_artemias[,-12]

# función de la PCA: tenemos que centrar los datos (lo que entiendo que es rotarlos xd) 
# y escalarlos.

pca <- prcomp(matrix.artem.num, scale = T, center = TRUE)

# Vamos a ver un resumen del pca para sacar información que viene bien.

summary(pca)

# Lo interesante de la función de arriba es la proproción de la varianza
# Explicada por las primeras componentes.
# La PC1 explica un total del 61.28% de la varianza
# La PC2 explica un total del 22.54% de la varianza
# La PC3 explica un total del 14.14% de la varianza
# Entre las tres explican un total de 97.96% de la varianza acumulada explicada 

# Correlación (Pearson) entre las variables y las componentes. De aquí se puede 
# sacar información interesante, dependindo de los valores, se encontrarán en una 
# posición u otra del gráfico que haremos

t(cor(matrix.artem.num, pca$x[,c(1,2)]))

#### en ese sentido vamos a obtener los valores de las 3 primeras componentes. Y
#### Vamos a añadir el vector de los grupos del que prescindimos en el principio

princ_comp <- as_tibble(pca$x[,c(1,2,3)]) %>% 
  mutate(tratamiento=matrix_artemias$tratamiento)


# además vamos a obtener la cada una de las varianzas para las componentes principales

resumen <- summary(pca)
var_pc1 <- round(resumen$importance[2,1]*100, 2)
var_pc2 <- round(resumen$importance[2,2]*100, 2)
var_pc3 <- round(resumen$importance[2,3]*100, 2)
var_tot <- round(resumen$importance[3,3]*100, 2)
var_pc1_pc2 <- var_pc1 + var_pc2

#------------------------------------------------------------------------------#
#                       Visualización de los datos 
#------------------------------------------------------------------------------#

princ_comp %>% 
  mutate(tratamiento= factor(tratamiento,
                             levels = c("Levadura", "Lectina marina",
                                        "Echium/Bacalao", "Enriquecedor comercial"),
                             labels = c("Levadura", "Lectina\nmarina",
                                        "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial"))) %>% 
  ggplot(aes(PC1, PC2, fill=tratamiento, color = tratamiento)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Tratamiento:",
                    values = c("skyblue", "orange", "tomato", "gray")) + 
  scale_color_manual(name="Tratamiento:",
                     values = c("skyblue", "orange", "tomato", "gray")) +
  labs(
    title = glue("PCA para cada tratamieto según su contenido en ácidos<br>grasos: <span style = 'color: red'>varianza explicada acumulada = {var_pc1_pc2}%</span>"),
    x=glue("PC1 ({var_pc1}% varianza explicada)"),
    y=glue("PC2 ({var_pc2}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_markdown(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

ggsave("artemias/data/pca_artemias.png",
       width = 6, 
       height = 6)

# Una forma de entender mejor el PCA es aplicar el "Varimax rotation".
# El tema de las componentes rotadas es una cosa que se mes escapa por el mo
# mento, pero en principio ayuda a una mejor interpretación de las componentes

rpca <- principal(matrix.artem.num, nfactors = 2, # Selecionamos las dos primeras componentes
                  rotate = "varimax", scores = TRUE)

# Estudiamos esta vez la correlación de las variables con las componentes rotadas
cor(rpca$scores[,c(1,2)], matrix.artem.num) 

var_rotado1_label <- round(rpca$Vaccounted[2,1]*100,2)
var_rotado2_label <- round(rpca$Vaccounted[2,2]*100,2)
var_rtotal_exp <- var_rotado1_label + var_rotado2_label

componentes_rotados <- as_tibble(rpca$scores[,c(1,2)]) %>%
  mutate(tratamiento=matrix_artemias$tratamiento)

componentes_rotados %>% 
  mutate(tratamiento= factor(tratamiento,
                             levels = c("Levadura", "Lectina marina",
                                        "Echium/Bacalao", "Enriquecedor comercial"),
                             labels = c("Levadura", "Lectina\nmarina",
                                        "Aceite\nEchium/Bacalao", "Enriquecedor\ncomercial"))) %>% 
  ggplot(aes(RC1, RC2, fill=tratamiento, color = tratamiento)) +
  geom_point(pch=21, color="black") +
  stat_ellipse(geom = "polygon", alpha = .25) +
  geom_vline(xintercept = 0, color="black", linetype="dashed") +
  geom_hline(yintercept = 0, color="black", linetype="dashed") +
  scale_fill_manual(name="Tratamiento:",
                    values = c("skyblue", "orange", "tomato", "gray")) + 
  scale_color_manual(name="Tratamiento:",
                     values = c("skyblue", "orange", "tomato", "gray")) +
  labs(
    title = glue("PCA rotado (Varimax) para cada tratamieto según su contenido<br>en ácidos grasos <span style = 'color: red'>Varianza explicada acumulada = {var_rtotal_exp}%"),
    x=glue("RC1 ({var_rotado1_label}% varianza explicada)"),
    y=glue("RC2 ({var_rotado2_label}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_markdown(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

ggsave("artemias/results/plots/rpca.png", 
       width = 6, 
       height = 6)

# ¿Existen diferencias entre cada grupo de ambas componentes principales?

tidy_rc <- componentes_rotados %>%
  pivot_longer(-tratamiento, names_to = "componentes", values_to = "valores")

# Normalidad de los datos, todas las variables son normales.
tidy_rc %>% 
  group_by(componentes, tratamiento) %>% 
  shapiro_test(valores) %>% 
  filter(p > 0.05)

# Hocedasticidad de los datos. PC2 no presenta homocedasticidad

tidy_rc %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  group_by(componentes) %>% 
  levene_test(valores ~ tratamiento)

# ANOVA de una vía PC1. Hay diferencias significativas 
tidy_rc %>% 
  filter(componentes == "RC1") %>% 
  group_by(componentes) %>% 
  anova_test(valores ~ tratamiento) 

# Tukey
tidy_rc %>% 
  filter(componentes == "RC1") %>% 
  group_by(componentes) %>% 
  tukey_hsd(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)

# ANOVA de Welch. Hay diferencias significativas 
tidy_rc %>% 
  filter(componentes == "RC2") %>% 
  group_by(componentes) %>% 
  welch_anova_test(valores ~ tratamiento) 

# Games-Howell. Todos presentan diferencias significativas OJO!!!, n < 6, no es post-hoc más preciso
tidy_rc %>% 
  filter(componentes == "RC2") %>% 
  group_by(componentes) %>% 
  games_howell_test(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)

RC2 <- tidy_rc %>% 
  filter(componentes == "RC2")

write_csv(RC2, "artemias/data/rca.csv")

#------------------------------------------------------------------------------#
#                       Gráfico 3D para ver las componentes 
#                        una 3º Dimensión (PC3) añadiendo 
#------------------------------------------------------------------------------#

plot_ly(princ_comp, 
        x = ~PC1, y = ~PC2, z = ~PC3, 
        color = ~tratamiento, 
        colors = c('tomato','gray','orange','skyblue') ) %>%
  add_markers(size = 12) %>%
  layout(
    title = glue('Total Explained Variance = {var_tot}'),
    scene = list(xaxis = list(title = glue('PC1 ({var_pc1}% varianza explicada)'),
                              zerolinewidth = 1,
                              ticklen = 5,
                              gridwith = 2),
                 zaxis = list(title = glue('PC2 ({var_pc2}% varianza explicada)'),
                              zerolinewidth = 1,
                              ticklen = 5,
                              gridwith = 2),
                 yaxis = list(title = glue('PC3 ({var_pc3}% varianza explicada)'),
                              zerolinewidth = 1,
                              ticklen = 5,
                              gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
  ) 
