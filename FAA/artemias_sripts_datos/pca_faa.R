################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)
library(glue)
library(ggtext)
library(xlsx)


#------------------------------------------------------------------------------#
#                      ANÁLISIS DE COMPONENTES PRINCIPALES
#                                       (PCA)        
#                 ANOVA DE UNA VÍA; ANOVA DE WELCH; KRUSKAL-WALLIS
#------------------------------------------------------------------------------#

# Para este caso no nos interesa las variables como ters columnas, si no en versión
# ancha, o al menos por el momento.

# write_csv(matrix_artemias, "matrix_artemias.csv")

url_artemias <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/matrix_artemias.csv"

matrix_artemias <- read_csv(url_artemias) %>% 
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

pca <- prcomp(matrix.artem.num, scale = T, center = T)

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

cor(matrix.artem.num, pca$x[,c(1,2)])

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
    title = "PCA para cada tratamieto según su\ncontenido en ácidos grasos",
    x=glue("PC1 ({var_pc1}% varianza explicada)"),
    y=glue("PC2 ({var_pc2}% varianza explicada)")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(size = 14, face = "bold", hjust = .5,
                              margin = margin(b=30)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(color = "white")
  )

# ¿Existen diferencias entre cada grupo de ambas componentes principales?

tidy_pc <- princ_comp %>%
  select(-PC3) %>% 
  pivot_longer(-tratamiento, names_to = "componentes", values_to = "valores")

# Normalidad de los datos, todas las variables son normales.
tidy_pc %>% 
  group_by(componentes, tratamiento) %>% 
  shapiro_test(valores) %>% 
  filter(p > 0.05)

# Hocedasticidad de los datos. PC2 no presenta homocedasticidad

tidy_pc %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  group_by(componentes) %>% 
  levene_test(valores ~ tratamiento)

# ANOVA de una vía PC1. Hay diferencias significativas 
tidy_pc %>% 
  filter(componentes == "PC1") %>% 
  group_by(componentes) %>% 
  anova_test(valores ~ tratamiento) 

# Tukey
tidy_pc %>% 
  filter(componentes == "PC1") %>% 
  group_by(componentes) %>% 
  tukey_hsd(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)

# ANOVA de Welch. Hay diferencias significativas 
tidy_pc %>% 
  filter(componentes == "PC2") %>% 
  group_by(componentes) %>% 
  welch_anova_test(valores ~ tratamiento) 

# Tukey. Todos presentan diferencias significativas 
tidy_pc %>% 
  filter(componentes == "PC1") %>% 
  group_by(componentes) %>% 
  games_howell_test(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)



#------------------------------------------------------------------------------#
#                       Gráfico 3D para ver las componentes 
#                        una 3º Dimensión (PC3) añadiendo 
#------------------------------------------------------------------------------#


library(plotly)

X <- subset(matrix_artemias, select = -c(tratamiento))

prin_comp <- prcomp(X, rank. = 3)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, matrix_artemias$tratamiento)

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = glue('Total Explained Variance = {var_tot}')

fig <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~matrix_artemias$tratamiento, colors = c('skyblue','orange','tomato','darkgray') ) %>%
  add_markers(size = 12)


fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "white")
  )

fig
