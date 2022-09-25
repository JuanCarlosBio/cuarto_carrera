library(tidyverse)
library(readxl)
library(tidytext)
library(rstatix)
library(glue)
library(ggtext)
library(psych)


artemias_2022_csv <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/artemias_2022/artemias_2022.csv")

# write_csv(artemias_2022_csv, "artemias_2022.csv")

artemias_2022_prep1 <- t(artemias_2022_csv)

colnames(artemias_2022_prep1) <- artemias_2022_prep1[1,] 

artemias_prep2 <- artemias_2022_prep1[-1,]

variable_tratamiento <- rownames(artemias_prep2)

artemias_2022 <- artemias_prep2 %>% 
  as_tibble() %>% 
  mutate(tratamiento_grupo=variable_tratamiento,
         tratamiento=case_when(grepl("levadura", tratamiento_grupo) ~ "Levadura",
                               grepl("lectina", tratamiento_grupo) ~ "Lectina marina",
                               grepl("echium", tratamiento_grupo) ~ "Echium/Bacalao",
                               grepl("comercial", tratamiento_grupo) ~ "Enriquecedor comercial"),
         ) %>% 
  select(-tratamiento_grupo) %>% 
  pivot_longer(-tratamiento, names_to = "acido_graso", values_to = "porcentaje") %>% 
  mutate(porcentaje=as.numeric(porcentaje))

# Tabla de PERFIL MEDIO DE ÁCIDOS GRASOS (%) DE LA ARTEMIA DE LOS CUATRO
# TRATAMIENTOS APLICADOS PARA EL ENRIQUECIMIENTO DE ARTEMIA

tabla_perfil_medio_ag <- artemias_2022 %>% 
  group_by(tratamiento, acido_graso) %>% 
  summarise(media=mean(porcentaje, na.rm=TRUE)) %>% 
  pivot_wider(names_from = "tratamiento", values_from = "media") %>% 
  select(acido_graso, Levadura, `Lectina marina`, `Echium/Bacalao`, `Enriquecedor comercial`)


# Gráfico de barras.

artemias_2022 %>% 
  mutate(tratamiento=factor(tratamiento,
                            levels = c("Levadura", "Lectina marina",
                                       "Echium/Bacalao","Enriquecedor comercial"),
                            labels = c("Levadura", "Lectina\nmarina",
                                       "Aceite\nEchium\nBacalao", "Enriquecedor\ncomercial"))) %>% 
  #group_by(tratamiento, acido_graso) %>% 
  #summarise(media=mean(porcentaje, na.rm=TRUE), 
  #          sd=sd(porcentaje, TRUE)) %>% 
  #ungroup() %>% 
  ggplot(aes(acido_graso, porcentaje, fill=tratamiento)) +
  geom_boxplot() +
  stat_summary(fun = "mean", color = "red", size = .25, width =.7,
               position = position_dodge(.7), geom="crossbar", show.legend = FALSE) +
  facet_wrap(~acido_graso, scales = "free") +
  #geom_bar(stat = "identity", position = "dodge",
  #         color="black") + 
  #geom_errorbar(aes(ymin=media-sd, ymax=media+sd),
  #              position = "dodge") +
  #scale_y_continuous(expand = expansion(0),
  #                   limits = c(0,27),
  #                   breaks = seq(0,27,5)) +
  scale_fill_manual(values = c("skyblue", "orange", "forestgreen", "gray")) +
  labs(
    title = "Perfil medio de AG (%) de la artemia de los cuatro\ntratamientos para el enriquecimiento de artemia (2022)",
    x = "Ácidos grasos",
    y = "Perfil de AG (%)", 
    fill="Tratamiento"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1),
    axis.ticks.y = element_line(size = 1),
    axis.ticks.x = element_blank(), 
    plot.title = element_text(face = "bold", hjust = .5, size=16, 
                              margin = margin(b=10)),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 13, face="bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(margin = margin(r=10)),
    legend.position = c(.9,.175),
    legend.text = element_text(margin = margin(b=5)),
    legend.background = element_rect(color = "black"),
    strip.text = element_text(color="black", face = "bold", size = 11),
    strip.background = element_rect(size=1)
  )

# ggsave("artemias_2022.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\artemias_2022",
#       width = 9.5, height = 6)

##### Análisis estadístico #####

# No son normales:
# tratamiento     acido_graso       p
# Echium/Bacalao    18:3n-3       0.0312 
# Echium/Bacalao    20:4n-6       0.00124

normalidad <- artemias_2022 %>%
  drop_na() %>% 
  filter(acido_graso != "18:3n-6") %>% 
  group_by(tratamiento, acido_graso) %>% 
  shapiro_test(porcentaje) %>% 
  filter(p < 0.05) %>% select(tratamiento, acido_graso, acido_graso, p)

normalidad

# Homocedasticidad, no lo son:
# acido_graso       p
#   16:1n-7     0.000957
#   18:0        0.00355 
  
levene <- artemias_2022 %>%
  drop_na() %>% 
  filter(acido_graso != "18:3n-6") %>% 
  group_by(acido_graso) %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  levene_test(porcentaje ~ tratamiento) %>% 
  filter(p < 0.05) %>% select(acido_graso, acido_graso, p)

levene

# ANOVA de una vía
vector_no_norm <- normalidad$acido_graso
vector_no_homc <- levene$acido_graso

artemias_anova <- artemias_2022 %>% 
  filter(!(acido_graso %in% c("18:3n-3","20:4n-6",
                              "16:1n-7","18:0","18:3n-6")))  %>% 
  drop_na()

resultados_anova1 <- artemias_anova %>% 
  group_by(acido_graso) %>% 
  anova_test(porcentaje ~ tratamiento)

resultados_anova1[,c("acido_graso", "p")]

## Tukey

artemias_anova %>% 
  group_by(acido_graso) %>% 
  tukey_hsd(porcentaje ~ tratamiento) %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion = p.adj.signif) %>% print(n=Inf)

# ANOVA de Welch

anova_welch <- artemias_2022 %>% 
  filter(acido_graso %in% vector_no_homc, !(acido_graso %in% vector_no_norm)) %>% 
  mutate(tratamiento = as.factor(tratamiento)) %>% 
  group_by(acido_graso)

anova_welch %>% 
  welch_anova_test(porcentaje ~ tratamiento) %>% 
  select(acido_graso, p)    

# Post-Hoc Games-Howell

anova_welch %>% 
  games_howell_test(porcentaje ~tratamiento) %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion=p.adj.signif)

### Kruskal-Wallis

kw_artemias <- artemias_2022 %>% 
  filter(acido_graso %in% vector_no_norm) %>% 
  mutate(tratamiento = as.factor(tratamiento)) %>% 
  group_by(acido_graso)

# Ambos grupos presntan diferencias significativas p < 0.05
kw_artemias %>% 
  kruskal_test(porcentaje ~ tratamiento) %>% 
  select(acido_graso, p) 

kw_artemias %>% 
  dunn_test(porcentaje ~ tratamiento, p.adjust.method = "bonf") %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion=p.adj.signif) 



#######################################
# Análisis de componentes principales #
#######################################

artemias_wider <- artemias_2022 %>% 
  group_by(tratamiento,acido_graso) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = acido_graso, values_from = porcentaje) %>%
  select(-rn) %>% 
  filter(!(`16:1n-7` %in% NA))

artemias_wider[is.na(artemias_wider)] <- 0  

matrix_artemias <- artemias_wider[,-1]

rpca <- principal(matrix_artemias, nfactors = 2, 
                  rotate = "varimax", scores = TRUE)

# Estudiamos esta vez la correlación de las variables con las componentes rotadas
cor(rpca$scores[,c(1,2)], matrix_artemias) 

var_rotado1_label <- round(rpca$Vaccounted[2,1]*100,2)
var_rotado2_label <- round(rpca$Vaccounted[2,2]*100,2)
var_rtotal_exp <- var_rotado1_label + var_rotado2_label

cp_2022 <- as_tibble(rpca$scores[,c(1,2)]) %>%
  mutate(tratamiento=artemias_wider$tratamiento)

cp_2022 %>% 
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

# ggsave("artemias_pca2022.png", 
#        path="C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\artemias_2022",
#        width = 7, height = 4.75)

# ¿Existen diferencias entre cada grupo de ambas componentes principales?

tidy_cp <- cp_2022 %>%
  pivot_longer(-tratamiento, names_to = "componentes", values_to = "valores")

# Normalidad de los datos, todas las variables son normales.
tidy_cp %>% 
  group_by(componentes, tratamiento) %>% 
  shapiro_test(valores) %>% 
  filter(p > 0.05)

# Homocedasticidad de los datos. PC2 no presenta homocedasticidad

tidy_cp %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  group_by(componentes) %>% 
  levene_test(valores ~ tratamiento)

# ANOVA de una vía PC1. Hay diferencias significativas 
tidy_cp %>% 
  filter(componentes == "RC1") %>% 
  group_by(componentes) %>% 
  anova_test(valores ~ tratamiento) 

# Tukey
tidy_cp %>% 
  filter(componentes == "RC1") %>% 
  group_by(componentes) %>% 
  tukey_hsd(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)

# ANOVA de Welch. Hay diferencias significativas 
tidy_cp %>% 
  filter(componentes == "RC2") %>% 
  group_by(componentes) %>% 
  welch_anova_test(valores ~ tratamiento) 

# Games Howell. Todos presentan diferencias significativas 
tidy_cp %>% 
  filter(componentes == "RC2") %>% 
  group_by(componentes) %>% 
  games_howell_test(valores ~ tratamiento) %>% 
  select(componentes, comparacion1=group1, 
         comparacion2=group2, significacion=p.adj.signif)



 



