################################################################################
########### PRÁCTICAS DE INFORMÁTICA DE FISIOLOGÍA ANIMAL APLICADA  ############
################################################################################

library(tidyverse)
library(ggthemes)
library(readxl)
library(rstatix)
library(xlsx)

url_artemias <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/FAA/artemias_sripts_datos/matrix_artemias.csv"

matrix_artemias <- read_csv(url_artemias)

# write_csv(matrix_artemias, "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\FAA\\artemias_sripts_datos\\matrix_artemias.csv")

tidy_artemias <- matrix_artemias %>% 
  mutate(tratamiento = Tratamiento,
         tratamiento = case_when(tratamiento == 1 ~ "Levadura",
                                 tratamiento == 2 ~ "Lectina marina",
                                 tratamiento == 3 ~ "Echium/Bacalao",
                                 tratamiento == 4 ~ "Enriquecedor comercial")) %>% 
  select(-Tratamiento) %>% 
  pivot_longer(-tratamiento, names_to  = "acido_graso", values_to = "valores")


#------------------------------------------------------------------------------#
#                           COMPARAR MÁS DE DOS GRUPOS
#                                 INDEPENDIENTES        
#                 ANOVA DE UNA VÍA; ANOVA DE WELCH; KRUSKAL-WALLIS
#------------------------------------------------------------------------------#

# Estudio de la normalidada de los datos...
# No son normales:
# tratamiento               acido_graso       p
# 1 Echium/Bacalao            C 18:3n-3   0.00861
# 2 Enriquecedor comercial    C 16:0      0.0497 

normalidad <- tidy_artemias %>% 
  group_by(tratamiento, acido_graso) %>% 
  shapiro_test(valores) %>% 
  filter(p < 0.05) %>% select(tratamiento, acido_graso, p)

normalidad

# Estudio de la homocedasticidad de los datos. No son homocedásticos:
# acido_graso       p
# 1 C 16:1n-7   0.000102
# 2 C 18:0      0.00105 
# 3 C 18:1n-9   0.0190  
# 4 C 18:3n-6   0.0243  
# 5 C 20:5n-3   0.0277  
# 6 C 22:6n-3   0.0203  

levene <- tidy_artemias %>% 
  group_by(acido_graso) %>% 
  mutate(tratamiento=as.factor(tratamiento)) %>% 
  levene_test(valores~tratamiento, center = mean) %>% 
  filter(p < 0.05) %>% select(acido_graso, p)

levene

#### Comparar más de dos grupo independientes cunado se cumplen las  ####
#### asunciones de la normalidad de los datos y la homocedasticidad: ####
####                              ANOVA                              ####

# Ácidos grasos compatibles con ANOVA: C 18:2n-6, C 18:4n-3, C 20:4n-6 
# En el tutorial, también utilizan C 16:0, porque en SPSS redondea el valor
# de Shapiro-Wilks = 0.0497 ~ 0.05, que cada uno tome la decisión que considere
# apropiada...

# Todos presntan diferencias significativas

acidos_anova <- tidy_artemias %>% 
  filter(!(acido_graso %in% c("C 16:0","C 16:1n-7","C 18:0","C 18:1n-9", 
                              "C 18:3n-3","C 18:3n-6","C 20:5n-3","C 22:6n-3"))) #%>% 
# group_by(acido_graso) %>% count()

resultados_anova1 <- acidos_anova %>% 
  group_by(acido_graso) %>% 
  anova_test(valores ~ tratamiento)

resultados_anova1[,c("acido_graso", "p")]

# En tal caso haremos el POST-HOC de Tukey
# Los únicos grupos que no presentan diferencias significativas son:

# acido_graso    comparacion1           comparacion2  significacion
# C 18:2n-6     Enriquecedor comercial    Levadura        ns           
# C 18:4n-3     Enriquecedor comercial    Levadura        ns    

acidos_anova %>% 
  group_by(acido_graso) %>% 
  tukey_hsd(valores ~ tratamiento) %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion = p.adj.signif) #%>% 
  # filter(significacion == "ns")


#### Comparar más de dos grupos indepentdientes en caso de que las variables sean ####
#### Homocedásticas. Para ello se realiza el test de ANOVA de Welch               ####

# Sin embargo, en el tutorial del aula los que hacer es transformar las variables 
# No homocedásticas mediante una técnica d etransformación, usando el arcoseno. Aquí
# voy  a  hacer la transoformación nada más, ya que a mí no me convence lo de transformar
# los datos. Haré el ANOVA de Welch directamente, si de todas formas quieres hacer ANOVA,
# una vez la transformación sigues los pasos anteriores para hacer el modelo.

#### Transofrmación de los datos, variables a usar: 16:0, 16:1n-7, 18:0, 18:1n-9, 18:3n-6, 
#### 20:5n-3 y 22:6n-3. Puedes elegir la opción que quieras que te de valores normales y 
#### homocedásticos.

tidy_artemias %>% 
  filter(acido_graso %in% c("C 16:0", "C 16:1n-7","C 18:0", "C 18:1n-9",
                            "C 18:3n-6", "C 20:5n-3", "C 22:6n-3")) %>% 
  mutate(valores_arcoseno = asin(sqrt(valores)/100),
         valores_cuadrado = valores^2,           
         valores_raiz = sqrt(valores),
         valores_log = log10(valores),
         valores_log_mas_1 = log10(valores + 1), # Valores negativos
         valores_inversa = 1/valores
  ) %>% view()

#### Yo por otro lado voy a hacer los test según la distribución y homocedasticidad de los 
#### datos reales.

# vector con los valores significativos de Levene
vect_no_norm <- normalidad$acido_graso
vect_no_homoc <- levene$acido_graso

anova_welch <- tidy_artemias %>% 
  filter(acido_graso %in% vect_no_homoc & !(acido_graso %in% vect_no_norm)) %>% 
  mutate(tratamiento = as.factor(tratamiento)) %>% 
  group_by(acido_graso)

# ANOVA de Welch. Todos presentan diferencias significativas.
anova_welch %>% 
  welch_anova_test(valores ~ tratamiento) %>% 
  select(acido_graso, p) %>% 
  filter(p < 0.05)                

# POST-HOC de Games-Hawell: no existen diferencias entre:

# acido_graso       comparacion1        comparacion2        significacion
# C 16:1n-7   Enriquecedor comercial Lectina marina               ns           
# C 18:0      Echium/Bacalao         Enriquecedor comercial       ns           
# C 18:0      Echium/Bacalao         Lectina marina               ns           
# C 18:0      Enriquecedor comercial Lectina marina               ns           
# C 18:0      Lectina marina         Levadura                     ns           
# C 18:1n-9   Enriquecedor comercial Lectina marina               ns           
# C 18:3n-6   Enriquecedor comercial Lectina marina               ns           
# C 18:3n-6   Enriquecedor comercial Levadura                     ns           
# C 18:3n-6   Lectina marina         Levadura                     ns           
# C 22:6n-3   Enriquecedor comercial Lectina marina               ns 

# OJO!!! La muestra es < 6, Games Howell no es muy certero, mira como hacer el otro post-hoc en python
anova_welch %>% 
  games_howell_test(valores ~tratamiento) %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion=p.adj.signif) %>% 
  filter(significacion == "ns")


#### Finalmente para los valores que no presentan normalidad: C 16:0 y C 18:3n-3
#### Realizaremos Kruskal-Wallis.

# vector con valores significativos de Shapio Wilks
vect_kw <- normalidad$acido_graso

kw_artemias <- tidy_artemias %>% 
  filter(acido_graso %in% vect_kw) %>% 
  mutate(tratamiento = as.factor(tratamiento)) %>% 
  group_by(acido_graso)

# Ambos grupos presntan diferencias significativas p < 0.05
kw_artemias %>% 
  kruskal_test(valores ~ tratamiento) %>% 
  select(acido_graso, p) 

# Solo hay diferencias significativas entre: 

# acido_graso  comparacion1   comparacion2   significacion
# C 16:0      Echium/Bacalao Lectina marina       **           
# C 18:3n-3   Echium/Bacalao Lectina marina       **      

kw_artemias %>% 
  dunn_test(valores ~ tratamiento, p.adjust.method = "bonf") %>% 
  select(acido_graso, 
         comparacion1=group1, comparacion2=group2, 
         significacion=p.adj.signif) #%>% 
  #filter(significacion == c("*", "**", "***"))




         

