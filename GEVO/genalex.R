library(tidyverse)
library(readxl)
library(openxlsx)
library(glue)
library(cowplot)

source("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/GEVO/funciones.R")

ruta_genalex <- "C:/Users/jcge9/Desktop/genalex_genetica_evolutiva/datos_practicas_2022.xls"

##### Preprocesamiento de los datos:
prac_genalex <- read_excel(ruta_genalex)

view(prac_genalex)

prac_genalex %>% ncol

prac_genalex <- prac_genalex[-c(1,2),-c(15:17)] 

colnames(prac_genalex) <- c("id", "poblacion", "a_a25_1", "a_a25_2", "a_d1_1", "a_d1_2", "a_apo_1", "a_apo_2",
                            "a_f13b_1", "a_f13b_2", "a_hs3.23_1", "a_hs3.23_2", "a_hs4.65_1", "a_hs4.65_2")

prac_genalex %>% view

#### Procesamiento de los datos
# Cantidad de muestras de cada población:

#t(
n <- prac_genalex %>%
    group_by(poblacion) %>%
    count()
#)

# poblacion "ANDALUCIA" "ARGELIA" "CANARIAS" "CATALUNA" "CLASE" "MARRUECOS_N"
# n         " 67"       " 47"     "364"      " 60"      " 61"   "111"
# poblacion "MARRUECOS_SE" "MARRUECOS_W" "PVASCO" "SAHARA_OCC" "TUNEZ"
# n         " 49"          "140"         " 96"    " 58"        " 48"

# Número por región:
prac_genalex %>%
    mutate(region=case_when(
       poblacion == "ANDALUCIA" | poblacion == "CANARIAS" | poblacion == "CATALUNA" | poblacion == "CLASE" | poblacion == "PVASCO" ~ "ESPANIA",
       poblacion == "ARGELIA" | poblacion == "MARRUECOS_N" | poblacion == "MARRUECOS_SE" | 
       poblacion == "SAHARA_OCC" | poblacion == "TUNEZ" | poblacion == "MARRUECOS_W" ~ "NAFRICA"
    )) %>%
    group_by(region) %>%
    count()

#   region      n
# 1 ESPANIA   648
# 2 NAFRICA   453

# Creamos los genotipos

gprac_genalex <-  prac_genalex %>% 
    mutate(
        ga25 = genotipo(a_a25_1, a_a25_2),
        gd1 = genotipo(a_d1_1, a_d1_2),
        gapo = genotipo(a_apo_1, a_apo_2),
        gf13b = genotipo(a_f13b_1, a_f13b_2),
        ghs3.23 = genotipo(a_hs3.23_1, a_hs3.23_2),
        ghs4.65 = genotipo(a_hs4.65_1, a_hs4.65_2)
    ) %>% drop_na()
    # select(-id) %>%
    # pivot_longer(-c(poblacion,ga25,gd1,gapo,gf13b,
    #                 ghs3.23,ghs4.65), names_to = ) %>%
    # pivot_longer(-c(poblacion, name, value))

# Frecuencias absolutas de los genotipos y alélicas
frec_abs <- gprac_genalex %>%
    select(-id, -starts_with("a_")) %>% 
    pivot_longer(-poblacion, names_to = "locus", values_to = "genotipo") %>%
    group_by(poblacion, locus, genotipo) %>%
    count() %>% 
    ungroup() %>%
    pivot_wider(names_from = genotipo, values_from = n, values_fill = 0) %>%
    mutate(alelo_1 = 2*`1/1` + `1/2`,
           alelo_2 = 2*`2/2` + `1/2`,
           locus = factor(locus,
                          levels = c("ga25", "gapo","gd1","gf13b",
                                     "ghs3.23","ghs4.65"),
                           labels = c("A25", "APO","D1","F13B",
                                     "HS3.23","HS4.65"))) 

####### Gráficas de las frecuencias absloutas ##########

### Frecuencias alélicas

frec_abs %>% 
    select(-c(`1/1`,`1/2`,`2/2`)) %>%
    pivot_longer(-c(poblacion, locus)) %>%
    mutate(name=factor(name,
                       levels = c("alelo_1", "alelo_2"),
                       labels = c("Alelo 1", "Alelo 2"))) %>%
    ggplot(aes(reorder(poblacion,value), value, fill=locus)) +
    geom_bar(stat="identity", position = "dodge", color="black") +
    coord_flip() +
    facet_wrap(~name, ncol=2) +
    labs(
       title = "Frecuencia absoluta de los alelos",
       y="Frecuencia absoluta",
       x=NULL,
       fill="Locus"
    ) +
    scale_y_continuous(expand=expansion(0),
                       limits = c(0,800)) +
    scale_fill_manual(values = c("orange","black","gray","red",
                                 "blue", "forestgreen")) +
    theme_classic() +
    theme(
        axis.line = element_line(size = 1),
        plot.title = element_text(hjust = .5,face = "bold", size = 14),
        strip.background = element_rect(fill="darkgray", color="darkgray", size=2),
        strip.text = element_text(color="black", face = "bold", size = 11),
        legend.position = c(.9,.5)
    )

# ggsave("frec_abs1.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\GEVO\\graficas",
#       width=8, height = 5)


######### Frecuencias relativas alélicas, heterocigosidad observada y esperada:

frecuencias_relativas <- frec_abs %>%
    mutate(total_alelos = alelo_1+alelo_2,
           total_genotipos = `1/1` + `1/2` + `2/2`,
           f_relat1 = alelo_1/total_alelos,
           f_relat2 = alelo_2/total_alelos,
           het_observada = `1/2`/total_genotipos,
           het_esperada = he(f_relat1,f_relat2)) %>% 
    select(-c(`1/1`,`1/2`,`2/2`,alelo_1,alelo_2, 
           total_alelos, total_genotipos)) %>% view

##### Gráfico de heterocigosidades observadas/esperadas para cada región

frecuencias_relativas %>%
    select(-starts_with("f_")) %>%
    pivot_longer(-c(poblacion, locus), names_to="heterocigosidad", 
                 values_to = "valores") %>%
    mutate(heterocigosidad=factor(heterocigosidad,
                                  levels = c("het_observada", "het_esperada"),
                                  labels = c("Observada", "Esperada"))) %>%
    ggplot(aes(valores, reorder(poblacion, valores),
           fill=heterocigosidad)) +
    geom_bar(stat = "identity", position = "dodge",
             color="black", width=.75) +
    facet_wrap(~locus) +
    labs(
       title = "Heterocigosidad observada/esperada para cada locus",
       y="Frecuencia",
       x=NULL,
       fill="Heterocigosidad"
    ) +
    scale_x_continuous(expand = expansion(0),
                       limits = c(0,.625)) +
    scale_fill_manual(values = c("blue", "red")) +
    theme(
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line = element_line(size = 1),
        plot.title = element_text(hjust = .5,face = "bold", size = 14),
        strip.background = element_rect(fill="darkgray", color="darkgray", size=1),
        strip.text = element_text(color="black", face = "bold", size = 11),
        legend.position = "top"
    )

# ggsave("Heterocigosidad.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\GEVO\\graficas",
#        width=8, height = 6)


####### Estructura poblacional

output_Hr <- frecuencias_relativas %>%
    group_by(poblacion) %>%
    summarise(media_he_pob=mean(het_esperada),
              n=n()) %>% 
    ungroup() %>%
    summarise(ni=(sum(media_he_pob*n))/sum(n))


Fs(frecuencias_relativas$het_esperada, output_Hr$ni,
   frecuencias_relativas$f_relat1, frecuencias_relativas$f_relat2)

############# Ahora quiero hacer la matrix de distancias, pero aún no sé muy bien como hacerla, 
#### con lo que voy a copirla de las diapositivas... shameless. Haber si descubro con que se hace 
#### y la puedo hace bien
clase <- c(0,.011,0,.009,.005,.020,.025,.021,.018,.024,.019)
canarias <- c(0,0,.001,.006,.007,.006,.011,.029,.015,.025,.013)
andalucia <- c(rep(0,4),.002,.007,.015,.031,.014,.022,.013)
catalunia <- c(rep(0,4),.001,.016,.025,.046,.026,.042,.020) 
pvasco <- c(rep(0,5),.025,.032,.043,.028,.050,.026)
marruecos_n <- c(rep(0,7),.011,0,.006,0)
marruecos_w <- c(rep(0,7),.010,.002,.013,.004)
marruecos_se <- c(rep(0,8),.006,.009,0)
tunez <- c(rep(0,9),.004,0)
argelia <- c(rep(0,10),.007)
sahara <- c(rep(0,11))

dist_matrix_places <- data.frame(clase,canarias,andalucia,catalunia,
                          pvasco,marruecos_n,marruecos_w,
                          marruecos_se,tunez,argelia,sahara) %>% 
    mutate(localidad=c("Clase","Canarias","Andalucía","Cataluña","País Vasco","Marruecos N",
                       "Marruecos O", "Marruecos SE", "Túnez", "Argelia","Sahara Occ."),
            region=c(rep("España",5),rep("Africa N",6)))

dist_matrix <- dist_matrix_places %>% 
    select(-localidad,-region)

pcoa <- cmdscale(dist_matrix, k=2, eig = TRUE, add=TRUE) 
colnames(pcoa) <- c("PCoA1","PCoA2")

positions %>%
    as_tibble() %>%
    mutate(region=dist_matrix_places$region,
           localidad=dist_matrix_places$localidad) %>%
    ggplot(aes(PCoA1, PCoA2, fill=region)) +
    geom_text(aes(label=localidad, color=region), show.legend = FALSE) +
    stat_ellipse(geom="polygon", alpha=.25) +
    scale_color_manual(values = c("red","blue")) +
    theme_classic()
