library(readxl)
library(tidyverse)
library(glue)
library(xlsx)
library(rstatix)

# Mi fórmula maestra 
source("https://raw.githubusercontent.com/Juankkar/mis_cosas/main/funciones_propias/inferencia.R")
# write_csv(practica2, "practica2.csv")
practica2 <- read_excel("practicas_abbm.xlsx", 
                             sheet = "Siberio1", col_types = c("numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "text", "text", "skip", "skip", "skip", 
                                                               "skip"), na = "NA") %>% 
  select(alumno, ul_10, ul_20 = yl_20,blanco, tiempo, grupos) %>% view()


### Práctica 1 Técnicas básiscas en el laboratorio.
### Práctica 2 Determinación de la expresión del gen ENA1 de levaduras.



practica2_prep <- practica2 %>% select(-alumno)

experimento <- practica2_prep %>% 
  pivot_longer(-c(tiempo,grupos), names_to = "tipo_exp", values_to = "absorbancia")

experimento %>% 
  group_by(tiempo, tipo_exp) %>% 
  summarise(media=mean(absorbancia, na.rm=T), sd=sd(absorbancia, na.rm=TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(tiempo, media, fill=tipo_exp)) +
  geom_errorbar(aes(ymin=media-sd, ymax=media+sd), 
                width=.3, position = position_dodge(.85)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.9),
                     breaks = seq(0,1.8, .3)) +
  scale_fill_manual(name="Volumen de enzima",
                    breaks = c("blanco", "ul_10", "ul_20"),
                    labels = c("Blanco", "10 μl", "20 μl"),
                    values = c("white", "black", "orange")) +
  labs(
    title = "Absorbanica para determinar la concentración de proteína",
    x = "Tiempo",
    y = "Absrobancia"
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size = .75),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = .75),
    plot.title = element_text(face = "bold", hjust = .5, 
                              size = 14, margin = margin(b = 20)),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.background = element_rect(color = "black", fill = NULL)
  )

#### Pero son estas diferencias significativas?

## Entre 10 microlitros y 20 microlitros (y blanco obviamente --> como son todos 0 el Shapiro se vuelve loco, existen diferencias igualmente vamos xd) de 
# cada grupo

una_hora <- experimento %>% filter(tiempo == "1 Horas" & tipo_exp != "blanco")
tres_hora <- experimento %>% filter(tiempo == "3 Horas" & tipo_exp != "blanco")
cinco_hora <- experimento %>% filter(tiempo == "5 Horas" & tipo_exp != "blanco")

tapply(una_hora$absorbancia, una_hora$tipo_exp, shapiro.test)  # El blanco da problemas, por ser todo 0, 
                                                               # obviamente existen diferencias así que 
                                                               # compararemos únicamente los otros

# En 1 hora T-test: p > 0.05 entre 10 ul y 20 ul
tw.groups(una_hora, absorbancia, "absorbancia", tipo_exp, "ul_10", "ul_20")           

# En 3 hora T-test: p < 0.05* entre 10 ul y 20 ul
tw.groups(tres_hora, absorbancia, "absorbancia", tipo_exp, "ul_10", "ul_20")           

# En 5 hora T-test: p < 0.01*** entre 10 ul y 20 ul
tw.groups(cinco_hora, absorbancia, "absorbancia", tipo_exp, "ul_10", "ul_20")           

## Y entre cada grupo de cada hora?

diez_ul <- experimento %>% filter(tipo_exp == c("ul_10"))
veinte_ul <- experimento %>% filter(tipo_exp == c("ul_20"))
                                    
                                    
# Con 10 ul, ANOVA de una vía: p > 0.05 No existen diferncias en los experimentos entre horas en 10 ul de disolución
th.groups(diez_ul, absorbancia, "absorbancia", tiempo, "1 Horas", "3 Horas", "5 Horas")           

# Con 20 ul, ANOVA de una vía: p > 0.05 No existen diferncias en los experimentos entre horas en 20 ul de disolución
th.groups(veinte_ul, absorbancia, "absorbancia", tiempo, "1 Horas", "3 Horas", "5 Horas") 

########################################################
# Curava patrón para los valores medios de absorvancia #
########################################################

# Tengo apuntado la sigiente curva patrón. Suponiendo un: 
# Falta un valor de la curva original (absorbancia de 12 ug), lo predeciremos un 
# una regresión lineal simple, porque las absorbancias medias se
# salen de la curva. En última instancia usaremos la ecuación de la recta

conc_albumina <- 0.1 # ug/ul

curva_patron <- tibble(
  ug_prot=c(0,2,4,6,8),
  absorbancia=c(0,.150,.282,.362,0.465)
)
lm_albumina <- lm(absorbancia~ug_prot, data = curva_patron)
coef_albumina <- lm_albumina$coefficients
# fórmula de la recta: absorbancia = ug_prot·0.0571 + 0.0234
# Gramos de prot, predecir hasta: 12,16,20,24 ug de albúmina 
ug_predecir <- seq(12,24, 4)
valores_curva_predichos <- tibble(
  ug_prot=ug_predecir,
  absorbancia = ug_predecir*lm_albumina$coefficients[2]+lm_albumina$coefficients[1]
  )
# Juntar ambos tibbles
curva_prediccion <- rbind(curva_patron,valores_curva_predichos)

abs_problema_media10ul <- experimento %>% 
  group_by(tiempo, tipo_exp) %>% 
  summarise(media=mean(absorbancia, na.rm=T), 
            sd=sd(absorbancia, na.rm=T)) %>% 
  filter(!(tipo_exp %in% c("blanco", "ul_20")))

abs_problema_media20ul <- experimento %>% 
  group_by(tiempo, tipo_exp) %>% 
  summarise(media=mean(absorbancia, na.rm=T), 
            sd=sd(absorbancia, na.rm=T)) %>% 
  filter(!(tipo_exp %in% c("blanco", "ul_10")))



absorbancia_media <- tibble(
  media_10ul = c(abs_problema_media10ul$media, abs_problema_media20ul$media),
  colores=c("10 ul: 1 Hora", "10 ul: 3 Horas", "10 ul: 5 Horas",
            "20 ul: 1 Hora", "20 ul: 3 Horas", "20 ul: 5 Horas"),
  ul = c(rep("10ul",3),rep("20ul",3))
)

ug_prot_problema <- tibble(ug_prot_interpolado = c(12.5,9.15,10.3,
                                                   21.12,20.3,22),
                           colores=c("10 ul: 1 Hora", "10 ul: 3 Horas", "10 ul: 5 Horas",
                                     "20 ul: 1 Hora", "20 ul: 3 Horas", "20 ul: 5 Horas"),
                           ul = c(rep("10ul",3),rep("20ul",3))
                           
)

curva_prediccion %>% 
  ggplot(aes(ug_prot, absorbancia)) +
  geom_line(size=1) +
  geom_point(pch=21, fill="white", size=2) +
  geom_hline(data = absorbancia_media, aes(yintercept = media_10ul,
                                 color=colores),
             linetype="dashed", size=.75) +
  geom_vline(data = ug_prot_problema, aes(xintercept = ug_prot_interpolado,
                                          color=colores),
             linetype="dashed", size=.75) +
  facet_wrap(~colores) +
  scale_color_manual(name="vol.extracto\ny Hora", values = c("red", "blue", "forestgreen",
                                       "magenta", "cyan", "green")) +
  labs(
    title = "Recta patrón para identificar la cantidad de proteína problema",
    x = "ug de albúmina",
    y = "Absorbancia",
    caption = glue("Resultados en orden de la legenda (ug):\n{ug_prot_problema[,1]}")
  ) +
  theme_classic() +
  theme(
    axis.line = element_line(size=1),
    axis.ticks = element_line(size=1),
    plot.title = element_text(hjust = .5, face = "bold", size=14, 
                              margin = margin(b=10)),
    plot.caption=element_text(hjust = 0, size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(hjust = .5, face = "bold", size=12),
    legend.title = element_text(face = "bold", hjust = .5),
    #legend.position = "bottom",
    legend.background = element_rect(color = "black"),
    strip.background = element_rect(size = 2, color="black",
                                    fill = "tomato"),
    strip.text = element_text(color = "white", face = "bold")
  )
  

#-------------------------------------------------------------------------------#
#                                 Práctica 6                                    #
#-------------------------------------------------------------------------------#

practica_6 <- read_excel("practicas_abbm.xlsx", 
                       sheet = "Bradford") %>% drop_na()



practica_6_absorbancias <- practica_6 %>% 
  select(alumno, grupo, tubo_blanco=blanco, starts_with("tubo")) %>% 
  pivot_longer(-c(alumno, grupo), names_to = "tubo", values_to = "absorbancia")

recta <- practica_6_absorbancias %>% filter(!(tubo %in% c("tubo6","tubo7")))
muestras <- practica_6_absorbancias %>% filter(tubo %in% c("tubo6","tubo7"))
  

practica_6_alb <- practica_6 %>% 
  select(alumno, grupo, starts_with("onpg")) %>% 
  pivot_longer(-c(alumno, grupo), names_to = "onpg", values_to = "conc_onpg") %>% 
  select(-alumno, -grupo)


tidy_recta <- cbind(recta,practica_6_alb) %>% view()

tidy_recta %>% 
  filter(!(tubo %in% c("tubo6","tubo7"))) %>% 
  ggplot(aes(conc_onpg, absorbancia, color= grupo, fill=grupo)) +
  geom_point(size=3, pch=21, color="black", size=1) +
  #geom_line(size=1) +
  geom_smooth(se=F, size=1.5) +
  facet_wrap(~grupo) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.2)) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(0,15)) +
  scale_color_manual(name="Hora",
                     breaks = c("Grupo 1","Grupo 2","Grupo 3", "Grupo 4"),
                     values = c("gray", "red", "blue", "forestgreen")) +
  scale_fill_manual(name="Hora",
                     breaks = c("Grupo 1","Grupo 2","Grupo 3", "Grupo 4"),
                     values = c("gray", "red", "blue", "forestgreen")) +
  labs(
    title = "Práctica 6 para cada grupo",
    x = "Concentración de Albúmina (μg)",
    y = "Absorbancia"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size=15, color = "black", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "black", size=.75), 
    axis.text = element_text(color = "black"),
    axis.title = element_text(color="black", face = "bold"),
    panel.background = element_rect(fill="white", color = "white"),
    plot.background = element_rect(fill="white", color = "white"),
    legend.background = element_rect(fill="white", color = "white"),
    legend.text = element_text(color = "black"),
    legend.key = element_rect(fill = "white", color="black"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.background = element_rect(size = 2, color="black",
                                    fill = "tomato"),
    strip.text = element_text(color = "white", face = "bold")
  )


# Interpolaresmos con mi resultado (alumno 1)
# Absorbancia de la proteína, que tengo apuntada:

muestra_media <- muestras %>% 
  filter(tubo %in% c("tubo6","tubo7")) %>% 
  group_by(tubo) %>% summarise(media=mean(absorbancia)) %>% 
  mutate(ug_prot=c(5.35,7.35))

alumno_elegido <- "31"
tidy_recta %>% 
  filter(!(alumno %in% c("36","37"))) %>%  # outliers
  # mutate(
  #   alumno_elegido = alumno == alumno_elegido,
  #   alumno = fct_reorder(factor(alumno), alumno_elegido)
  # ) %>%
  ggplot(aes(conc_onpg, absorbancia, 
             #color=alumno_elegido, 
             group=alumno)) +
  geom_line(size=1, color="gray") +
  geom_smooth(aes(conc_onpg, absorbancia, group=1),
              se=FALSE, color="black") +
  geom_hline(data = muestra_media, 
             aes(yintercept=media,color=tubo),
             linetype="dashed",size=.85) +
  geom_vline(data = muestra_media,aes(xintercept=ug_prot,
                                      color=tubo),
             linetype="dashed",size=.85) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.2)) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(0,13),
                     breaks=seq(0,13,2)) +
  scale_color_manual(values = c("red", "blue")) +
  labs(
    title = "Resultado de la práctica 6",
    x = "Microgramos de albúmina",
    y = "Absorbancia",
   # caption=glue("Interpolación de la proteína, alumno: ({mejor10_ul}/10)*10 = {resultado_10_mejor} μg/μl\nB. Concentración de la proteína de mí muestra: ({mio10_ul}/10)*10  = {resul_mio10} μg/μl")
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size=13, color = "black", 
                              face = "bold", margin = margin(b=30)),
    plot.caption = element_text(face = "italic", hjust = 0, size = 9),
    axis.line = element_line(color = "black", size=.75), 
    axis.text = element_text(color = "black"),
    axis.title = element_text(color="black", face = "bold"),
    panel.background = element_rect(fill="white", color = "white"),
    plot.background = element_rect(fill="white", color = "white"),
    legend.background = element_rect(fill="white", color = "white"),
    legend.text = element_text(color = "black"),
    legend.key = element_rect(fill = "white", color="black"),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

###################################################################
# Crearemos una función para la actividad específica de la enzima #
###################################################################

# Se puede hacer una función muy sencillita. Y la tenemos para siempre. Si es cierto
# que no podemos meter valores a lo loco, ya que estos no tienen porque estar en las
# unidades adecuadas.

# Abs: Absotbancia
# vT: Volumane total
# E: no me acuerdo exactamente por ahora xd
# conc_prot: concentración de la proteína de estudio
# ve: volumen de extracto de la proteína
# t: tiempo

act_esp <- function(Abs, vT, E, conc_prot, ve, t){
  cuerpo=(Abs*vT)/(E*conc_prot*ve*t)
  return(cuerpo)
}

# Vamos a decir:
resutado6 <- muestra_media$ug_prot*c(10,10)/c(10,20)
aborbancia6 <- muestra_media$media
# absorbancia_ae
# ug_prot_ae
volumen_total_ul <- 850 # ul
volumen_total_ml <- volumen_total_ul*(1/10^6) # l
E <- 4.5 # Mm^-1*cm^-1
conc_prot <- resutado6 *1000/1000 # mg/ml
v_extracto_ml <- 0.01 # ml
tiempo <- 25 # min

act_esp(Abs = aborbancia6, vT = volumen_total_ml,
        E = E, conc_prot = resutado6, ve = v_extracto_ml, t = tiempo)*1000
