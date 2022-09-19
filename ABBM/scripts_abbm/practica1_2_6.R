library(readxl)
library(tidyverse)
library(glue)
library(xlsx)
library(rstatix)
library(ggtext)

# Mi fórmula maestra 
source("https://raw.githubusercontent.com/Juankkar/mis_cosas/main/funciones_propias/inferencia.R")
# write_csv(practica2, "practica2.csv")

practica2 <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/practica2.csv") 


### Práctica 1 Técnicas básiscas en el laboratorio (que desastre de parte xd).
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
                width=.3, position = position_dodge(.5)) +
  geom_bar(stat = "identity", position = "dodge", color="black",
           width = .5) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.9),
                     breaks = seq(0,1.8, .3)) +
  scale_fill_manual(name=NULL,
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

# ggsave("graficax.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\ABBM\\graficas",
#       width=8, height = 4)

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

#-------------------------------------------------------------------------------#
#           Determinaciñon de ug de proteína mediante curva patrón              #
#-------------------------------------------------------------------------------#

practica_2 <- read_csv("https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/practica_6.csv") 



practica_2_absorbancias <- practica_2 %>% 
  select(alumno, grupo, tubo_blanco=blanco, starts_with("tubo")) %>% 
  pivot_longer(-c(alumno, grupo), names_to = "tubo", values_to = "absorbancia")

recta <- practica_2_absorbancias %>% filter(!(tubo %in% c("tubo6","tubo7")))
muestras <- practica_2_absorbancias %>% filter(tubo %in% c("tubo6","tubo7"))
  

practica_2_alb <- practica_2 %>% 
  select(alumno, grupo, starts_with("onpg")) %>% 
  pivot_longer(-c(alumno, grupo), names_to = "onpg", values_to = "conc_onpg") %>% 
  select(-alumno, -grupo)

tidy_recta <- cbind(recta,practica_2_alb)

# Vamos a ver cuál es el almuno con una correlación más alta, y haremos con el susodicho la interpolación.
# Ños parece que hay varios fieras, tendremos que sacar a suertes al ganador.

alumno_ganadores <- tidy_recta %>% 
  filter(!(tubo %in% c("tubo6","tubo7"))) %>%
  group_by(alumno) %>%
  cor_test(absorbancia, conc_onpg) %>%
  select(alumno, cor) %>%
  filter(cor == .99)

# con la función sacaremos a un alumno al hazar de los que tuvo una correlación de 0.99 ... Redoble de tambores
# sample(alumno_ganadores$alumno, 1) --> alumno 14, lo comento porque continuaré con este pibe por siempre

# Absorbancias del alumno 14 
muestras_ganador <- muestras %>% 
  filter(tubo %in% c("tubo6","tubo7") & alumno == "14") %>%
  mutate(ug_prot = c(3.6, 6.2))

concentracio_tubo6 <- (muestras_ganador[1,5]/10)*10
concentracio_tubo7 <- (muestras_ganador[2,5]/10)*20

alumno_elegido <- "14"

tidy_recta %>% 
  filter(!(alumno %in% c("36","37"))) %>%  # outliers
   mutate(
     alumno_elegido = alumno == alumno_elegido,
     alumno = fct_reorder(factor(alumno), alumno_elegido)
   ) %>%
  ggplot(aes(conc_onpg, absorbancia, 
             color=alumno_elegido, 
             group=alumno)) +
  geom_line(size=1, show.legend = FALSE) +
  geom_hline(data = muestras_ganador, 
             aes(yintercept=absorbancia,color=tubo),
             linetype="dashed",size=.85, show.legend = FALSE) +
  geom_vline(data = muestras_ganador,
             aes(xintercept=ug_prot, color=tubo),
             linetype="dashed",size=.85, show.legend = FALSE) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.1)) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(0,13),
                     breaks=seq(0,13,2)) +
  scale_color_manual(values = c("gray", "red", "blue", "forestgreen")) +
  labs(
    title = "Resultado conjunto, el <span style = 'color: red'>alumno elegido</span><br>es el 14 (cor = 14) y <span style = 'color: darkgray'>el resto</span> de comparativa",
    x = "Microgramos de albúmina",
    y = "Absorbancia",
    caption=glue("Interpolación de la proteína:<br><span style = 'color: blue'>Concentración del tubo 6: ({muestras_ganador[1,5]}/10)*10 = {concentracio_tubo6} μg/μl</span>\n<br><span style = 'color: forestgreen'>Concentración del tubo 7: ({muestras_ganador[2,5]}/10)*20  = {concentracio_tubo7} μg/μl</span>")
  ) +
  theme_classic() +
  theme(
    plot.title = element_markdown(size=13, color = "black", 
                              face = "bold", margin = margin(b=20), hjust=.5),
    plot.caption = element_markdown(face = "italic", hjust = 0, size = 11),
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

ggsave("interpolacion.png", path = "C:\\Users\\jcge9\\Desktop\\cuarto_carrera\\cuarto_carrera\\ABBM\\graficas",
       width=7, height=4.5)

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
resutado_tubos <- unlist(c(concentracio_tubo6, concentracio_tubo7)) 
aborbancia_tubos <- muestras_ganador$absorbancia
# absorbancia_ae
# ug_prot_ae
volumen_total_ul <- rep(850,2) # ul
volumen_total_l <- rep(volumen_total_ul*(1/10^6)) # l
E <- 4.5 # Mm^-1*cm^-1
v_extracto_ml <- rep(0.01,2) # ml
tiempo <- rep(25,2) # min

actividad_especifica <- act_esp(Abs = aborbancia_tubos, vT = volumen_total_l,
        E = E, conc_prot = resutado_tubos, ve = v_extracto_ml, t = tiempo)

resul_act_esp <- as_tibble(actividad_especifica) %>%
  select(actividad_especifica=value) %>%
  mutate(tubo = c("tubo 6", "tubo 7"));resul_act_esp
