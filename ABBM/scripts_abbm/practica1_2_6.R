library(readxl)
library(tidyverse)
library(glue)
library(xlsx)

# Mi fórmula maestra 
source("https://raw.githubusercontent.com/Juankkar/mis_cosas/main/funciones_propias/inferencia.R")
# write_csv(practica2, "practica2.csv")
url_practica2 <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/practica2.csv"
practica2 <- read_csv(url_practica2) %>% 
  select(alumno, ul_10, ul_20 = yl_20,blanco, tiempo, grupos)


### Práctica de Siberio 1.

practica2_prep <- practica2 %>% select(-alumno, -grupos)

experimento <- practica2_prep %>% 
  pivot_longer(-tiempo, names_to = "tipo_exp", values_to = "absorbancia")

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
    title = "Absorbancia según el tiempo de reacción\nenzimática (precipitación de sustrato)",
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

# En 1 hora T-test: p < 0.05* entre 10 ul y 20 ul
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
#                                 Práctica 6                                    #
#-------------------------------------------------------------------------------#

#### Curva patrón.

# Va a ser duro poner apunto la base de datos, especiealmente uno de los grupos les faltó apuntar el tiempo
# de reacción, vamos a ver si podemos improvisar algo...

# write_csv(bradford, "bradford.csv")
url_bradford <- "https://raw.githubusercontent.com/Juankkar/cuarto_carrera/main/ABBM/data/bradford.csv"
bradford <- read_excel(url_bradford)

####################
###### 1 Horas #####
####################

brad_1h <- bradford %>% 
  filter(hora == "1 Hora") %>% 
  select(hora, blanco, starts_with("tubo")) %>% 
  pivot_longer(-hora, names_to = "tubos", values_to = "absorbancia")


onpg_1h <- bradford %>% 
  filter(hora == "1 Hora") %>% 
  select(hora, starts_with("onpg")) %>% 
  pivot_longer(-hora, names_to = "sustrato", values_to = "concentracion") 
  
una_bradford <- cbind(brad_1h, onpg_1h)

# Alumnos de 5 horas
bradford %>% filter(hora=="1 Hora") %>% select(alumno, hora)

una_bradford[-1] %>% 
  mutate(alumno = c(rep("alumno10", 8),rep("alumno22", 8),rep("alumno24", 8))) %>% 
  ggplot(aes(concentracion, absorbancia, color= tubos,group=alumno)) +
  geom_point(size=3) +
  geom_line(size=1) +
  geom_text(data=tibble(x=60, y=.8),
            aes(x=x,y=y, label="En pricipio esto no debería\nser así, pero bueno..."),
            inherit.aes = F, color="white") +
  geom_line(data=tibble(x=c(50,70), y=c(.7,.7)),
            aes(x=x,y=y), inherit.aes = F, color="white") +
  scale_color_manual(breaks = c("blanco", "tubo1","tubo2","tubo3",
                                "tubo4","tubo5","tubo6","tubo7"),
                     values = c("white", "red", "blue", "forestgreen",
                                "gray", "yellow", "orange", "magenta")) +
  labs(title = "Reacción en 5 horas",
       x="Concentración de ONPG",
       y="Absorbancia") +
  theme(
    plot.title = element_text(size=15, color = "white", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "white"), 
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white", face = "bold"),
    panel.background = element_rect(fill="black", color = "black"),
    plot.background = element_rect(fill="black", color = "black"),
    legend.background = element_rect(fill="black", color = "black"),
    legend.text = element_text(color = "white"),
    legend.key = element_rect(fill = "azure", color="azure"),
    panel.grid = element_blank())

####################
###### 3 Horas #####
####################

brad_3h <- bradford %>% 
  filter(hora == "3 Horas") %>% 
  select(hora, blanco, starts_with("tubo")) %>% 
  pivot_longer(-hora, names_to = "tubos", values_to = "absorbancia")


onpg_3h <- bradford %>% 
  filter(hora == "3 Horas") %>% 
  select(hora, starts_with("onpg")) %>% 
  pivot_longer(-hora, names_to = "sustrato", values_to = "concentracion") 

tres_bradford <- cbind(brad_3h, onpg_3h)

# Alumnos de 5 horas
bradford %>% filter(hora=="3 Horas") %>% select(alumno, hora)

tres_bradford[-1] %>% 
  mutate(alumno = c(rep("alumno14", 8),rep("alumno21", 8),rep("alumno23", 8),
                    rep("alumno28", 8))) %>% 
  ggplot(aes(concentracion, absorbancia, color= tubos,group=alumno)) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_text(data=tibble(x=60, y=.8),
            aes(x=x,y=y, label="En pricipio esto no debería\nser así, pero bueno..."),
            inherit.aes = F, color="white") +
  geom_line(data=tibble(x=c(50,70), y=c(.7,.7)),
            aes(x=x,y=y), inherit.aes = F, color="white") +
  scale_color_manual(breaks = c("blanco", "tubo1","tubo2","tubo3",
                                "tubo4","tubo5","tubo6","tubo7"),
                     values = c("white", "red", "blue", "forestgreen",
                                "gray", "yellow", "orange", "magenta")) +
  labs(title = "Reacción en 5 horas",
       x="Concentración de ONPG",
       y="Absorbancia") +
  theme(
    plot.title = element_text(size=15, color = "white", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "white"), 
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white", face = "bold"),
    panel.background = element_rect(fill="black", color = "black"),
    plot.background = element_rect(fill="black", color = "black"),
    legend.background = element_rect(fill="black", color = "black"),
    legend.text = element_text(color = "white"),
    legend.key = element_rect(fill = "azure", color="azure"),
    panel.grid = element_blank())


####################
###### 5 Horas #####
####################

brad_5h <- bradford %>% 
  filter(hora == "5 Horas") %>% 
  select(hora, blanco, starts_with("tubo")) %>% 
  pivot_longer(-hora, names_to = "tubos", values_to = "absorbancia") 


onpg_5h <- bradford %>% 
  filter(hora == "5 Horas") %>% 
  select(hora, starts_with("onpg")) %>% 
  pivot_longer(-hora, names_to = "sustrato", values_to = "concentracion") 

cinco_bradford <- cbind(brad_5h, onpg_5h)

# Alumnos de 5 horas
bradford %>% filter(hora=="5 Horas") %>% select(alumno, hora)

cinco_bradford[-1] %>% 
  mutate(alumno = c(rep("alumno13", 8),rep("alumno15", 8),rep("alumno16", 8),
                    rep("alumno17", 8),rep("alumno18", 8),rep("alumno19", 8),
                    rep("alumno25", 8),rep("alumno26", 8), rep("alumno27", 8))) %>% 
  ggplot(aes(concentracion, absorbancia, color= tubos,group=alumno)) +
  geom_line(size=1) +
  geom_point(size=3) +
  geom_text(data=tibble(x=60, y=.8),
            aes(x=x,y=y, label="En pricipio esto no debería\nser así, pero bueno..."),
            inherit.aes = F, color="white") +
  geom_line(data=tibble(x=c(50,70), y=c(.7,.7)),
            aes(x=x,y=y), inherit.aes = F, color="white") +
  scale_color_manual(breaks = c("blanco", "tubo1","tubo2","tubo3",
                                "tubo4","tubo5","tubo6","tubo7"),
                     values = c("white", "red", "blue", "forestgreen",
                                "gray", "yellow", "orange", "magenta")) +
  labs(title = "Reacción en 5 horas",
       x="Concentración de ONPG (μg)",
       y="Absorbancia") +
  theme(
    plot.title = element_text(size=15, color = "white", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "white"), 
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white", face = "bold"),
    panel.background = element_rect(fill="black", color = "black"),
    plot.background = element_rect(fill="black", color = "black"),
    legend.background = element_rect(fill="black", color = "black"),
    legend.text = element_text(color = "white"),
    legend.key = element_rect(fill = "azure", color="azure"),
    panel.grid = element_blank())

#######################################################
# Vamos a intentar extarpolar loresultados anteriores #
# para aproximar las horas del grupo que no se anotó  #
# la hora, que creo que fue el mío xd                 #
#######################################################

# Utilizaremos los datos hasta el tubo 4 ya que los otros 
# dan una cosa un poco rara

brad_1h %>% 
  filter(!(tubos %in% c("tubo5","tubo6","tubo7"))) %>% 
  group_by(tubos) %>% summarise(media=mean(absorbancia), desv_tipica=sd(absorbancia)) %>% 
  mutate(mas=media+desv_tipica,
         menos=media-desv_tipica)

brad_3h %>%
  filter(!(tubos %in% c("tubo5","tubo6","tubo7"))) %>% 
  group_by(tubos) %>% summarise(media=mean(absorbancia), desv_tipica=sd(absorbancia))%>% 
  mutate(mas=media+desv_tipica,
         menos=media-desv_tipica)

brad_5h %>% 
  filter(!(tubos %in% c("tubo5","tubo6","tubo7"))) %>% 
  group_by(tubos) %>% summarise(media=mean(absorbancia), desv_tipica=sd(absorbancia))%>% 
  mutate(mas=media+desv_tipica,
         menos=media-desv_tipica)

# Vamos a intentar una cosita

sin_hora <- bradford[c(1:12),c(1,4:11)] %>% 
  pivot_longer(-alumno, names_to = "tubos", values_to = "absorbancia") %>% view()

# He hecho lo único que se me ha ocurrido xd, no identifico el grupo de tres horas :(
# Era esto o nada.
hora_perdida <- sin_hora %>% 
  filter(tubos == "tubo1") %>% 
  # Valores máximos de las horas de la media más la desviación típica
  mutate(hora=case_when(absorbancia < .145 ~ "1 Hora",
                        absorbancia > .145 & absorbancia < .155 ~ "3 Horas",
                        absorbancia > .155  ~ "5 Horas")) %>%
  arrange(alumno) %>% 
  select(hora)

con_hora <- sin_hora %>% 
  arrange(tubos) %>% 
  mutate(hora=rep(hora_perdida$hora, 8))

###### Vamos a echarle un vistazo esta vez a este grupo.

sin_hora_onpg <- bradford[c(1:12),c(1,12:19)] %>% 
  pivot_longer(-alumno, names_to = "sustrato", values_to = "concentracion") %>%
  mutate(sustrato=case_when(
    sustrato == "onpg_blanco" ~ "1", sustrato == "onpg_1" ~ "2",
    sustrato == "onpg_2" ~ "3", sustrato == "onpg_3" ~ "4",
    sustrato == "onpg_4" ~ "5", sustrato == "onpg_5" ~ "6",
    sustrato == "onpg_6" ~ "7", sustrato == "onpg_7" ~ "8",
  )) %>% arrange(sustrato) %>% select(-alumno) 

grupo1 <- cbind(con_hora, sin_hora_onpg)

grupo1_1h <- grupo1 %>% 
  filter(hora == "1 Hora") %>% 
  arrange(concentracion, tubos)

grupo1_1h %>% 
  ggplot(aes(concentracion, absorbancia, color= tubos,group=alumno)) +
  geom_point(size=3) +
  geom_line(size=1) +
  geom_text(data=tibble(x=60, y=.29),
            aes(x=x,y=y, label="Este no está mal"),
            inherit.aes = F, color="white") +
  scale_color_manual(breaks = c("blanco", "tubo1","tubo2","tubo3",
                                "tubo4","tubo5","tubo6","tubo7"),
                     values = c("white", "red", "blue", "forestgreen",
                                "gray", "yellow", "orange", "magenta")) +
  labs(title = "Reacción en 1 hora",
       x="Concentración de ONPG (μg)",
       y="Absorbancia") +
  theme(
    plot.title = element_text(size=15, color = "white", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "white"), 
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white", face = "bold"),
    panel.background = element_rect(fill="black", color = "black"),
    plot.background = element_rect(fill="black", color = "black"),
    legend.background = element_rect(fill="black", color = "black"),
    legend.text = element_text(color = "white"),
    legend.key = element_rect(fill = "azure", color="azure"),
    panel.grid = element_blank())


####################
###### 5 Horas #####
####################

grupo1_5h <- grupo1 %>% 
  filter(hora == "5 Horas") %>% 
  arrange(concentracion, tubos)

grupo1_5h %>% 
  ggplot(aes(concentracion, absorbancia, color= tubos,group=alumno)) +
  geom_point(size=3) +
  geom_line(size=1) +
  scale_color_manual(breaks = c("blanco", "tubo1","tubo2","tubo3",
                                "tubo4","tubo5","tubo6","tubo7"),
                     values = c("white", "red", "blue", "forestgreen",
                                "gray", "yellow", "orange", "magenta")) +
  labs(title = "Reacción en 5 horas",
       x="Concentración de ONPG (μg)",
       y="Absorbancia") +
  theme(
    plot.title = element_text(size=15, color = "white", 
                              face = "bold", margin = margin(b=30)),
    axis.line = element_line(color = "white"), 
    axis.text = element_text(color = "white"),
    axis.title = element_text(color="white", face = "bold"),
    panel.background = element_rect(fill="black", color = "black"),
    plot.background = element_rect(fill="black", color = "black"),
    legend.background = element_rect(fill="black", color = "black"),
    legend.text = element_text(color = "white"),
    legend.key = element_rect(fill = "azure", color="azure"),
    panel.grid = element_blank())


##########################
# Ahora todos juntos!!!! #
##########################

c <- cinco_bradford[-1] %>% 
  mutate(alumno = c(rep("13", 8),rep("15", 8),rep("16", 8),
                    rep("17", 8),rep("18", 8),rep("19", 8),
                    rep("25", 8),rep("26", 8), rep("27", 8)))

u <- una_bradford[-1] %>% 
  mutate(alumno = c(rep("10", 8),rep("22", 8),rep("24", 8)))

t <- tres_bradford[-1] %>% 
  mutate(alumno = c(rep("14", 8),rep("21", 8),rep("23", 8),
                    rep("28", 8)))

grupo_2_3 <- rbind(u,t,c) %>% 
  mutate(sustrato=case_when(
    sustrato == "onpg_blanco" ~ "1", sustrato == "onpg_1" ~ "2",
    sustrato == "onpg_2" ~ "3", sustrato == "onpg_3" ~ "4",
    sustrato == "onpg_4" ~ "5", sustrato == "onpg_5" ~ "6",
    sustrato == "onpg_6" ~ "7", sustrato == "onpg_7" ~ "8",
  )) %>% 
  select("alumno", "tubos", "absorbancia", 
         "hora", "sustrato", "concentracion")

todos_grupos <- rbind(grupo1, grupo_2_3)

# Ya estamos :), a ello

todos_grupos %>% 
  #filter(hora=="1 Hora") %>% 
  arrange(desc(hora)) %>% 
  ggplot(aes(concentracion, absorbancia, color= hora,group=alumno)) +
  geom_point(size=3) +
  geom_line(size=1) +
  geom_smooth(aes(concentracion, absorbancia, color = hora, group=1),
              color="orange", se=F, size=1.5) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.2)) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(0,80)) +
  scale_color_manual(name="Hora",
                     breaks = c("1 Hora","3 Horas","5 Horas"),
                     values = c("blue", "red", "gray")) +
  labs(
    title = "Recta patrón para todos los grupos",
    x = "Concentración de ONPG (μg)",
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
    panel.grid = element_blank()
  )

# Vamos a ver quien tiene el coeficiente de correlación lineal más alto
# redoble de tambores.... El alumno nº 4 es el que tiene fue el alumno 
# con la supuesta mejor curva. estimación de Pearson = 0.97, p-value < 0.01***
# las variables están correlacionadas.

todos_grupos %>% 
  group_by(alumno) %>% 
  cor_test(concentracion, absorbancia) %>%
  arrange(desc(cor))
  summarise(maximo_correlacion=max(cor),
            minimo_pvalor=min(p)
            ) 

# Y yo? soy el 1: 0.92, not bad tampoco, no entro en el top 5 por un puesto shit xd.

todos_grupos %>% 
  group_by(alumno) %>% 
  filter(alumno == "1") %>% 
  cor_test(concentracion, absorbancia) %>% 
  summarise(maximo_correlacion=max(cor),
            minimo_pvalor=min(p) 
  ) 


# Interpolaresmos con el resultado anterior
# Absorbancia de la proteína, que tengo apuntada:

abs_ul10 <- .310
abs_ul20 <- .4

mejor_alumno <- "4"
yo <- "1"
mejor10_ul <- 26.55
mio10_ul <- 6


resultado_10_mejor <- (mejor10_ul/10)*10
resul_mio10 <- (mio10_ul/10)*10



todos_grupos %>% 
  mutate(
    best_alumno = alumno == mejor_alumno,
    alumno = fct_reorder(factor(alumno), best_alumno)
  ) %>% 
  ggplot(aes(concentracion, absorbancia, color= best_alumno,group=alumno)) +
  geom_line(size=1) +
  geom_hline(yintercept = abs_ul10,size=.75, 
             color="black", linetype="dashed") +
  geom_vline(xintercept = mejor10_ul, size=.75,
             color="red", linetype="dashed") +
  geom_vline(xintercept = mio10_ul,size=.75,
             color="forestgreen", linetype="dashed") +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,1.2)) +
  scale_x_continuous(expand = expansion(0),
                     limits = c(0,80),
                     breaks=seq(0,80,10)) +
  scale_color_manual(name=NULL,
                     breaks = c(T,F),
                     labels=c("Mejor resultado (alumno 4,\nno se quien es xd)", "Resto de alumnos"),
                     values = c("blue", "gray")) +
  labs(
    title = "Interpolación con el mejor resultado\nobtenido en rojo (el mío en verde)",
    x = "Concentración de ONPG (μg)",
    y = "Absorbancia",
    caption=glue("A. Concentración de la proteína del mejor resultado: ({mejor10_ul}/10)*10 = {resultado_10_mejor} μg/μl\nB. Concentración de la proteína de mí muestra: ({mio10_ul}/10)*10  = {resul_mio10} μg/μl")
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

absorbancia <- 0.310
volumen_total_ul <- 850 # ul
volumen_total_ml <- volumen_total_ul*(1/10^6) # l
E <- 4.5 # Mm^-1*cm^-1
conc_prot <- mejor10_ul *1000/1000 # mg/ml
v_extracto_ml <- 0.01 # ml
tiempo <- 25 # min

act_esp(Abs = absorbancia, vT = volumen_total_ml, 
        E = E, conc_prot = conc_prot, ve = v_extracto_ml, t = tiempo)


