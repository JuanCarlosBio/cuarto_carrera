################################################################################
############################      PRÁCTICAS ABBM      ##########################
################################################################################

library(tidyverse)


#------------------------------------------------------------------------------#
#                              Práctica 2                                      #
#------------------------------------------------------------------------------#

# Comparación de la actividad enzimática e x tiempo

library(readxl)
parte_1_jueves <- read_excel("parte_1_jueves.xlsx", 
                             col_types = c("numeric", "text", "text"));View(parte_1_jueves)

parte_1_jueves %>% 
  group_by(tiempo, muestra) %>% 
  summarise(media = mean(parte_1_jueves$valor_conc),
            sd = sd(parte_1_jueves$valor_conc))

parte_1_jueves %>% 
  group_by(muestra) %>% 
  mutate(media = mean(valor_conc), sd = sd(valor_conc)) %>% 
  ggplot(aes(tiempo, media, fill = muestra)) +
  geom_errorbar(aes(ymin = media+sd,
                    ymax = media-sd), width = .3, 
                position = position_dodge(.5)) +
  geom_bar(stat = "identity", position = position_dodge(),
           col = "black", width = .5) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 2),
                     breaks = seq(0,2, .2))

# Curva patrón

df_curva <- data.frame(ug_prot = rep(c(0,2,4,6,8,12), 7),
                       abs = c(0,.131,.291,.376,.465,.654,
                               0,.15,.27,.385,.466,.639,
                               0,.111,.254,.423,.513,.622,
                               0,.151,.295,.298,.498,.670,
                               0,.15,.29,.52,.536,.659,
                               0,.16,.282,.363,.463,.601,
                               0,.16,.277,.362,.441,.625),
                       alumnos = c("1","2","3","4","5","6","7"),
                       tiempo = c("5h","3h",rep("5h",5)));view(df_curva)

alumno_elegido <- filter(df_curva, alumnos %in% "5") # COR Pearson = 0.9929959 --> p < 0.001


alumno_elegido %>% 
  ggplot(aes(abs,ug_prot)) +
  geom_point()
# Abs del estracto del alumno a 20 = 0.310, 40 = 0.482 ul
# Método de interpolación:
alumno_elegido %>% 
  ggplot(aes(ug_prot, abs)) +
  geom_point() +
  geom_line() +
  #geom_abline(intercept = 0.038314, slope = 0.052129, col = "blue") +
  geom_abline(intercept = .310, slope = 0, col = "red", linetype = "dashed") +
  geom_abline(intercept = .482, slope = 0, col = "red", linetype = "dashed") +
  geom_vline(xintercept = 4.69, col = "green", linetype = "dashed") +
  geom_vline(xintercept = 8.4, col = "green", linetype = "dashed") +
  geom_vline(xintercept = 5.211801, col = "blue", linetype = "dashed") +
  geom_vline(xintercept = 8.511308, col = "blue", linetype = "dashed") +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,.1)) +
  scale_x_continuous(limits = c(0,12.5),
                     breaks = seq(0,12,.5)) +
  theme_classic() +
  labs(title = "Recta patrón, determinación de la masa de proteína",
       subtitle = "Prácticas de ABBM, 4º Biología",
       y = "ABSORMANCIA",
       x = "ug DE PROTEÍNA") +
  theme(title = element_text(face = "bold", size = 13)) +
  geom_text(data = tibble(x = 1, y = .9),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Resultados a ojo:") +
  geom_text(data = tibble(x = 1.5, y = .8),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Muestra1 (10ul) = 4.69 ug", col = "forestgreen") +
  geom_text(data = tibble(x = 1.5, y = .85),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Muestra2 (20ul) = 8.4 ug", col = "forestgreen") +
  geom_text(data = tibble(x = 1.5, y = .75),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Resultados Recta de regresión:") +
  geom_text(data = tibble(x = 1.5, y = .7),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Muestra1 (20ul) = 8.51 ug", col = "blue") +
  geom_text(data = tibble(x = 1.5, y = .65),
            aes(x=x, y=y), inherit.aes = FALSE,
            label = "Muestra2 (10ul) = 5.21 ug", col = "blue") 

# método de regresión lineal
modelo_5 <- lm(abs~ug_prot, data = alumno_elegido);summary(modelo_5) 

# Fórmula de la recta: ABS = ug_prot·0.052129 + 0.038314
# ug_prot = (ABS-0.038314)/0.052129
ug_prot_10ul <- (0.310-0.038314)/0.052129;ug_prot_10ul
ug_prot_20ul <- (0.482-0.038314)/0.052129;ug_prot_20ul

# Cálculo de la actividad específica (nmol de NP * min^-1 * ug ^-1)
# NP --> A420 --> A = E(4.5 nM^-1*cm^-1)*b(1 cm)*c --> c = A/(E*b)(nM)

c = 0.310/(4.5*1);c # 0.069 nM

# nM*vt (litros) = nmol/t(tiempo de incubación = 25 min)

Act_esp = 
  
  
  
  #------------------------------------------------------------------------------#
  #                  Estudio de los resultdos de la pizarra                      #
  #------------------------------------------------------------------------------#
  
  
  
  df_curva %>% 
  ggplot(aes(ug_prot, abs, col = alumnos)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Recta Patrón, práctica 2",
       subtitle = "Avances en bioquímica y biología molecular",
       X = "",
       y = "ABSORBANCIA") +
  scale_x_continuous(limits = c(0,13),
                     breaks = seq(0,12, 1)) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,.1)) +
  theme_bw() 

#                    Coeficientes de correlación:                              #
# Alumno 1

alumno_1 <- filter(df_curva, alumnos %in% "1")
alumno_2 <- filter(df_curva, alumnos %in% "2")
alumno_3 <- filter(df_curva, alumnos %in% "3")
alumno_4 <- filter(df_curva, alumnos %in% "4")
alumno_5 <- filter(df_curva, alumnos %in% "5")
alumno_6 <- filter(df_curva, alumnos %in% "6")
alumno_7 <- filter(df_curva, alumnos %in% "8")

COR1 <- cor.test(alumno_1$ug_prot, alumno_1$abs, method = "pearson");COR1
COR2 <- cor.test(alumno_2$ug_prot, alumno_2$abs, method = "pearson");COR2
COR3 <- cor.test(alumno_3$ug_prot, alumno_3$abs, method = "pearson");COR3
COR4 <- cor.test(alumno_4$ug_prot, alumno_4$abs, method = "pearson");COR4
COR5 <- cor.test(alumno_5$ug_prot, alumno_5$abs, method = "pearson");COR5
COR6 <- cor.test(alumno_6$ug_prot, alumno_6$abs, method = "pearson");COR6
COR7 <- cor.test(alumno_7$ug_prot, alumno_7$abs, method = "pearson");COR7


#------------------------------------------------------------------------------#
#                                Practica 6                                    #
#------------------------------------------------------------------------------#


library(readxl)
practica_6 <- read_excel("practica_6.xls", 
                         col_types = c("numeric", "numeric", "text"));View(practica_6)


practica_6 %>% 
  ggplot(aes(conc_sust, abs, col = alumno)) +
  geom_line() +
  geom_point()+
  labs(title = "Michales-Menten, práctica 6",
       subtitle = "Avances en bioquímica y biología molecular",
       X = "Concentración de Sustrato",
       y = "ABSORBANCIA") +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100, 10)) +
  scale_y_continuous(limits = c(0,1.5),
                     breaks = seq(0,1,.1)) +
  theme_bw() 
###############
# correlación #
###############

alumno_1_  <- filter(practica_6, alumno %in% "1")
alumno_2_  <- filter(practica_6, alumno %in% "2")
alumno_3_  <- filter(practica_6, alumno %in% "3")
alumno_4_  <- filter(practica_6, alumno %in% "4")
alumno_5_  <- filter(practica_6, alumno %in% "5")
alumno_6_  <- filter(practica_6, alumno %in% "6")
alumno_7_  <- filter(practica_6, alumno %in% "7")
alumno_8_  <- filter(practica_6, alumno %in% "8")
alumno_9_  <- filter(practica_6, alumno %in% "9")
alumno_10_ <- filter(practica_6, alumno %in% "10")
alumno_11_ <- filter(practica_6, alumno %in% "11")
alumno_12_ <- filter(practica_6, alumno %in% "12")


COR1 <- cor.test(alumno_1_$conc_sust, alumno_1_$abs, method = "pearson");COR1
COR2 <- cor.test(alumno_2_$conc_sust, alumno_2_$abs, method = "pearson");COR2
COR3 <- cor.test(alumno_3_$conc_sust, alumno_3_$abs, method = "pearson");COR3
COR4 <- cor.test(alumno_4_$conc_sust, alumno_4_$abs, method = "pearson");COR4
COR5 <- cor.test(alumno_5_$conc_sust, alumno_5_$abs, method = "pearson");COR5
COR6 <- cor.test(alumno_6_$conc_sust, alumno_6_$abs, method = "pearson");COR6
COR7 <- cor.test(alumno_7_$conc_sust, alumno_7_$abs, method = "pearson");COR7
COR8 <- cor.test(alumno_8_$conc_sust, alumno_8_$abs, method = "pearson");COR1
COR9 <- cor.test(alumno_9_$conc_sust, alumno_9_$abs, method = "pearson");COR2
COR10 <- cor.test(alumno_10_$conc_sust, alumno_10_$abs, method = "pearson");COR3
COR11 <- cor.test(alumno_11_$conc_sust, alumno_11_$abs, method = "pearson");COR4
COR12 <- cor.test(alumno_12_$conc_sust, alumno_12_$abs, method = "pearson");COR5


mi_grafica <- filter(practica_6, alumno %in% "1")

# La gráfica no está del todo bien, necesitas la actividad biológica
mi_grafica %>% 
  ggplot(aes(conc_sust, abs)) +
  geom_line(size = 1) +
  geom_point(pch = 21, fill = "white", size = 2)+
  geom_hline(yintercept = .933, col = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.4665, col = "orange", linetype = "dashed") +
  geom_vline(xintercept = 10.7, col = "green", linetype = "dashed") +
  labs(title = "Michales-Menten, práctica 6",
       subtitle = "Avances en bioquímica y biología molecular",
       X = "Concentración de Sustrato",
       y = "ABSORBANCIA") +
  scale_x_continuous(limits = c(0,100),
                     breaks = seq(0,100, 10)) +
  scale_y_continuous(limits = c(0,1.5),
                     breaks = seq(0,1,.1)) +
  theme(panel.background = element_blank(),
        axis.line = element_line()) +
  geom_text(data = tibble(x = 90, y = 0.955),
            aes(x = x, y = y), inherit.aes = F, 
            label = "Vmáx", col = "red") +
  geom_text(data = tibble(x = 90, y = 0.49),
            aes(x = x, y = y), inherit.aes = F, 
            label = "1/2Vmáx", col = "orange")

#------------------------------------------------------------------------------#
#                          Cálculo de la km                                    #
#------------------------------------------------------------------------------#

carita_feliz <- data.frame(S = c(.14,.22,.29,.56,.77,1.46),
                           Vi = c(.15,.17,.23,.32,.39,.49)) %>% 
  mutate(inv_S = 1/S) %>% 
  mutate(inv_vi = 1/Vi);view(carita_feliz)

cor.test(carita_feliz$inv_S, carita_feliz$inv_vi)

# Interpolación simple

carita_feliz %>% 
  ggplot(aes(S,Vi)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .49, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 0.245, linetype = "dashed", col = "green") +
  geom_vline(xintercept = .325,linetype = "dashed", col = "green") # km = 3.25

# interpolación invertida

modelo <- lm(inv_vi~inv_S, data = carita_feliz);summary(modelo)

carita_feliz %>% 
  ggplot(aes(inv_S,inv_vi)) +
  geom_point() +
  geom_abline(intercept = 1.73576, slope = 0.75173, col = "red") +
  geom_vline(xintercept = -2.78, col = "green", linetype = "dashed") +
  geom_hline(yintercept = 1.75, col = "blue", linetype = "dashed") +
  scale_y_continuous(limits = c(0,8),
                     breaks = seq(0,8,1)) +
  scale_x_continuous(limits = c(-3,10),
                     breaks = seq(-3,10,1)) +
  geom_vline(xintercept = 0) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        title = element_text(face = "bold", size = 18)) +
  labs(title = "Examen de ABBM",
       y ="1/vi",
       x = "1/[S]")



#------------------------------------------------------------------------------#
#                 Cálculo del peso molecular de una proteína                   #
#------------------------------------------------------------------------------#

# Función del rf
fun_rf <- function(drf,frente) {
  operacion = drf/frente
  return(operacion)
}


# Valor de drf
drf_PM <- c(1.50,1.70,2.15,2.55,
            3.35,4.00,4.90,5.40)
rf_PM <- fun_rf(drf_PM, frente)
# Valor del frente
frente <- 5.78
# Valor del rf proteína
rf_prot <- 2.9/frente
rf_prot
# log(PM)
pesom <- log10(c(250,150,100,75,50,37,25,20));pesom

# Tabla de los datos
datos <- data.frame(drf=drf,
                    rf = rf_PM,
                    frente = c(5.78,rep(NA, 7)),
                    prot = c(rf_prot, rep(NA, 7)));view(datos)


df_rf <- data.frame(log_peso= pesom,
                    rf = rf_PM);df_rf
# Regresión lineal
mod_rf <- lm(log_peso~rf, data = df_rf);summary(mod_rf)
# Gráfico
df_rf %>% 
  ggplot(aes(rf,log_peso)) +
  geom_point() +
  geom_abline(intercept = 2.6124, slope = -1.4668) +
  geom_vline(xintercept = rf_prot, linetype = "dashed", col = "red")+
  scale_y_continuous(breaks = seq(0,3, .15)) +
  geom_hline(yintercept = 1.875,col = "green", linetype = "dashed") +
  geom_hline(yintercept = 1.875,col = "green", linetype = "dashed") +
  theme_classic() +
  labs(title = "Cálculo del PM Proteína heteróloga",
       x = "Rf",
       y = "Log(PM)") +
  geom_text(data = tibble(x = .75, y = 1.8),
            aes(x = x, y=y), label = "PM = 10^1.875 = 74.989 kd
r = -0.98 ", col = "forestgreen")

#####################
# Resultado         #
10^1.875   # 74.989  #
#####################

cor.test(df_rf$rf, df_rf$log_peso)

-1.4668* + 2.6124

#------------------------------------------------------------------------------#
#                             Cálculo de la km                                 #
#------------------------------------------------------------------------------#

ONPG_uM <- c(0,.11,.22,.43,.87,1.1,1.3,1.5)*10^3;ONPG_uM

ul <- 600

abs <- c(0,.270,.454,.644,.697,.825,.845,.933)

umol <- function(uM,ul) {
  micro_moles = uM*ul
  return(micro_moles)
}

u_moles <- umol(ONPG_uM,ul);u_moles


mod <- lm(abs~ug_prot, data = alumno_elegido);summary(mod)

ug_ul_prot <- function(abs,a,b) {
  operacion = ((abs-a)/b)*.01
  return(operacion)
}

conc_prot <- ug_ul_prot(abs, 0.038314,0.052129);conc_prot

#conc_prot <- c(0.115998005,0.109283892,0.094896507,
#               0.094321011,0.031016517,0.029481862,.001,0)

|-1|
  
  a_e <- function(abs,E,vt,conc_prot,v_ext,min) {
    operacion = (abs*(vt*10^-6))/(E*conc_prot*v_ext*min)
    return(operacion)
  }

act_esp <- a_e(abs,4.5,ul,conc_prot,0.01,25)




mic_menten <- data.frame(a_e = act_esp,
                         micro_mol = c(900000,780000,780000,660000,522000,258000,132000,66000,0));mic_menten
                         
u_moles
mic_menten %>% 
  ggplot(aes(micro_mol, act_esp)) +
  geom_line() +
  scale_y_continuous(limits = c(0,.005)) +
  geom_hline(yintercept = 0.00288) +
  geom_hline(yintercept = 0.00144) +
  geom_vline(xintercept = 29000) 
                         
                         
                         
                         
                         
alumno_elegido %>% 
  ggplot(aes(ug_prot, abs)) +
  labs(x = "[S]?",
       y = "Vo?") +
  theme_classic() +
  theme(title = element_text(size = 18, face = "bold"))
                         
                         
tabla1 <- data.frame(cinc=c(0.05,0.10,.15,.20,.30),
                     v=c(30,43,52,57,58)) %>%
  mutate(invcinc = 1/cinc) %>% 
  mutate(invv = 1/v);tabla1
                         
attach(tabla1)
plot(cinc,v, type = "line")
                         
plot(invcinc,invv)
                         
cor.test(invv,invcinc)
l <- lm(invv~invcinc, data = tabla1)
summary(l)
                         
tabla1 %>% 
  ggplot(aes(cinc, v)) +
  geom_line() +
  geom_hline(yintercept = 38.46) +
  geom_vline(xintercept = 0.0825)
                         
tabla2 <- data.frame(v = c(30,43,52,57,60),
                           c = c(0.1,0.15,.23,.35,.4))

library(tidyverse)
tabla2 %>% 
  ggplot(aes(c,v)) +
  geom_line() +
  geom_hline(yintercept =45.045) +
  geom_vline(xintercept = .17) +
  geom_text(data = tibble(x = .25, y = 40),
            aes(x = x, y = y), inherit.aes = F, label = "km = 0.17")
                         
                         
alumno_elegido %>% 
  ggplot(aes(ug_prot, abs)) +
  geom_line() +
  geom_hline(yintercept = 0.34) +
  geom_vline(xintercept = 5.4) +
  geom_text(data = tibble(x = 10, y = .2),
  aes(x = x, y = y), label = "ug_prot = 1.5 ug")
                         
data <- data.frame(x = 0,
                   y = 0)
                         
data %>% 
  ggplot(aes(x , y)) +
  geom_point() +
  geom_abline(intercept = 1.2, solve=0.06) +
  scale_y_continuous(limits = c(0,10))
                         
                         
