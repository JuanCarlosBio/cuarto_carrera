################################################################################
############################ Tenebrios Todos los grupos ########################
################################################################################

library(readxl)                                                     
library(tidyverse)
library(DescTools)
library(rstatix)
library(nortest)
library(tidytext)

url <- "https://raw.githubusercontent.com/Juankkar/tenebrios_faa/main/bases_datos/tene_todos_crudos.csv"
tene_todos_crudos <- read.csv(url,sep = ";") # madre de dios la limpieza lloro ); sea todo por la ciencia
ten_tod2 <- tene_todos_crudos[1:42,]

ten_tod3 <- ten_tod2[-c(15,30), -seq(0,35,6)] 

separar <- function(df,x,y) {
  cuerpo <- df[,c(x:y)]
  return(cuerpo)
}

nombres_grupos <- c("grupo","semana","masa_corp","d","dx")

uno <- separar(ten_tod3,1,5)
colnames(uno) <- nombres_grupos
dos <- separar(ten_tod3,6,10)
colnames(dos) <- nombres_grupos
tre <- separar(ten_tod3,11,15)
colnames(tre) <- nombres_grupos
cua <- separar(ten_tod3,16,20)
colnames(cua) <- nombres_grupos
cin <- separar(ten_tod3,21,25)
colnames(cin) <- nombres_grupos
sei <- separar(ten_tod3,25,30)
sei <- sei[,-1]
colnames(sei) <- nombres_grupos



df_tenebrios <- bind_rows(uno,dos, tre, cua, cin, sei) %>% 
  mutate(masa_corp=str_replace(masa_corp, pattern = ",", replacement = "."),
         masa_corp=as.numeric(masa_corp),
         d=str_replace(d, pattern = ",", replacement = "."),
         d=as.numeric(d),
         dx=str_replace(dx, pattern = ",", replacement = "."),
         dx=as.numeric(dx),
         semana=as.numeric(semana)) %>%
  filter(!(semana %in% NA)) %>% 
  mutate(experimento=case_when(grepl("C",grupo)~"Control",
                               grepl("O",grupo)~"Oscuridad",
                               grepl("T",grupo)~"Temperatura")) 

#fila <- seq(0,216,12) 
df_tenebrios[
  # fila, # Por qué no? );
  c(24,48,60,72,84,96,108,120,132,144,192,204,216)
             ,c(4,5)] <- NA # Por alguna extraña razón algunas personas decidieron poner el promedio de d y dx al final
df_tenebrios[12,3] <- 0.1319 # Y este valor es el único desaparecido, pero nada que no pueda solucionarse con fuerza bruta

df_tenebrios %>% 
  group_by(semana,experimento) %>% 
  summarise(media=mean(masa_corp, na.rm=T),
            sd=sd(masa_corp, na.rm=T)) %>% 
  ggplot(aes(as.numeric(semana),media, col=experimento)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=media+sd,ymax=media-sd),width=.3,size=.65) +  
  geom_point(size=2) +
  scale_color_manual(values = c("blue","black","red")) +
  labs(title = "Variación masa corporal",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semana",
       y="Masa Corporal (g)",
       col="Variación") +
  geom_text(data = tibble(x=4, y =0.09),
            aes(x=x,y=y, label="*"), size = 8, inherit.aes = F) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(1,12,1)) +
  scale_y_continuous(limits = c(0,.18),
                     breaks = seq(0,.18,.03)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = c(.25,.8),
    legend.background = element_rect(color = "black"),
    legend.key = element_rect(fill="white"),
    legend.title = element_text(hjust = .5, size=13, face = "bold"),
    legend.text = element_text(size = 12)
  )

# Existen diferencias significativas en el aumento de la masa corpora según las semanas 
# La ide sería ser capaz de hacerlo sin moirir en el intento :(

modelo_inferencia <- function(df,sem) {
  oscuridad = subset(df, experimento == "Oscuridad" & semana == sem & masa_corp != "NA")
  control = subset(df, experimento == "Control" & semana == sem & masa_corp != "NA")
  temperatura = subset(df, experimento == "Temperatura" & semana == sem & masa_corp != "NA")
  normalidad_osc=shapiro.test(as.numeric(oscuridad$masa_corp))
  normalidad_con=shapiro.test(as.numeric(control$masa_corp))
  normalidad_tem=shapiro.test(as.numeric(temperatura$masa_corp))
  vector=c(normalidad_con$p.value,normalidad_osc$p.value,normalidad_tem$p.value)
  df=data.frame(
    masa_corp=c(control$masa_corp,oscuridad$masa_corp,temperatura$masa_corp),
    experimento=c(control$experimento,oscuridad$experimento,temperatura$experimento)
  )
  levene = LeveneTest(as.numeric(masa_corp)~experimento, data = df, center = "mean")
  p_valor_levene = data.frame(p_valor=levene$`Pr(>F)`)[1,]
  condicion= all(vector > 0.05)
  if(p_valor_levene > 0.05 & condicion == TRUE) {
    print("Anova de una vía")
    print(summary(aov(masa_corp~experimento, data = df)))
  } else if(p_valor_levene < 0.05 & condicion == TRUE){
    print("Anova de Welch")
    print(welch_anova_test(masa_corp~experimento, data = df))
  } else{
    print("Kruskal-Wallis")
    print(kruskal.test(masa_corp~experimento, data = df))
  }
  return(condicion)
} # No es perfecta pero me sirve para practicar,
  # la mejoraré en el futuro

modelo_inferencia(df_tenebrios, 1) # p > 0.05
modelo_inferencia(df_tenebrios, 2) # p > 0.05
modelo_inferencia(df_tenebrios, 3) # p > 0.05
modelo_inferencia(df_tenebrios, 4) # p < 0.05*

semana4 <- df_tenebrios %>% filter(semana %in% 4) %>% mutate(masa_corp=masa_corp)
TukeyHSD(aov(masa_corp~experimento, data = semana4)) # p < 0.05* temperatura alta-control solamente

modelo_inferencia(df_tenebrios, 5) # p > 0.05 
modelo_inferencia(df_tenebrios, 6) # p > 0.05
modelo_inferencia(df_tenebrios, 7) # p > 0.05
modelo_inferencia(df_tenebrios, 8) # p > 0.05
modelo_inferencia(df_tenebrios, 9)

modelo_inferencia(tenebrios_todos, 10) # A partir de aqui es complicado sacar la normalidad ya 
# que disminuyen los valores de las muestras significativamenete


df_tenebrios %>% 
  group_by(semana,experimento) %>% 
  summarise(media=mean(d, na.rm=T),
            sd=sd(d, na.rm=T)) %>% 
  ggplot(aes(as.numeric(semana),media, col=experimento)) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=media+sd,ymax=media-sd),width=.3,size=.65) +  
  geom_point(size=2) +
  scale_color_manual(values = c("blue","black","red")) +
  labs(title = "Variación tasa de crecimiento",
       subtitle = "Fisiología Animal Aplicada ULL, Grupo 4 Oscuridad",
       x="Semana",
       y="Tasa de crecimiento (g)",
       col="Variación") +
   scale_x_continuous(limits = c(0,12),
                      breaks = seq(1,12,1)) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = c(.25,.2),
    legend.background = element_rect(color = "black"),
    legend.key = element_rect(fill="white"),
    legend.title = element_text(hjust = .5, size=13, face = "bold"),
    legend.text = element_text(size = 12)
  )


# ¿existen diferencias significativas? hay que sustituir en la función masa_corp por tasa_crec_s, no he logrado que la 
# función sea del todo reproducible, el test de shapiro me da problesmas pero tiene buena pinta para ir 
# mejorandola.

modelo_inferencia(df_tenebrios, 1) # p > 0.05
modelo_inferencia(df_tenebrios, 2) # p > 0.05
modelo_inferencia(df_tenebrios, 3) # p > 0.05
modelo_inferencia(df_tenebrios, 4) # p > 0.05
modelo_inferencia(df_tenebrios, 5) # p > 0.05 
modelo_inferencia(df_tenebrios, 6) # p > 0.05
modelo_inferencia(df_tenebrios, 7) # p > 0.05
modelo_inferencia(df_tenebrios, 8) # p > 0.05
modelo_inferencia(df_tenebrios, 9) # A partir de aqui es complicado sacar la normalidad ya 
# que disminuyen los valores de las muestras significativamenete.
 