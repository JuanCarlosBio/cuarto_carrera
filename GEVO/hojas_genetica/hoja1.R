#### funciones de las hojas

source()

# Librerías
library(tidyverse)

# Problemas de la hoja 1

# (1) Analizando la G6pdh en humanos se encontraron 15 machos y 15 hembras de fenotipo A, 25 machos y
# 50 hembras de fenotipo B, 10 machos y 22 hembras de genotipo C, 88 hembras AB, 25 hembras AC y
# 100 hembras BC. Estime las frecuencias genotípicas y alélicas para las tres muestras: machos, hembras
# y total.
# (Recuerde que la G6pdh en humanos está ligada al sexo). Presente la tabla de frecuencias alélicas,
# Heterocigosidad observada (Ho) y esperada (He)

condicion <- function(vect,g1,g2,g3,g4,g5,g6){
    cuerpo=case_when(vect == g1 ~ 2,
                     vect == g2 ~ 1,
                     vect == g3 ~ 1,
                     vect == g4 | vect == g5 | vect == g6 ~ 0)
    return(cuerpo)
}

# Tabla y calculos:
tabla <- tibble(fenotipos=c("A","B","C","AB","AC","BC"),
       macho=c(15,25,10,NA,NA,NA),
       hembra=c(15,50,22,88,25,100)) 
       
tabla %>%
    mutate(sum_machos=sum(macho, na.rm=TRUE),
           sum_hembras=sum(hembra),
           total=sum(macho,na.rm=TRUE)+sum(hembra),
           frec_al_g_macho=round(macho/sum_machos,2),
           frec_g_hembra=round(hembra/sum_hembras,2),
           alelo_A=condicion(fenotipos,"A","AB","AC","B","C","BC"),
           alelo_B=condicion(fenotipos,"B","AB","BC","A","C","AC"),
           alelo_C=condicion(fenotipos,"C","AC","BC","A","B","AB"),
           frec_A_hembra=round((alelo_A*hembra)/(2*sum_hembras),2),
           frec_B_hembra=round((alelo_B*hembra)/(2*sum_hembras),2),
           frec_C_hembra=round((alelo_C*hembra)/(2*sum_hembras),2),
           sum_fA=sum(tabla$frec_A_hembra),
           sum_fB=sum(tabla$frec_B_hembra),
           sum_fC=sum(tabla$frec_C_hembra),
           frec_A_tot=round((2*15+88+25+15)/(2*300+50),2),
           frec_B_tot=round((2*50+88+100+25)/(2*300+50),2),
           frec_C_tot=round((2*22+25+100+10)/(2*300+50),2)
           ) %>% view
#select(frec_al_g_macho, sum_fA,sum_fB,sum_fC,frec_A_tot,frec_B_tot,frec_C_tot) %>% view()

# Heterocigosidad observada y esperada
fA <- mean(tabla$frec_A_tot)
fB <- mean(tabla$frec_B_tot)
fC <- mean(tabla$frec_C_tot)

ho <- (tabla[4,3]+tabla[5,3]+tabla[6,3])/mean(tabla$sum_hembras)
he <- 1- (fA^2+fB^2+fC^2)

# 2) Dos poblaciones presentan las siguientes frecuencias genotípicas:

# Población I 0'24 AA 0'32 Aa 0'44 aa
# Población II 0'33 AA 0'14 Aa 0'53 aa
# ¿Estima las frecuencias alélicas que presenta cada población?


tabla2 <- tibble(AA=c(.24,.33),
                 Aa=c(.32,.14),
                 aa=c(.44,.53)) 

pob1_A <- tabla2[1,1]+(1/2*tabla2[1,2]) #=0.4
pob1_a <- tabla2[1,3]+(1/2*tabla2[1,2]) #=0.6

pob2_A <- tabla2[2,1]+(1/2*tabla2[2,2]) #=0.4
pob2_a <- tabla2[2,3]+(1/2*tabla2[2,2]) #=0.6

# 3.- Si las muestras de las poblaciones I y II fueron de 100 y 200 individuos, respectivamente, indique cuántos
# individuos y cuántos alelos de cada tipo se analizaron en cada población.

tabla3 <- tibble(frec_pob1=c(.24,.32,.44),
                 frec_pob2=c(.33,.14,.53),
                 genotipos=c("AA","Aa","aa"))

total_pob1 <- 100
total_pob2 <- 200

tabla3 %>%
    mutate(individuos1=frec_pob1*total_pob1,
           individuos2=frec_pob2*total_pob2)

# 4.- Se analizó una población humana en el norte de África para 5 STRs. En la tabla siguiente se muestran las
# frecuencias alélicas y las heterocigosidades observadas para por locus encontradas en la población. Se
# pide estimar los parámetros heterocigosidad por locus, heterocigosidades observadas y promedio para el
# conjunto de loci, porcentaje de loci polimórficos al 95% y número de alelos promedio por locus de la
# población:

tabla4 <- t(read.table("C:\\Users\\jcge9\\Desktop\\paper_ph\\ejercicio4.txt", header = TRUE))

colnames(tabla4) <- tabla4[1,]
tabla4 <- tabla4[-c(1,2,11),]

N <- 56
ho <- as.numeric(tabla4[8,])
media_ho <- round(mean(ho),2)
sd_ho <- round(sd(ho),2) 

tabla4 <- as_tibble(tabla4[-8,]) %>%
    mutate(D13S317=as.numeric(D13S317),
           D7S820=as.numeric(D7S820),
           D2S1338=as.numeric(D2S1338),
           D21S11=as.numeric(D21S11),
           D16S539=as.numeric(D16S539),
           grupo=rep("grupo",7)) %>%
    pivot_longer(-grupo, names_to = "locus", values_to ="f_alelo")

tabla4 %>%
    group_by(locus) %>%
    summarise(he=round(1-sum(f_alelo^2, na.rm=TRUE),2)) %>%
    ungroup() %>%
    summarise(he_media=mean(he),
              he_error=sd(he)/sqrt(N))

# El % de Loci Polimórficos al 95% es el 100%
tabla4 %>%
    filter(f_alelo >= 0.95)

# Ejercicio 5



sust <- function(col,base){
    cuerpo=str_replace(col, pattern = "[:punct:]", base)
    return(cuerpo)
}

tabla5 <- read.table("C:\\Users\\jcge9\\Desktop\\paper_ph\\ejercicio5.txt", header = TRUE) %>%
    mutate(
        X1=sust(X1,"A"), X2=sust(X2,"T"), X3=sust(X3,"T"), X4=sust(X4,"T"), X5=sust(X5,"C"),
        X6=sust(X6,"T"), X7=sust(X7,"A"), X8=sust(X8,"G"), X9=sust(X9,"T"), X10=sust(X10,"A"),
        X11=sust(X11,"T"), X12=sust(X12,"T"), X13=sust(X13,"T"), X14=sust(X14,"G"), X15=sust(X15,"C"),
        X16=sust(X16,"A"), X17=sust(X17,"T"), X18=sust(X18,"T"), X19=sust(X19,"C"), X20=sust(X20,"A"),
        X21=sust(X21,"A"), X22=sust(X22,"T"), X23=sust(X23,"A"), X24=sust(X24,"T"), X25=sust(X25,"G") 
)

# Cuerpo necesario
dist(tabla5)

comparar <- function(col1,col2){
    attach(tabla5, warn.conflicts = FALSE)
    x = col1 == col2
    y = sum(x == FALSE)
    return(y) 
}

# Bastante coñazo, no se ni como harías esto a mano, parece mucho pero se puede harcore coding (copiar y pegar), ojala encontrar una librería que te haga el trabajo
dist_matrix <- data.frame(
    sec1=c(comparar(X1,X2),comparar(X1,X3),comparar(X1,X4),comparar(X1,X5),comparar(X1,X6),comparar(X1,X7),comparar(X1,X8),comparar(X1,X9), comparar(X1,X10),comparar(X1,X11),comparar(X1,X12),comparar(X1,X13),comparar(X1,X14),comparar(X1,X15)),
    sec2=c(comparar(X2,X3),comparar(X2,X4),comparar(X2,X5),comparar(X2,X6),comparar(X2,X7),comparar(X2,X8),comparar(X2,X9),comparar(X2,X10),comparar(X2,X11),comparar(X2,X12),comparar(X2,X13),comparar(X2,X14),comparar(X2,X15),NA),
    sec3=c(comparar(X3,X4),comparar(X3,X5),comparar(X3,X6),comparar(X3,X7),comparar(X3,X8),comparar(X3,X9), comparar(X3,X10),comparar(X3,X11),comparar(X3,X12),comparar(X3,X13),comparar(X3,X14),comparar(X3,X15),rep(NA,2)),
    sec4=c(comparar(X4,X5),comparar(X4,X6),comparar(X4,X7),comparar(X4,X8),comparar(X4,X9), comparar(X4,X10),comparar(X4,X11),comparar(X4,X12),comparar(X4,X13),comparar(X4,X14),comparar(X4,X15),rep(NA,3)),
    sec5=c(comparar(X5,X6),comparar(X5,X7),comparar(X5,X8),comparar(X5,X9), comparar(X5,X10),comparar(X5,X11),comparar(X5,X12),comparar(X5,X13),comparar(X5,X14),comparar(X5,X15),rep(NA,4)),
    sec6=c(comparar(X6,X7),comparar(X6,X8),comparar(X6,X9), comparar(X6,X10),comparar(X6,X11),comparar(X6,X12),comparar(X6,X13),comparar(X6,X14),comparar(X6,X15),rep(NA,5)),
    sec7=c(comparar(X7,X8),comparar(X7,X9), comparar(X7,X10),comparar(X7,X11),comparar(X7,X12),comparar(X7,X13),comparar(X6,X14),comparar(X7,X15),rep(NA,6)),
    sec8=c(comparar(X8,X9), comparar(X8,X10),comparar(X8,X11),comparar(X8,X12),comparar(X8,X13),comparar(X8,X14),comparar(X8,X15),rep(NA,7)),
    sec9=c(comparar(X9,X10),comparar(X9,X11),comparar(X9,X12),comparar(X9,X13),comparar(X9,X14),comparar(X9,X15),rep(NA,8)),
    sec10=c(comparar(X10,X11),comparar(X10,X12),comparar(X10,X13),comparar(X10,X14),comparar(X10,X15),rep(NA,9)),
    sec11=c(comparar(X11,X12),comparar(X11,X13),comparar(X11,X14),comparar(X11,X15),rep(NA,10)),
    sec12=c(comparar(X12,X13),comparar(X12,X14),comparar(X12,X15),rep(NA,11)),
    sec13=c(comparar(X13,X14),comparar(X13,X15),rep(NA,12)),
    sec14=c(comparar(X14,X15),rep(NA,13))
)

rownames(dist_matrix) <- c("sec2","sec3","sec4","sec5","sec6","sec7","sec8","sec9",
                           "sec10","sec11","sec12","sec13","sec14","sec15")

diffij <- sum(dist_matrix, na.rm=TRUE)
L <- 600
n <- 15

round(num_ndiff(diffij,L,n),5) # No entiendo de donde viene ese 0 de menos en la varianza tengo que repasarlo
