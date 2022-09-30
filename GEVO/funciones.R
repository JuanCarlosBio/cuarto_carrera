##### En este script están algunas funciones necesarias, que facilitan mucho a la hora de hacer 
# el script de las practicas_genetica.R, y volverlo más automático.

# Función para calcular la Heterocigocidad esperada:

he <- function(frec1, frec2) {
  operacion = 1-(frec1^2+frec2^2)
  return(operacion)
}

# Función para extraer la información de la tabla como vectores. De esta manera, 
# debería funcionar de manera automática incluso con tus datos, siempre y cuando
# el nº total de genotipos sean 3, ejemplo AA Aa aa.
vect_hw <- function(df,      # tabla de hardy weinberg que hemos creado
                    linea,   # línea de la tabla de hardy weinberg, en mi caso, 1 = clase, 2 = canarias
                    col1,    # columna 1 homocigoto "dominante"
                    col2,    # columna 2 heterocigoto
                    col3){   # columna 3 homocigoto "recesivo"
  cuerpo=c(MM=as.numeric(df[linea,col1]),
           MN=as.numeric(df[linea,col2]),  # M, N... da igual, como si es A a, sirve cualquiera
           NN=as.numeric(df[linea,col3]))
  return(cuerpo)
}

# Función para contar los alelos de los genotipos; es para resumir el código básicamente
# Se trata de un condicional, en caso de que se cumpla la función de que el alelo que estamos
# estudiando es el "1" (puede que se llame de otra manera), nos da el primer output, si el el "2"
# nos dará la opción del else if.
contar_al <- function(locus, alelo){
  condicion=if(alelo == "1"){
    case_when(locus == "1/1" ~ 2,
              locus == "1/2" ~ 1,
              locus == "2/2" ~ 0)
  } else if(alelo == "2"){
     case_when(locus == "1/1" ~ 0,
               locus == "1/2" ~ 1,
               locus == "2/2" ~ 2)
  }
  return(condicion)
}

# Función creada por mí, necesarias para desequilibrio gamético en el método de langley.
# Obtenemos a partir de una única función: v; chi cuadrado; R
# Los parámetros necesarios serían:
# A,B,C,D: los sacas cuando haces la tabla comparativa de genotipos
# N: nº de individuos
# p1, p2, q1, q2: frecuencias alélicas 
langley <- function(A,B,C,D,N,p1,p2,q1,q2){
  print(">>> Resultados de desequilibrio gametico de Langley:")
  v=((4*A+2*B+2*C+D)/(2*N)) - (2*p1*p2)
  chi_cuadrado=(N*v^2)/(p1*p2*q1*q2)
  R=sqrt(chi_cuadrado/(4*N))
  resultado=c("v_result:"=v,
              "chi_cuadrado_result"=chi_cuadrado,
              "R_result"=R)
  return(round(resultado,2))
}


################################
########## GENALEX  ############
################################

# Función para obtener los genotipos
genotipo <- function(x, y){
  cuerpo = case_when(x == "1" & y == "1" ~ "1/1",
                     x == "1" & y == "2" ~ "1/2",
                     x == "2" & y == "1"  ~ "1/2",
                     x == "2" & y == "2" ~ "2/2")
  return(cuerpo)
}

##### Funciones de estructura poblacional
# Seguramente hayan librerías, pero whatever
### F de Wright (índices de fijación)
# En proceso ...................................................... Emmmm, creo que no funciona muy bien............
Fs <- function(he,output,pi,qi){
  print(">>> Este es el resultado de la estructura poblacional")
  Hs=mean(he)                     # Heterocigosidad media de las subpoblaciones
  Hr=output_Hr                # media ponderada de la heterocigosidad esperada de los grupo (ki=número de subpoblaciones en región i) esta está complicada de automatizar
  Ht=1-(mean(pi)^2 + mean(qi)^2)  # frecuencia media del alelo i en la población total
  Fsr=(Hr-Hs)/Hr       
  Frt=(Ht-Hr)/Ht
  Fst=(Ht-Hs)/Ht
  resultado=c("Fsr_result:"=Fsr,
            "Frt_result:"=Frt,
            "Fst_result:"=Fst)
  return(resultado)
}

