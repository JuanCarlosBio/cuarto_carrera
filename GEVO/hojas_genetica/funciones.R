##### Métodos para la cuantificación genética de secuencias. Haplotipos

### Loci polimórficos

LP <- function(loci_polimorficos,ntotal_loci){
    cuerpo=(loci_polimorficos/ntotal_loci)*100
    return(cuerpo)
}

### Diversidad haplotípica.
# 1)
Hd <- function(pi){
    cuerpo=1-sum(pi^2)
    return(cuerpo)
}
# 2) N = individuos
Hd <- function(N,pi){ 
    cuerpo=(N/(N-1))*(1-sum(pi^2))
    return(cuerpo)
}

### Polimorfismo nucleotídico.
Ps = function(P,L){ 
    cuerpo=P/L
    return(cuerpo)
}

### Diversidad nucletídica 
# estumador de Watterson (0~O +/- var)
# i=vector de las frecuencias
ndiff <- function(i,Ps){
    a1=sum(1/i)
    a2=sum(1/i^2)
    O=Ps/a1
    V=(O/(L*a1))+((a2*O^2)/(a1^2))
    var=sqrt(V)
    vector=c("diff_nucleotide"=O, "var"=var)
    return(vector)
} 

### Diversidad nucleotídica basada en el número de diferencias

num_ndiff <- function(diff_ij,L,n){
    print(">>> Resultado de la diversidad nucleotídica:  (u+/-σ)")
    C=n*(n-1)/2
    npi=diff_ij/(L*C)
    b1=(n+1)/(3*(n-1))
    b2=(2*(n^2+n+3))/(9*n*(n-1))
    V=(npi*(b1/L)) + (b2*npi^2)
    vector=c("num_ndiff"=npi,"var"=V)
    return(vector)
    }


