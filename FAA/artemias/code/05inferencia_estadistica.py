#!/usr/bin/env python

import pandas as pd
import numpy as np
import scikit_posthocs as sp

matrix_artemias = pd.read_csv("artemias/data/matrix_artemias.csv")
rca2=pd.read_csv("artemias/data/rca.csv")

print(matrix_artemias.head(n=6))

# Hacemos la condición para cambiar los grupos de Tratamiento
condicion = [
    (matrix_artemias["Tratamiento"] == 1),
    (matrix_artemias["Tratamiento"] == 2),
    (matrix_artemias["Tratamiento"] == 3),
    (matrix_artemias["Tratamiento"] == 4),
]
# Nomres de los grupos
nombres = ["Levadura", 
           "Lectina marian", 
           "Echium/bacalao", 
           "Enriquecedor comercial"]
# Cambio mediante la función select de numpy
matrix_artemias["Tratamiento"] = np.select(condicion, 
                                           nombres)
print(matrix_artemias.head(n=6))

# Hacemos Tamhane T2.
# AGs, normales pero no homocedásticos homocedásticos:
# C 16:1n-7; C 18:0; C 18:1n-9; C 18:3n-6; C 20:5n-3; C 22:6n-3
for ag in ['C 16:1n-7', 'C 18:0', 'C 18:1n-9',
           'C 18:3n-6', 'C 20:5n-3', 'C 22:6n-3']:
    print("===>" + ag + "<===")
    print(sp.posthoc_tamhane(matrix_artemias, 
                             val_col=ag, 
                             group_col='Tratamiento', 
                             welch=True))

print("===> Valores de rca 2021 <===")
rc2_artemias = sp.posthoc_tamhane(rca2, 
                                  val_col='valores', 
                                  group_col='tratamiento', 
                                  welch=True)
print(rc2_artemias)

## Resultados de artemias en 2022
artemias_2022 = pd.read_csv("artemias/data/artemias_wider_2022.csv")
pca_artemias_22 = pd.read_csv("artemias/data/cp_2022.csv")
print(artemias_2022.head(n=6))


# Hacemos Tamhane T2.
# AGs, normales pero no homocedásticos homocedásticos:
# 16:1n-7; 18:0 
for ag in ['16:1n-7', '18:0']:
    print("===>" + ag + "<===")
    sp.posthoc_tamhane(artemias_2022, 
                       val_col=ag, 
                       group_col='tratamiento', 
                       welch=True)

RC2_2022 = sp.posthoc_tamhane(pca_artemias_22, 
                              val_col='RC2', 
                              group_col='tratamiento', welch=True)
print("===> Valores de rca 2022 <===")
print(RC2_2022)