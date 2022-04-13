# -*- coding: utf-8 -*-
"""
Années : 2021-2022

Auteur : Damien TANNEAU
Cursus : CMI-4 Science des Données

Unité d'enseignement : Projet intergénérationnel CMI
Enseignant : Gilles DURRIEU
Exercice : Simulation
"""

# Modules
import matplotlib.pyplot as plt
import numpy as np

# Population initiale : (sains + infectés + rétablis = 1)
S = [0.95]  # sains
I = [0.05]  # infectés
R = [0]     # rétablis ou vaccinés

# Paramètres
B = 1       # beta
L = 2       # lambda
j_max = 30  # jour de fin de la simulation

# Calculs différentiels
for loop in range(j_max):
    Sprime = -B * S[-1] * I[-1]
    Iprime = B * S[-1] * I[-1] - I[-1]/L
    Rprime = (1/L) * I[-1]
    newS = S[-1] + Sprime
    newI = I[-1] + Iprime
    newR = R[-1] + Rprime
    S.append(newS)
    I.append(newI)
    R.append(newR)

# Graphique
jours = np.arange(0, j_max+1)
plt.plot(jours, S, label="Sains (en %)")
plt.plot(jours, I, label="Infectés (en %)")
plt.plot(jours, R, label="Rétablis (en %)")
plt.xlim(0, j_max)
plt.ylim(0, 1)
plt.xlabel("Temps (jours)")
plt.ylabel("Population")
plt.title(f"Parts d'individus sains, infectés et rétablis au cours du temps\n(β = {B} et λ = {L})")
plt.grid(alpha=0.2)
plt.legend()
plt.show()
