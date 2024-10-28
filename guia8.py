from queue import LifoQueue as Pila
from random import randint

def generar_nros_al_azar(cantidad:int, desde:int, hasta:int) -> Pila[int]:
    p = Pila()

    for i in range(0, cantidad):
        el = randint(desde, hasta)
        p.put(el)
    return p

#generar_nros_al_azar(2,10,20)
print("hoal")

