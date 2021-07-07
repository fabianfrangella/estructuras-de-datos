#include "linkedList.h"
#include <iostream>

//Devuelve la suma de todos los elementos.
// O(n)
int sumatoria(LinkedList xs) {
    int resultado = 0;
    initialize(xs);
    while(!finished(xs)) {
        resultado+= current(xs);
        next(xs);
    }
    return resultado;
}
//Incrementa en uno todos los elementos.
// O(n)
void sucesores(LinkedList xs) {
    initialize(xs);
    while (!finished(xs)) {
        setCurrent(current(xs) + 1, xs);
        next(xs);
    }
}
//Indica si el elemento pertenece a la lista.
// O(n)
bool pertenece(int x, LinkedList xs) {
    bool existe = false;
    initialize(xs);
    while (!existe && !finished(xs)) {
        existe = current(xs) == x;
        next(xs);
    }
    return existe;
}
//Indica la cantidad de elementos iguales a x.
// O(n)
int apariciones(int x, LinkedList xs) {
    int i = 0;
    initialize(xs);
    while (!finished(xs)) {
        i += current(xs) == x ? 1 : 0;
        next(xs);
    }
    return i;
}

//Devuelve el elemento más chico de la lista.
// O(n)
int minimo(LinkedList xs) {
    int resultado = head(xs);
    initialize(xs);
    while (!finished(xs)) {
        resultado = resultado < current(xs) ? resultado : current(xs);
        next(xs);
    }
    return resultado;
}
