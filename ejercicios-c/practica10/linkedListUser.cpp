#include "linkedList.h"
#include <iostream>

//Devuelve la suma de todos los elementos.
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
void sucesores(LinkedList xs) {
    initialize(xs);
    while (!finished(xs)) {
        setCurrent(current(xs) + 1, xs);
        next(xs);
    }
}
//Indica si el elemento pertenece a la lista.
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
int minimo(LinkedList xs) {
    int resultado = head(xs);
    initialize(xs);
    while (!finished(xs)) {
        resultado = resultado < current(xs) ? resultado : current(xs);
        next(xs);
    }
    return resultado;
}
//Dada una lista genera otra con los mismos elementos, en el mismo orden.
//Nota: notar que el costo mejoraría si snoc fuese O(1), ¿cómo podría serlo?
LinkedList copy(LinkedList xs);
//Agrega todos los elementos de la segunda lista al final de los de la primera.
//Nota: notar que el costo mejoraría si snoc fuese O(1), ¿cómo podría serlo?
void append(LinkedList xs, LinkedList ys);
