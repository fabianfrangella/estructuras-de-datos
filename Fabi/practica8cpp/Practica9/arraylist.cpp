#include "arraylist.h"
#include <iostream>
using namespace std;
struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.
ArrayList newArrayList() {
    ArrayListSt* arr = new ArrayListSt;
    arr->cantidad = 0;
    arr->capacidad = 16;
    arr->elementos = new int;
    return arr;
}
//Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* arr = new ArrayListSt;
    arr->cantidad = 0;
    arr->capacidad = capacidad;
    arr->elementos = new int;
    return arr;
}
//Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs) {
    return xs->cantidad;
}
//Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs) {
    return xs->elementos[i];
}
//Reemplaza el iésimo elemento por otro dado.
int set(int i, int x, ArrayList xs) {
    xs->elementos[i] = x;
    return x;
}
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
void resize(int capacidad, ArrayList xs) {
    if (xs->cantidad > capacidad) {
        xs->cantidad = capacidad;
    }
    xs->capacidad = capacidad;
}

//Agrega un elemento al final de la lista.
void add(int x, ArrayList xs) {
    xs->elementos[xs->cantidad + 1] = x;
}
//Borra el último elemento de la lista.
void remove(ArrayList xs) {
    xs->cantidad--;
}

//Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs) {
    int suma = 0;
    for (int i = 0; i < xs->cantidad; i++) {
        suma+= xs->elementos[i];
    }
    return suma;
}
//Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
    for (int i = 0; i < xs->cantidad; i++) {
        xs->elementos[i]++;
    }
}
//Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
    int i = 0;
    bool existe = false;
    while (!existe && i < xs->cantidad) {
        existe = x == xs->elementos[i];
        i++;
    }
    return existe;
}
//Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
    int aparicionesX = 0;
    for (int i = 0; i < xs->cantidad; i++) {
        aparicionesX += xs->elementos[i] == x ? 1 : 0;
    }
    return aparicionesX;
}
// Agrega los elementos de xs al final de ys
void addBulk(ArrayList xs, ArrayList ys) {
    for (int i = 0; i < xs->cantidad; i++) {
        add(xs->elementos[i], ys);
    }
}
//Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys) {
    int capacidad = xs->capacidad > ys->capacidad
            ? xs->capacidad
            : ys->capacidad;
    ArrayListSt* arr = newArrayListWith(capacidad * 2);
    addBulk(xs, arr);
    addBulk(ys, arr);
    return arr;
}

//Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs) {
    int elMenor = xs->elementos[0];
    for (int i = 1; i < xs->cantidad; i++) {
        elMenor = elMenor < xs->elementos[i] ? elMenor : xs->elementos[i];
    }
    return elMenor;
}
