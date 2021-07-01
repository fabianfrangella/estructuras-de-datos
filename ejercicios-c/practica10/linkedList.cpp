#include "linkedList.h"
#include <iostream>
using namespace std;

struct NodoL {
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
    NodoL* actual; // puntero al nodo actual (para recorridos)
    NodoL* ultimo;
};

//Crea una lista vacía.
LinkedList nil() {
    LinkedListSt* ls = new LinkedListSt;
    ls->actual = nullptr;
    ls->primero = nullptr;
    ls->ultimo = nullptr;
    ls->cantidad = 0;
    return ls;
}
//Indica si la lista está vacía.
bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
}
//Devuelve el primer elemento.
int head(LinkedList xs) {
    return xs->primero->elem;
}
//Agrega un elemento al principio de la lista.
void cons(int x, LinkedList xs) {
    NodoL* node = new NodoL;
    node->elem = x;
    node->siguiente = xs->primero;
    xs->primero = node;
    xs->cantidad++;
    if (xs->ultimo == nullptr) {
        xs->ultimo = node;
    }
}
//Quita el primer elemento.
void tail(LinkedList xs) {
    NodoL* tmp = xs->primero;
    xs->primero = xs->primero->siguiente;
    delete tmp;
    xs->cantidad--;

    if (xs->primero == nullptr) {
        xs->ultimo = nullptr;
    }
}
//Devuelve la cantidad de elementos.
int length(LinkedList xs) {
    return xs->cantidad;
}
//Agrega un elemento al final de la lista.
void snoc(int x, LinkedList xs) {
    NodoL* node = new NodoL;
    node->elem = x;
    node->siguiente = nullptr;
    xs->cantidad++;
    if (xs->ultimo == nullptr) {
        xs->ultimo = node;
        xs->primero = node;
        return;
    }
    xs->ultimo->siguiente = node;
    xs->ultimo = node;
}
//Apunta el recorrido al primer elemento.
void initialize(LinkedList xs) {
    xs->actual = xs->primero;
}
//Devuelve el elemento actual en el recorrido.
int current(LinkedList xs) {
    return xs->actual->elem;
}
//Reemplaza el elemento actual por otro elemento.
void setCurrent(int x, LinkedList xs) {
    xs->actual->elem = x;
}
//Pasa al siguiente elemento.
void next(LinkedList xs) {
    xs->actual = xs->actual->siguiente;
}
//Indica si el recorrido ha terminado.
bool finished(LinkedList xs) {
    return xs->actual == nullptr;
}
//Libera la memoria ocupada por la lista.
void destroyL(LinkedList xs) {
    while (!isEmpty(xs)) {
        tail(xs);
    }
    delete xs;
}
