#include <iostream>
#include "Set.h"

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};
struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

//Crea un conjunto vacío.
Set emptyS() {
    SetSt* set = new SetSt;
    set->primero = nullptr;
    set->cantidad = 0;
    return set;
}
//Indica si el conjunto está vacío.
bool isEmptyS(Set s) {
    return s->cantidad == 0;
}
//Indica si el elemento pertenece al conjunto.
bool belongsS(int x, Set s) {
    bool existe = false;
    NodoS* actual = s->primero;
    while (!existe && actual != nullptr) {
        existe = actual->elem == x;
        actual = actual->siguiente;
    }
    return existe;
}
//Agrega un elemento al conjunto.
void addS(int x, Set s) {
    if (belongsS(x, s)) return;
    NodoS* node = new NodoS;
    node->elem = x;
    if (s->primero == nullptr) {
        node->siguiente = nullptr;
        s->primero = node;
        s->cantidad++;
        return;
    }
    node->siguiente = s->primero;
    s->primero = node;
    s->cantidad++;
}

//Quita un elemento dado.
void removeS(int x, Set s) {
    NodoS* actual = s->primero;
    if (actual->elem == x) {
        s->primero = actual->siguiente;
        delete actual;
        s->cantidad--;
        return;
    }
    while (actual != nullptr && actual->elem != x) {
        NodoS* siguiente = actual->siguiente;
        if (actual->elem == x) {
            delete actual;
        }
        actual = siguiente;
        s->cantidad--;
    }
}
//Devuelve la cantidad de elementos.
int sizeS(Set s) {
    return s->cantidad;
}
//Devuelve una lista con los lementos del conjunto.
LinkedList setToList(Set s) {
    LinkedList list = nil();
    NodoS* actual = s->primero;
    while (actual != nullptr) {
        cons(actual->elem, list);
        actual = actual->siguiente;
    }
    return list;
}
//Libera la memoria ocupada por el conjunto.
void destroyS(Set s) {
    NodoS* actual = s->primero;
    while (actual != nullptr) {
        NodoS* siguiente = actual->siguiente;
        delete actual;
        actual = siguiente;
    }
    delete s;
}


