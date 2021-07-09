#include "Tree.h"
#include "../Practica 9/arraylist.h"
#include "TreeFunctions.h"
/*
Tree emptyT();
Tree nodeT(int elem, Tree left, Tree right);
bool isEmptyT(Tree t);
int rootT(Tree t);
Tree left(Tree t);
Tree right(Tree t);
 */
//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t) {
    if (isEmptyT(t)) return 0;
    return rootT(t) + sumarT(left(t)) + sumarT(right(t));
}
//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés).
int sizeT(Tree t) {
    if (isEmptyT(t)) return 0;
    return 1 + sizeT(left(t)) + sizeT(right(t));
}
//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
//árbol.
bool perteneceT(int e, Tree t) {
    return true;
}
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e.
int aparicionesT(int e, Tree t) {
    return 0;
}
//Dado un árbol devuelve su altura.
int heightT(Tree t) {
    return 0;
}
//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    return newArrayList();
}
//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    return newArrayList();
}
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {
    return newArrayList();
}

