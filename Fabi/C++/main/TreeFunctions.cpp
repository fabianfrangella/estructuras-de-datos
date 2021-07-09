#include <utility>
#include "Tree.h"
#include "ArrayList.h"
#include "TreeFunctions.h"

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
    if (isEmptyT(t)) return false;
    return rootT(t) == e || perteneceT(e, left(t)) || perteneceT(e, right(t));
}
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e.
int aparicionesT(int e, Tree t) {
    if (isEmptyT(t)) return 0;
    return rootT(t) == e ?
        1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t))
        : aparicionesT(e, left(t)) + aparicionesT(e, right(t));
}
//Dado un árbol devuelve su altura.
int heightT(Tree t) {
    if (isEmptyT(t)) return 0;
    return 1 + max(heightT(left(t)), heightT(right(t)));
}

void toListOut(Tree t, ArrayList output) {
    if (!isEmptyT(t)) {
        add(rootT(t), output);
        toListOut(left(t), output);
        toListOut(right(t),output);
    }
}

//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    ArrayList arr = newArrayList();
    toListOut(t, arr);
    return arr;
}

void leavesToListOut(Tree t, ArrayList output) {
    if (isEmptyT(t)) return;
    if (isEmptyT(left(t)) && isEmptyT(right(t))) {
        add(rootT(t), output);
        return;
    }
    leavesToListOut(left(t), output);
    leavesToListOut(right(t), output);
}

//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {
    ArrayList arr = newArrayList();
    leavesToListOut(t, arr);
    return arr;
}

void levelNToListOut(int n, Tree t, ArrayList output) {
    if (isEmptyT(t)) return;
    if (n == 0) {
        add(rootT(t), output);
        return;
    }
    levelNToListOut(n - 1, left(t), output);
    levelNToListOut(n - 1, right(t), output);
}
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {
    ArrayList arr = newArrayList();
    levelNToListOut(n, t, arr);
    return arr;
}

