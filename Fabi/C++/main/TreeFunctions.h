//
// Created by Fabian Frangella on 9/7/2021.
//

#include "ArrayList.h"

#ifndef MAIN_TREEFUNCTIONS_H
#define MAIN_TREEFUNCTIONS_H

#endif //MAIN_TREEFUNCTIONS_H

int sumarT(Tree t);
//Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés).
int sizeT(Tree t);
//Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
//árbol.
bool perteneceT(int e, Tree t);
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
//iguales a e.
int aparicionesT(int e, Tree t);
//Dado un árbol devuelve su altura.
int heightT(Tree t);
//Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t);
//Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t);
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t);

int sumarTBFS(Tree t);
int sizeTBFS(Tree t);
bool perteneceTBFS(int e, Tree t);
int aparicionesTBFS(int e, Tree t);
ArrayList toListBFS(Tree t);