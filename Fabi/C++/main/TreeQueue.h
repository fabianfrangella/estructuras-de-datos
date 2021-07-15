#ifndef PRACTICA10_QUEUE_H
#define PRACTICA10_QUEUE_H

#endif //PRACTICA10_QUEUE_H
#include "Tree.h"

struct NodoQ;
struct QueueSt;
typedef QueueSt* TreeQueue;

//Crea una lista vacía.
//Costo: O(1).
TreeQueue emptyQ();
//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(TreeQueue q);
//Devuelve el primer elemento.
//Costo: O(1).
Tree firstQ(TreeQueue q);
//Agrega un elemento al final de la cola.
//Costo: O(1).
void enqueue(Tree x, TreeQueue q);
//Quita el primer elemento de la cola.
//Costo: O(1).
void dequeue(TreeQueue q);
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(TreeQueue q);
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void mergeQ(TreeQueue q1, TreeQueue q2);
//Libera la memoria ocupada por la lista.
//Costo: O(n).
void destroyQ(TreeQueue q);
