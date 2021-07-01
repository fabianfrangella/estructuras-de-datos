#ifndef PRACTICA10_QUEUE_H
#define PRACTICA10_QUEUE_H

#endif //PRACTICA10_QUEUE_H
struct NodoQ;
struct QueueSt;
typedef QueueSt* Queue;

//Crea una lista vacía.
//Costo: O(1).
Queue emptyQ();
//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(Queue q);
//Devuelve el primer elemento.
//Costo: O(1).
int firstQ(Queue q);
//Agrega un elemento al final de la cola.
//Costo: O(1).
void enqueue(int x, Queue q);
//Quita el primer elemento de la cola.
//Costo: O(1).
void dequeue(Queue q);
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(Queue q);
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void mergeQ(Queue q1, Queue q2);
//Libera la memoria ocupada por la lista.
//Costo: O(n).
void destroyQ(Queue q);
