#ifndef PRACTICA10_LINKEDLIST_H
#define PRACTICA10_LINKEDLIST_H

#endif //PRACTICA10_LINKEDLIST_H

struct NodoL;
struct LinkedListSt;
typedef LinkedListSt* LinkedList;

LinkedList nil();
//Crea una lista vacía.
bool isEmpty(LinkedList xs);
//Indica si la lista está vacía.
int head(LinkedList xs);
//Devuelve el primer elemento.
void cons(int x, LinkedList xs);
//Agrega un elemento al principio de la lista.
void tail(LinkedList xs);
//Quita el primer elemento.
int length(LinkedList xs);
//Devuelve la cantidad de elementos.
void snoc(int x, LinkedList xs);
//Agrega un elemento al final de la lista.
void initialize(LinkedList xs);
//Apunta el recorrido al primer elemento.
int current(LinkedList xs);
//Devuelve el elemento actual en el recorrido.
void setCurrent(int x, LinkedList xs);
//Reemplaza el elemento actual por otro elemento.
void next(LinkedList xs);
//Pasa al siguiente elemento.
bool finished(LinkedList xs);
//Indica si el recorrido ha terminado.
void destroyL(LinkedList xs);
//Libera la memoria ocupada por la lista.