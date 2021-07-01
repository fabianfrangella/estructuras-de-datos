#include "Queue.h"

struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

//Crea una lista vacía.
//Costo: O(1).
Queue emptyQ() {
    Queue queue = new QueueSt;
    queue->primero = nullptr;
    queue->ultimo = nullptr;
    queue->cantidad = 0;
}
//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(Queue q) {
    return q->cantidad == 0;
}
//Devuelve el primer elemento.
//Costo: O(1).
int firstQ(Queue q) {
    return q->primero->elem;
}
//Agrega un elemento al final de la cola.
//Costo: O(1).
void enqueue(int x, Queue q) {
    NodoQ* node = new NodoQ;
    node->elem = x;
    node->siguiente = nullptr;
    q->cantidad++;
    if (isEmptyQ(q)) {
        q->primero = node;
        q->ultimo = node;
        return;
    }
    q->ultimo->siguiente = node;
    q->ultimo = node;
}
//Quita el primer elemento de la cola.
//Costo: O(1).
void dequeue(Queue q) {
    if (isEmptyQ(q)) return;
    NodoQ* tmp = q->primero;
    q->cantidad--;
    if (q->cantidad == 1) {
        q->primero = nullptr;
        q->ultimo = nullptr;
        delete tmp;
        return;
    }
    q->primero = tmp->siguiente;
    delete tmp;
}
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(Queue q) {
    return q->cantidad;
}
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void mergeQ(Queue q1, Queue q2) {
    
}
//Libera la memoria ocupada por la lista.
//Costo: O(n).
void destroyQ(Queue q);
