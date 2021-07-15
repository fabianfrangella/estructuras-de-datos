#include "TreeQueue.h"
#include "Tree.h"
struct NodoQ {
    Tree elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

//Crea una lista vacía.
//Costo: O(1).
TreeQueue emptyQ() {
    TreeQueue queue = new QueueSt;
    queue->primero = nullptr;
    queue->ultimo = nullptr;
    queue->cantidad = 0;
    return queue;
}
//Indica si la lista está vacía.
//Costo: O(1).
bool isEmptyQ(TreeQueue q) {
    return q->cantidad == 0;
}
//Devuelve el primer elemento.
//Costo: O(1).
Tree firstQ(TreeQueue q) {
    return q->primero->elem;
}
//Agrega un elemento al final de la cola.
//Costo: O(1).
void enqueue(Tree x, TreeQueue q) {
    if (isEmptyT(x)) return;
    NodoQ* node = new NodoQ;
    node->elem = x;
    node->siguiente = nullptr;
    q->cantidad++;
    if (q->primero == nullptr) {
        q->primero = node;
        q->ultimo = node;
        return;
    }
    q->ultimo->siguiente = node;
    q->ultimo = node;
}
//Quita el primer elemento de la cola.
//Costo: O(1).
void dequeue(TreeQueue q) {
    if (isEmptyQ(q)) return;
    NodoQ* tmp = q->primero;
    if (q->cantidad == 1) {
        q->primero = nullptr;
        q->ultimo = nullptr;
        q->cantidad = 0;
        delete tmp;
        return;
    }
    q->primero = tmp->siguiente;
    q->cantidad--;
    delete tmp;
}
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).
int lengthQ(TreeQueue q) {
    return q->cantidad;
}
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).
void mergeQ(TreeQueue q1, TreeQueue q2) {
    q1->ultimo->siguiente = q2->primero;
    q1->ultimo = q2->ultimo;
    delete q2;
}
//Libera la memoria ocupada por la lista.
//Costo: O(n).
void destroyQ(TreeQueue q) {
    while (!isEmptyQ(q)) {
        dequeue(q);
    }
    delete q;
}
