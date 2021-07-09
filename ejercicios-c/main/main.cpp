#include <iostream>
#include "Practica 11/Tree.h"
#include "Practica 11/TreeFunctions.h"
using namespace std;
int main() {
    Tree t = nodeT(1,
                   nodeT(2, emptyT(), emptyT()),
                   nodeT(3, emptyT(), emptyT()));
    cout << sumarT(t) << endl;
    return 0;
}
