#include <iostream>
#include "Tree.h"
#include "TreeFunctions.h"
#include "Set.h"

void printArr(ArrayList arr) {
    std:: cout << "[";
    for (int i = 0; i < lengthAL(arr); i ++) {
        if (i + 1 != lengthAL(arr)) {
            std::cout << get(i, arr) << ",";
            continue;
        }
        std::cout << get(i, arr) << "";
    }
    std:: cout << "]" << std::endl;
}

int main() {
    Tree t = emptyT();

    Tree t2 = nodeT(1,
                    nodeT(1,
                          nodeT(8, emptyT(), emptyT()), emptyT()),
                    nodeT(3,
                          nodeT(5, emptyT(), emptyT()),
                          nodeT(6, emptyT(), emptyT())));
    std::cout << "------Recursivos------" << std::endl;
    ArrayList treeList = toList(t2);
    ArrayList leavesList = leaves(t2);
    ArrayList levelNList = levelN(2, t2);
    std::cout << "suma: " << sumarT(t2) << std::endl;
    std::cout << "size: " <<sizeT(t2) << std::endl;
    std::cout << "pertenece: " <<perteneceT(4, t2) << std::endl;
    std::cout << "apariciones: " <<aparicionesT(1, t2) << std::endl;
    std::cout << "height: " <<heightT(t2) << std::endl;
    std::cout << "lenght: " <<lengthAL(treeList) << std::endl;

    printArr(treeList);

    printArr(leavesList);

    printArr(levelNList);

    std::cout << "------Iterativos------" << std::endl;
    std::cout << "suma: " <<sumarTBFS(t2) << std::endl;
    std::cout << "size: " <<sizeTBFS(t2) << std::endl;
    std::cout << "pertenece: " <<perteneceTBFS(4, t2) << std::endl;
    std::cout << "apariciones: " <<aparicionesTBFS(1, t2) << std::endl;
    ArrayList treeListBFS = toListBFS(t2);
    printArr(treeListBFS);

    Set s = emptyS();
    imprimirS(s);
    addS(1, s);
    addS(2, s);
    addS(3, s);
    addS(3, s);
    imprimirS(s);

    removeS(1, s);
    removeS(5, s);

    imprimirS(s);

    return 0;
}


