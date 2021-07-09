#include <iostream>
#include "Tree.h"
#include "TreeFunctions.h"

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
    ArrayList treeList = toList(t2);
    ArrayList leavesList = leaves(t2);
    ArrayList levelNList = levelN(2, t2);
    std::cout << sumarT(t2) << std::endl;
    std::cout << sizeT(t2) << std::endl;
    std::cout << perteneceT(4, t2) << std::endl;
    std::cout << aparicionesT(1, t2) << std::endl;
    std::cout << heightT(t2) << std::endl;
    std::cout << lengthAL(treeList) << std::endl;

    printArr(treeList);

    printArr(leavesList);

    printArr(levelNList);

    return 0;
}


