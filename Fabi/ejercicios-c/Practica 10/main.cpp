#include <iostream>
#include "linkedList.h"
#include "Set.h"

int main() {
    LinkedList list = nil();
    cons(10, list);
    cons(20, list);

    Set set = emptyS();
    std::cout << isEmptyS(set) << std::endl;
    addS(10, set);
    addS(5, set);
    addS(15, set);
    LinkedList setList = setToList(set);
    std::cout << sizeS(set) << std::endl;
    std::cout << belongsS(15, set) << std::endl;
    removeS(15, set);
    std::cout << belongsS(15, set) << std::endl;
    return 0;
}
