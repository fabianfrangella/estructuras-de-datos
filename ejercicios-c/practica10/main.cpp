#include <iostream>
#include "linkedList.h"

int main() {
    LinkedList list = nil();
    cons(10, list);
    cons(20, list);

    std::cout << head(list) << std::endl;
    return 0;
}
