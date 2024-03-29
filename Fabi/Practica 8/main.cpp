#include <iostream>
using namespace std;

struct Par {
    int x;
    int y;
};
// Propósito: construye un par
Par consPar(int x, int y) {
    return {x, y};
}
// Propósito: devuelve la primera componente
int fst(Par p) {
    return p.x;
}
// Propósito: devuelve la segunda componente
int snd(Par p) {
    return p.y;
}
// Propósito: devuelve la mayor componente
int maxDelPar(Par p) {
    if (p.x > p.y) {
        return p.x;
    }
    return p.y;
}
// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p) {
    return consPar(p.y, p.x);
}
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m) {
    return consPar(n / m,n % m);
}

/*
 *
 * Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando
 * la menor cantidad posible de variables. Recuerde que puede definir subtareas en caso de que sea
 * estrictamente necesario.
 */

//Propósito: imprime n veces un string s.
void printN(int n, string s) {
    for (int i = 0; i < n; i++) {
        cout << s << endl;
    }
}
void printNRecursive(int n, string s) {
    if (n > 0) {
        cout << s << endl;
        printNRecursive(n - 1, s);
    }
}

//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    for (int i = n; i >= 0; i--) {
        cout << i << endl;
    }
}

void cuentaRegresivaRecursive(int n) {
    if (n >= 0) {
        cout << n << endl;
        cuentaRegresivaRecursive(n - 1);
    }
}
//Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n) {
    for (int i = 0; i <= n; i++) {
        cout << i << endl;
    }
}

void desdeNaM(int n, int m) {
    if (n <= m) {
        cout << n << endl;
        desdeNaM(n + 1, m);
    }
}

void desdeCeroHastaNRecursive(int n) {
    desdeNaM(0, n);
}

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    int resultado = 0;
    for (int i = 0; i < m; i++) {
        resultado = resultado + n;
    }
    return resultado;
}
int multRecursive(int n, int m) {
    int resultado = 0;
    if (m > 0) {
        resultado = multRecursive(n, m - 1);
    }
    return resultado;
}
//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.
void primerosN(int n, string s) {
    for (int i = 0; i < n; i++) {
        cout << s[i] << endl;
    }
}

void primerosNRecursive(int n, string s) {
    if (n > 0) {
        cout << s[0] << endl;
        primerosNRecursive(n - 1, s.substr(1));
    }
}
//Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    int i = 0;
    while (c != s[i] && i < s.length()) {
        i++;
    }
    return i < s.length();
}

string sinLosPrimerosN(int n, string s) {
    string str;
    for (int i = n; i < s.length(); i++) {
        str += s[i];
    }
    return str;
}

bool perteneceRecursive(char c, string s) {
    return s.length() > 0 &&
            (c == s[0] || perteneceRecursive(c, sinLosPrimerosN(1, s)));
}

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    int apariciones = 0;
    for (char i : s) {
        apariciones+= c == i ? 1 : 0;
    }
    return apariciones;
}

int aparicionesRecursive(char c, string s) {
    return s.length() == 0 ? 0
        : c == s[0] ? 1 + aparicionesRecursive(c, s.substr(1))
        : aparicionesRecursive(c, s.substr(1));
}

struct Fraccion {
    float numerador;
    float denominador;
};
// Suponer que el denominador no es cero
// Propósito: construye una fraccion
Fraccion consFraccion(float numerador, float denominador) {
    return {numerador, denominador};
}
// Propósito: devuelve el numerador
float numerador(Fraccion f) {
    return f.numerador;
}
// Propósito: devuelve el denominador
float denominador(Fraccion f) {
    return f.denominador;
}
// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return f.numerador / f.denominador;
}
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    return consFraccion(f1.numerador * f2.numerador, f1.denominador * f2.denominador);
}
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p) {
    return {1,2};
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
    return {1, 2};
}

/*
int main() {
    string p = perteneceRecursive('c', "abfd") ? "SI" : "NO";
    cout << p << endl;
    return 0;
}
 */
