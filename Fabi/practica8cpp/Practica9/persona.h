#include <iostream>
using namespace std;
#ifndef PRACTICA8CPP_PERSONA_H
#define PRACTICA8CPP_PERSONA_H

#endif //PRACTICA8CPP_PERSONA_H

struct PersonaSt;
typedef PersonaSt* Persona;

//Devuelve el nombre de una persona
Persona consPersona(string nombre, int edad);
//Devuelve el nombre de una persona
string nombre(Persona p);
//Devuelve la edad de una persona
int edad(Persona p);
//Aumenta en uno la edad de la persona.
void crecer(Persona p);
//Modifica el nombre una persona.
void cambioDeNombre(string nombre, Persona p);
//Dadas dos personas indica si la primera es mayor que la segunda.
bool esMayorQueLaOtra(Persona p1, Persona p2);
//Dadas dos personas devuelve a la persona que sea mayor.
Persona laQueEsMayor(Persona p1, Persona p2);
