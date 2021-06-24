#include "pokemon.h"
#include <iostream>
using namespace std;

struct PokemonSt {
    TipoDePokemon tipo;
    int vida;
};
struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

//Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo) {
    PokemonSt* p = new PokemonSt;
    p->tipo = tipo;
    p->vida = 100;
}
//Devuelve el tipo de un pokemon.
TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo;
}
//Devuelve el porcentaje de energía.
int energia(Pokemon p) {
    return p->vida;
}
//Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p) {
    p->vida-= energia;
}
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2) {
    if (tipoDePokemon(p1) == "agua") {
        return tipoDePokemon(p2) == "fuego";
    } else if (tipoDePokemon(p1) == "fuego") {
        return tipoDePokemon(p2) == "planta";
    } else if (tipoDePokemon(p1) == "planta") {
        return tipoDePokemon(p2) == "agua";
    }
    return false;
}

//Una vez hecho eso, implementar la siguiente interfaz de Entrenador:

//Dado un nombre, una cantidad de pokemones, y un array de pokemones de ese tamaño,
//devuelve un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones) {
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = pokemones;
    return e;
}
//Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
}
//Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
}
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int cantidad = 0;
    for (int i = 0; i < e->cantPokemon; i++) {
        cantidad += tipoDePokemon(e->pokemon[i]) == tipo ? 1 : 0;
    }
    return cantidad;
}
//Devuelve el pokémon número i de los pokemones del entrenador.
//Precondición: existen al menos i − 1 pokemones.
Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i];
}
// Indica si algun pokemon del entrenador e le gana al pokemon p
bool algunoLeGana(Pokemon p, Entrenador e) {
    int i = 0;
    bool leGanan = false;
    while (!leGanan && i < e->cantPokemon) {
        leGanan = superaA(e->pokemon[i], p);
        i++;
    }
    return leGanan;
}

//Dados dos entrenadores, indica si, para cada pokemon del segundo entrenador, el primero
//posee al menos un pókemon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2) {
   int i = 0;
   bool esGanador = true;
   while (esGanador && i < e2->cantPokemon) {
       esGanador = algunoLeGana(e2->pokemon[i], e1);
       i++;
   }
   return esGanador;
}

