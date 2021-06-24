#ifndef PRACTICA8CPP_POKEMON_H
#define PRACTICA8CPP_POKEMON_H

#endif //PRACTICA8CPP_POKEMON_H
#include <iostream>
using namespace std;

typedef string TipoDePokemon;
struct PokemonSt;
typedef PokemonSt* Pokemon;
struct EntrenadorSt;
typedef EntrenadorSt* Entrenador;

//Dado un tipo devuelve un pokémon con 100 % de energía.
Pokemon consPokemon(TipoDePokemon tipo);
//Devuelve el tipo de un pokemon.
TipoDePokemon tipoDePokemon(Pokemon p);
//Devuelve el porcentaje de energía.
int energia(Pokemon p);
//Le resta energía al pokémon.
void perderEnergia(int energia, Pokemon p);
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2);

//Una vez hecho eso, implementar la siguiente interfaz de Entrenador:

//Dado un nombre, una cantidad de pokemones, y un array de pokemones de ese tamaño,
//devuelve un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon pokemones);
//Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e);
//Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e);
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
//Devuelve el pokémon número i de los pokemones del entrenador.
//Precondición: existen al menos i − 1 pokemones.
Pokemon pokemonNro(int i, Entrenador e);
//Dados dos entrenadores, indica si, para cada pokemon del segundo entrenador, el primero
//posee al menos un pókemon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2);
