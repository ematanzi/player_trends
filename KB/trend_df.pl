:- use_module(library(csv)).
:- dynamic df/10.

carica_dati :-
    csv_read_file('players_df.csv', Rows, [functor(dati)]), 
    processa_dati(Rows).

% precedente configurazione
% ["Player", "Age", "Pos", "90s", "90s2", "TklWon", "TklWon2", "Int", "Int2", "GA", "GA2", "Pts/G", "Pts/G2"]

% nuova configurazione
% ["Player", "Age", "Pos", "90s", "90s2", "TklWon", "TklWon2", "Err", "Err2" "GA", "GA2"]

processa_dati([]).
processa_dati([Row | Rest]) :-
    Row = dati(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2), 
    assert(df(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2)), 
    processa_dati(Rest).


trend(Player, very_good) :-
    df(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2),
    very_young_age(Age),
    min_games(Games),
    very_good_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2).

trend(Player, good) :-
    df(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2),
    young_age(Age),
    allowable_games(Games, Age),
    ((very_good_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2));
    (good_condition(TklWon, TklWon2, Games, Games2, Err, Err2, Age) )).

trend(Player, very_bad) :-
    df(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2),
    \+ young_age(Age),
    bad_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2).

trend(Player, bad) :-
    df(Player, Age, Games, Games2, TklWon, TklWon2, Err, Err2, GA, GA2),
    \+ very_young_age(Age),
    bad_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2).

very_young_age(Age) :-
    Age =< 25.

young_age(Age) :-
    Age =< 30.

allowable_games(Games, Age) :-
    ((Games >= 3, Age =< 22); min_games(Games)).

min_games(Games) :-
    Games >= 10.

very_good_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2) :-
    (TklWon2 / Games2 > TklWon / Games; Err2 / Games2 < Err / Games),
    GA2 / Games2 < GA / Games.

good_condition(TklWon, TklWon2, Games, Games2, Err, Err2, Age) :-
    (TklWon2 / Games2 > TklWon / Games, Err2 / Games2 < Err / Games), 
    Age =< 28.

bad_condition(TklWon, TklWon2, Games, Games2, Err, Err2, GA, GA2) :-
    (TklWon2 / Games2 < TklWon / Games; Err2 / Games2 > Err / Games),
    GA2 / Games2 > GA / Games.

trend(Player, static) :-
    \+ trend(Player, very_good),
    \+ trend(Player, good),
    \+ trend(Player, bad),
    \+ trend(Player, very_bad).
    
:- carica_dati.

    