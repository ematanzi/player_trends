:- use_module(library(csv)).
:- dynamic mf/10.

carica_dati :-
    csv_read_file('players_mf.csv', Rows, [functor(dati)]), 
    processa_dati(Rows).

% ["Player", "Age", "Pos", "90s", "90s2", "PasTotCmp%", "PasTotCmp%2", "GCA", "GCA2", "GF", "GF2"]

processa_dati([]).
processa_dati([Row | Rest]) :-
    Row = dati(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2), 
    assert(mf(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2)), 
    processa_dati(Rest).


trend(Player, very_good) :-
    mf(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2),
    Age =< 25,
    Games >= 10,
    good_condition(PTC2, PTC, GCA2, GCA, GF2, GF).


trend(Player, good) :-
    mf(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2),
    young_age(Age),
    allowable_games(Games, Age),
    avg_good_condition(PTC2, PTC, GCA2, GCA, GF2, GF, Age).


trend(Player, very_bad) :-
    mf(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2),
    \+ young_age(Age),
    very_bad_condition(PTC, PTC2, GCA, GCA2, GF, GF2).


trend(Player, bad) :-
    mf(Player, Age, Games, Games2, PTC, PTC2, GCA, GCA2, GF, GF2),
    \+ very_young_age(Age),
    ((very_bad_condition(PTC, PTC2, GCA, GCA2, GF, GF2));
    (bad_condition(PTC, PTC2, GCA, GCA2, GF, GF2, Age))).


very_young_age(Age) :-
    Age =< 25.
    
young_age(Age) :-
    Age =< 30.
    
allowable_games(Games, Age) :-
    ((Games >= 3, Age =< 25); min_games(Games)).
    
min_games(Games) :-
    Games >= 10.
    
good_condition(PTC2, PTC, GCA2, GCA, GF2, GF) :-
    (PTC2 > PTC, (GCA2) / GF2 > (GCA) / GF).

avg_good_condition(PTC2, PTC, GCA2, GCA, GF2, GF, Age) :-
    (PTC2 > PTC, (GCA2) / GF2 > (GCA) / GF);
    ((PTC2 > PTC; (GCA2) / GF2 > (GCA) / GF),
    Age=<22).

very_bad_condition(PTC, PTC2, GCA, GCA2, GF, GF2) :-
    PTC2 < PTC, (GCA2) / GF2 < (GCA) / GF.

bad_condition(PTC, PTC2, GCA, GCA2, GF, GF2, Age) :-
    ((PTC2 < PTC; (GCA2) / GF2 < (GCA) / GF), Age >= 28).   

trend(Player, static) :-
    \+ trend(Player, very_good),
    \+ trend(Player, good),
    \+ trend(Player, bad),
    \+ trend(Player, very_bad).

:- carica_dati.