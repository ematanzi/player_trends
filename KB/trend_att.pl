:- use_module(library(csv)).
:- dynamic att/10.

carica_dati :-
    csv_read_file('players_fw.csv', Rows, [functor(dati)]), 
    processa_dati(Rows).

processa_dati([]).
processa_dati([Row | Rest]) :-
    Row = dati(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2), 
    assert(att(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2)), 
    processa_dati(Rest).


trend(Player, very_good) :-
    att(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2),
    very_young_age(Age),
    min_games(Games),
    goals_increased(GCA, GCA2),
    goals_team_increased(GF, GF2, Games, Games2). 


trend(Player, good) :-
    att(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2),
    young_age(Age),
    allowable_games(Age, Games),
    ((goals_increased_age_constraint(GCA, GCA2, Games, Games2, Age));
    (goals_increased(GCA, GCA2), goals_team_increased(GF, GF2, Games, Games2))).


trend(Player, very_bad) :-
    att(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2),
    \+ young_age(Age),
    ((goals_scored_decreased(Goals, Goals2, Games, Games2), \+ goals_increased(GCA, GCA2));
    (abs_goals_team_increased(GF, GF2), goals_scored_decreased(Goals, Goals2, Games, Games2); \+ goals_increased(GCA, GCA2))).


trend(Player, bad) :-
    att(Player, Age, Games, Games2, Goals, Goals2, GCA, GCA2, GF, GF2),
    \+ very_young_age(Age),
    ((goals_scored_decreased(Goals, Goals2, Games, Games2); \+ goals_increased(GCA, GCA2));
    ((goals_scored_decreased(Goals, Goals2, Games, Games2), \+ goals_increased(GCA, GCA2)),
    abs_goals_team_increased(GF, GF2))).


very_young_age(Age) :-
    Age =< 25.
    
young_age(Age) :-
    Age =< 30.
    
allowable_games(Games, Age) :-
    ((Games >= 3, Age =< 22); min_games(Games)).
    
min_games(Games) :-
    Games >= 10.

goals_increased_age_constraint(GCA, GCA2, Games, Games2, Age) :-
    Age =< 28,
    goals_increased(GCA, GCA2).

goals_increased(GCA, GCA2) :-
    GCA2 > GCA.

goals_scored_decreased(Goals, Goals2, Games, Games2) :-
    Goals2 / Games2 < Goals / Games.

goals_team_increased(GF, GF2, Games, Games2) :-
    GF2 / Games2 > GF / Games.

abs_goals_team_increased(GF, GF2) :-
    GF =< GF2.

trend(Player, static) :-
    \+ trend(Player, very_good),
    \+ trend(Player, good),
    \+ trend(Player, bad),
    \+ trend(Player, very_bad).

:- carica_dati.
