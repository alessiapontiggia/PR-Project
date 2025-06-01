:- asserta(user:file_search_path(library,"C:/Prolog/indigolog/interpreters")).
:- ensure_loaded(library(indigolog)).
:- set_prolog_flag(print_depth, 1000).

% STATIC OBJECTS

robot(r1).  robot(r2).
plant(p1).  plant(p2).  plant(p3). 
resource(water).
resource(nutrients).
resource(fertilizer).

cell(c(1,1)). cell(c(1,2)).
cell(c(2,1)). cell(c(2,2)).
cell(c(3,1)). cell(c(3,2)).

adjacent(c(R,C), c(R1,C)) :- R1 is R+1, cell(c(R1,C)).
adjacent(c(R,C), c(R1,C)) :- R1 is R-1, cell(c(R1,C)).
adjacent(c(R,C), c(R,C1)) :- C1 is C+1, cell(c(R,C1)).
adjacent(c(R,C), c(R,C1)) :- C1 is C-1, cell(c(R,C1)).

% NUMERIC CONSTANTS

max_battery(100).

consumption(move,1).
consumption(feed_plant,10).
consumption(transfer_resource,1).
consumption(transfer_energy,1).
consumption(charge,0).

% SITUATION-DEPENDENT FLUENTS

:- dynamic   at/3, battery/3, stock/4, fed/3.
:- multifile stock/4.

% PRIMITIVE ACTIONS

prim_action(move(R,Cf,Ct)).
prim_action(feed_plant(R,P,Res)).
prim_action(charge(R)).
prim_action(transfer_resource(R1,R2,Res,Q)).
prim_action(transfer_energy(R1,R2,Q)).

poss(move(R,Cf,Ct),S) :-
    robot(R), cell(Cf), cell(Ct), Cf \= Ct,
    at(R,Cf,S), adjacent(Cf,Ct),
    battery(R,L,S), consumption(move,C), L >= C.

poss(feed_plant(R,P,Res),S) :-
    robot(R), plant(P), resource(Res),
    at(R,C,S), plant_at(P,C),
    stock(R,Res,Q,S), Q > 0,
    battery(R,L,S), consumption(feed_plant,T), L >= T,
    \+ fed(P,true,S).

poss(charge(R),S) :-
    robot(R),
    recharge_station(C), at(R,C,S),
    battery(R,L,S), L < 100.

poss(transfer_resource(R1,R2,Res,Q),S) :-
    robot(R1), robot(R2), R1 \= R2,
    resource(Res),
    stock(R1,Res,Qmax,S), Qmax > 0,
    Q = 1,                     % istanzia Q (1..Qmax)
    at(R1,C,S), at(R2,C,S).

poss(transfer_energy(R1,R2,Q),S) :-
    robot(R1), robot(R2), R1 \= R2,
    battery(R1,L1,S), L1 > 0,
    Q = 1,                      % istanzia Q (1..L1)
    at(R1,C,S), at(R2,C,S),
    battery(R2,L2,S), max_battery(Max), L2+Q =< Max.

% SUCCESSOR-STATE AXIOMS

at(R,C2,do(A,S)) :-
    ( A = move(R,_,C2) )
 ;  ( at(R,C2,S), A \= move(R,_,_) ).

battery(R,L2,do(A,S)) :-
    battery(R,L1,S),
    (   A = move(R,_,_),       consumption(move,C),            L2 is L1-C
    ;   A = feed_plant(R,_,_), consumption(feed_plant,C),       L2 is L1-C
    ;   A = charge(R),         max_battery(L2)
    ;   A = transfer_energy(R,_,Q),                            L2 is L1-Q
    ;   A = transfer_energy(_,R,Q),                            L2 is L1+Q
    ;                         L2 = L1 ).

stock(R,Res,Q2,do(A,S)) :-
    (   A = feed_plant(R,_,Res)            -> stock(R,Res,Q1,S), Q2 is Q1-1
    ;   A = transfer_resource(R,_,Res,Q)   -> stock(R,Res,Q1,S), Q2 is Q1-Q
    ;   A = transfer_resource(_,R,Res,Q)   -> stock(R,Res,Q1,S), Q2 is Q1+Q
    ;                                         stock(R,Res,Q2,S) ).

fed(P,true,do(A,_)) :- A = feed_plant(_,P,_), !.
fed(P,true,do(_,S)) :- fed(P,true,S).

% INITIAL SITUATION  (s0)

recharge_station(c(1,1)).

plant_at(p1,c(1,2)).
plant_at(p2,c(2,1)).
plant_at(p3,c(3,2)).

at(r1,c(1,1),s0).
at(r2,c(3,2),s0).

battery(r1,80,s0).
battery(r2,10,s0).

stock(r1,water,2,s0).
stock(r1,fertilizer,1,s0).
stock(r2,water,1,s0).
stock(r2,fertilizer,2,s0).