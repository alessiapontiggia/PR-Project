:- consult(greenhouse_domain).
:- use_module(library(heaps)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

:- dynamic goal/1.

goal_p1(S) :-
    fed(p1, true, S).

goal_p1p2(S) :-
    fed(p1, true, S),
    fed(p2, true, S).

goal_p1p2p3(S) :-
    fed(p1, true, S),
    fed(p2, true, S),
    fed(p3, true, S).

% Default
goal(S) :- goal_p1p2p3(S).

coord(c(R,C), R, C).

manhattan(C1, C2, D) :-
    coord(C1, R1, X1),
    coord(C2, R2, X2),
    D #= abs(R1-R2) + abs(X1-X2).

% elenco delle piante non ancora innaffiate in S
remaining_plants(S, Ps) :-
    findall(P, (plant(P), \+ fed(P,true,S)), Ps),
    Ps \= [].

% euristica h(S): distanza minima dalla posizione corrente alla pianta più vicina
h(S, H) :-
    ( goal(S) ->
        H = 0
    ; at(r1, C, S),
      remaining_plants(S, Ps),
      findall(D,
          ( member(P, Ps),
            plant_at(P, Cp),
            manhattan(C, Cp, D)
          ),
          Ds),
      min_list(Ds, H)
    ).

% nodo nella frontier: node(Stato, PianoFinoAdOra, g)

% predicate principale A*
find_plan_astar(Plan) :-
    h(s0, H0),
    empty_heap(Open0),
    add_to_heap(Open0, H0, node(s0, [], 0), Open),
    empty_assoc(Closed0),
    astar_loop(Open, Closed0, Plan).

% condizione di successo: estraggo un nodo goal
astar_loop(Open, _Closed, Plan) :-
    % get_from_heap(+HeapIn, -Key, -Value, -HeapOut)
    get_from_heap(Open, _F, node(S,Plan,_G), _),
    goal(S), !.

% passo ricorsivo A* 
astar_loop(Open, Closed, Plan) :-
    % estraggo il nodo con f minimo
    get_from_heap(Open, _F, node(S, PSoFar, G), Open1),
    % se già chiuso con costo migliore, skip
    ( get_assoc(S, Closed, G0), G0 =< G ->
        astar_loop(Open1, Closed, Plan)
    ;
        % altrimenti aggiungo S nei chiusi
        put_assoc(S, Closed, G, Closed1),
        % genero tutti i successori validi
        findall(F2-node(S2, P2, G2),
            ( poss(A, S),
              S2 = do(A, S),
              \+ get_assoc(S2, Closed1, _),
              G2 is G + 1,
              h(S2, H2),
              F2 is G2 + H2,
              append(PSoFar, [A], P2)
            ),
            Succs),
        % li inserisco nella frontier
        foldl(add_succ, Succs, Open1, Open2),
        astar_loop(Open2, Closed1, Plan)
    ).

% helper per inserire nella heap 
add_succ(F-node(State,Plan,G), OpenIn, OpenOut) :-
    add_to_heap(OpenIn, F, node(State,Plan,G), OpenOut).
