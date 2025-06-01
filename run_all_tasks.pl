:- consult(greenhouse_domain).
:- consult(legality_tasks).
:- consult(projection_tasks).
:- consult(astar_controller).

print_state(S) :-
    forall(robot(R), (
        at(R,C,S), battery(R,B,S),
        format('  ~w @ ~w, battery=~w~n',[R,C,B])
    )),
    forall(plant(P), (
        (fed(P,true,S) -> St = fed ; St = not_fed),
        format('  ~w: ~w~n',[P,St])
    )).
run :-
    open('results.txt', write, Out),
    setup_call_cleanup(
        set_output(Out),
        run_report,
        ( close(Out), set_output(user) )
    ).
exec_and_print(Label, Plan, S0) :-
    format('~w: ~w~n', [Label, Plan]),
    ( proj(Plan, S0, Sf) ->
        print_state(Sf),
        format('  Sf = ~q~n', [Sf])
    ; writeln('  **ERROR: Plan not executable**')
    ).

run_report :-
    writeln('**'), writeln('LEGALITY TASKS'), writeln('**'), nl,
    (legality_easy->LE=true;LE=false), format('legality_easy:  ~w~n',[LE]),
    (legality_mid ->LM=true;LM=false), format('legality_mid:   ~w~n',[LM]),
    (legality_hard->LH=true;LH=false), format('legality_hard:  ~w~n',[LH]), nl,

    writeln('**'), writeln('PROJECTION TASKS'), writeln('**'), nl,
    (projection_easy(S1)->writeln('projection_easy: OK'), print_state(S1), format('  Sf = ~q~n',[S1])
                       ;writeln('projection_easy: FAILED')), nl,
    (projection_mid(S2) ->writeln('projection_mid:  OK'), print_state(S2), format('  Sf = ~q~n',[S2])
                         ;writeln('projection_mid:  FAILED')), nl,
    (projection_hard(S3)->writeln('projection_hard: OK'), print_state(S3), format('  Sf = ~q~n',[S3])
                         ;writeln('projection_hard: FAILED')), nl,

    writeln('**'), writeln('CONTROLLER TASKS'), writeln('**'), nl,

    % ASTAR: p1 
    retractall(goal(_)), assert((goal(S):-goal_p1(S))),
    writeln('A*: goal p1 fed'),
    ( find_plan_astar(Plan1)
      -> exec_and_print('Plan 1', Plan1, s0)
      ;  writeln('ASTAR (p1 only): FAILED')
    ), nl,

    % ASTAR: p1 & p2 
    retractall(goal(_)), assert((goal(S):-goal_p1p2(S))),
    writeln('A*: goal p1 p2 fed'),
    ( find_plan_astar(Plan2)
      -> exec_and_print('Plan 2', Plan2, s0)
      ;  writeln('ASTAR (p1-p2): FAILED')
    ), nl,

    % ASTAR: p1 & p2 & p3 
    retractall(goal(_)), assert((goal(S):-goal_p1p2p3(S))),
    writeln('A*: goal all p fed'),
    ( find_plan_astar(Plan3)
      -> exec_and_print('Plan 3', Plan3, s0)
      ;  writeln('ASTAR (all p): FAILED')
    ), nl.

%  Utilizzo da prompt:
%     ?- consult(run_all_tasks).
%     ?- run.