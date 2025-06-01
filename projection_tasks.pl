:- consult(greenhouse_domain).

proj([],S,S).
proj([A|Rest],S0,Sf) :-
    poss(A,S0),
    proj(Rest,do(A,S0),Sf).

projection_easy(Sf) :-
    Actions = [
        move(r1,c(1,1),c(1,2)),
        feed_plant(r1,p1,water)
    ],
    proj(Actions,s0,Sf).

projection_mid(Sf) :-
    Actions = [
        charge(r1), 
        move(r1,c(1,1),c(2,1)),
        feed_plant(r1,p2,water)
    ],
    proj(Actions,s0,Sf).

projection_hard(Sf) :-
    Actions = [
        move(r1,c(1,1),c(2,1)),
        move(r1,c(2,1),c(2,2)),
        move(r1,c(2,2),c(3,2)),
        transfer_resource(r2,r1,fertilizer,1),
        feed_plant(r2,p3,water)
    ],
    proj(Actions,s0,Sf).