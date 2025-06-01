
:- consult(greenhouse_domain).

%   Single‑action legality checks (true if poss/2 holds in s0)

legality_easy :- poss(move(r1,c(1,1),c(1,2)),s0).

%  Mid difficulty: r1 can ricaricare la batteria alla stazione (c(1,1))
legality_mid  :- poss(charge(r1),s0).

%  Hard: r2 tenta a trasferire fertilizzante a r1 ma NON sono nella stessa cella
legality_hard :- poss(transfer_resource(r2,r1,fertilizer,1),s0).

