%-------------------------------------------------------------------------

%                %PROJECT TP AI - PROLOG - -M1-ISI-USDB%

% -----------------------------------------------------------------------

                % LINES
ligne(2, metro, [
		 [nation, 0],
		 [avron, 1],
		 [alexandre_dumas,2],
		 [philippe_auguste,1],
		 [pere_lachaise,2],
		 [menilmontant,2],
		 [couronnes,1],
		 [belleville,2],
		 [colonel_fabien,1],
		 [jaures,1],
		 [stalingrad,2],
		 [la_chapelle,1],
		 [barbes_rochechouart,3],
		 [anvers,2],
		 [pigalle,1],
		 [blanche,2],
		 [place_clichy,3],
		 [rome,2],
		 [villiers,3],
		 [monceau,2],
		 [courcelles,2],
		 [ternes,3],
		 [charles_de_gaulle_etoile,3],
		 [victor_hugo,2],
		 [porte_dauphine,3]
		 ], [[5,0],2,[1,45]], [[5,15],2,[1,55]]
).

ligne(3, metro, [
		 [pont_levallois_becon,0],
		 [anatole_france,2],
		 [louise_michel,3],
		 [porte_de_champerret,2],
		 [pereire,2],
		 [wagram,2],
		 [malesherbes,3],
		 [villiers,2],
		 [europe,3],
		 [telegraphe,2],
		 [saint_lazare,4],
		 [havre_caumartin,2],
		 [opera,3],
		 [quatre_septembre,3],
		 [bourse,2],
		 [sentier,3],
		 [reaumur_sebastopol,3],
		 [arts_metiers,3],
		 [temple,2],
		 [republique,3],
		 [parmentier,2],
		 [rue_saint_maur,3],
		 [pere_lachaise,4],
		 [gambetta,2],
		 [porte_de_bagnolet,3],
		 [gallieni,3]
		 ], [[5,35],4,[0,20]], [[5,30],4,[0,20]]
).

ligne(bis_3, metro, [
		    [porte_lilas,0],
		    [saint_fargeau,2],
		    [pelleport,1],
		    [gambetta, 2]
		    ], [[6,0],7,[23,45]], [[6,10],7,[23,55]]
).

ligne(5, metro, [
		 [bobigny_pablo_picasso, 0],
		 [bobigny_pantin, 2],
		 [eglise_de_pantin, 3],
		 [hoche,4],
		 [porte_pantin,3],
		 [ourcq,4],
		 [laumiere,3],
		 [jaures,3],
		 [stalingrad,2],
		 [gare_du_nord,3],
		 [gare_de_est,1],
		 [jacques_bonsergent,2],
		 [republique,3],
		 [oberkampf,2],
		 [richard_lenoir,2],
		 [breguet_sabin,2],
		 [bastille,2],
		 [quai_de_la_rapee,3],
		 [gare_austerlitz,2],
		 [saint_marcel,3],
		 [campo_formio,2],
		 [place_italie,3]
		], [[5,24],3,[1,20]], [[5,30],3,[1,0]]
).

ligne(bis_7, metro, [
		    [pre_saint_gervais,0],
		    [place_fetes, 3],
		    [danube, 0],
		    [jourdain, 2],
		    [bolivar, 2],
		    [buttes_chaumont, 2],
		    [botzaris, 2],
		    [republique, 3],
		    [jaures, 5],
		    [louis_blanc,2]
		    ], [[5,35],8,[0,0]], [[5,50],8,[23,45]]
).

ligne(11, metro, [
                   [mairie_lilas, 0],
                   [porte_lilas, 3],
                   [telegraphe,1],
                   [place_fetes,1],
                   [jourdain, 1],
                   [pyrenees, 1],
                   [belleville, 2],
                   [goncourt, 2],
                   [republique, 3],
                   [arts_metiers, 2],
                   [rambuteau, 1],
                   [hotel_de_ville, 1],
                   [chatelet, 1]
                   ], [[5,15],5,[1,30]], [[5,0],5,[2,0]]
).

ligne(15, bus, [
			[silvestre, 0],
			[martin, 2],
			[simon, 3]
			], [[5,15],5,[1,30]], [[5,0],5,[2,0]]
).

%---------------- EXO 01 ------------------------------

% The addh/3 predicate adds a number of minutes (N) to a given time ([X, Y]).
% The result is a new time represented as [S, C], where S is the new hours and C is the new minutes.
addh([X, Y], 0, [X, Y]).
addh([X, Y], N, [S, C]) :- C is (Y + N) mod 60, S is (X + ((Y + N) // 60)) mod 24.

% The affiche/1 predicate displays a time in a specific format.
affiche([X, Y]) :- X < 10, Y < 10, write('0'), write(X), write('H'), write(':0'), write(Y), !.
affiche([X, Y]) :- X > 10, Y > 10, write(X), write(':H'), write(Y), !.
affiche([X, Y]) :- X < 10, Y > 10, write('0'), write(X), write('H'), write(':'), write(Y), !.
affiche([X, Y]) :- X > 10, Y < 10, write(X), write(':H'), write(':0'), write(Y), !.
%---------------- EXO 02 ------------------------------
% The membre/2 predicate checks if an element A is a member of the given list [[A,_]|_].
% It is used to check if a station is a member of the list of stations in a metro or bus line.
membre(A, [[A,_]|_]).
membre(A, [_|Z]) :- membre(A, Z).

% The lig/3 predicate checks if there is a line (L) between two stations (A1 and A2).
% It uses the membre/2 predicate to check if both stations are members of the line's station list.
lig(A1, A2, L) :- ligne(L, _, LA, _, _), membre(A1, LA), membre(A2, LA).

% The rang/3 predicate finds the position (R) of a station (A) in the station list of a metro or bus line.
rang(A, [[A,_]|_], 1).
rang(A, [_|Z], R) :- rang(A, Z, R1), R is R1 + 1.

% The dernier/2 predicate finds the last element (N) in a list.
dernier([[N,_]], N).
dernier([_|Z], N) :- dernier(Z, N).

% The premier/2 predicate finds the first element (X) in a list.
premier([[X,_]], X).
premier([[X,_]|_], X).

% The directionligne/4 predicate determines the direction (D) of a metro or bus line (L) between two stations (A1 and A2).
% It considers the order of the stations in the station list of the line.
directionligne(A1, A2, L, D) :- lig(A1, A2, L), ligne(L, _, LA, _, _), rang(A1, LA, R1), rang(A2, LA, R2), R1 < R2, dernier(LA, D).
directionligne(A1, A2, L, D) :- lig(A1, A2, L), ligne(L, _, LA, _, _), rang(A1, LA, R1), rang(A2, LA, R2), R1 > R2, premier(LA, D).

% The diffh/3 predicate calculates the difference in minutes (M) between two time representations [H1, M1] and [H2, M2].
diffh([H1, M1], [H2, M2], M) :- H1 >= H2, H3 is (H1 - H2), M3 is (M1 - M2), M is ((H3 * 60) + M3).

% The departtot/5 predicate calculates the departure time (A) from station A1 to A2 given the current time (H) and the line (L).
departtot(A1, A2, H, L, A) :- ligne(L, _, LA, _, [H0, INT, _]), rang(A1, LA, R1), rang(A2, LA, R2),
                           R1 < R2, diffh(H, H0, M), A is (INT - (M mod INT)).
departtot(A1, A2, H, L, A) :- ligne(L, _, LA, _, [H0, INT, _]), rang(A1, LA, R1), rang(A2, LA, R2),
                           R1 > R2, diffh(H, H0, M), A is (INT - (M mod INT)).

% The listedepart/4 predicate finds all possible departure times (R) from station A1 to A2 given the current time (H).
listedepart(A1, A2, H, R) :- findall((X, T), (lig(A1, A2, X), departtot(A1, A2, H, X, T)), R).

% The minimum/2 predicate finds the minimum element in a list of pairs (T, X).
minimum([(T, X)], (T, X)).
minimum([(T1, A) | B], X) :- minimum(B, (T, Y)), mincouple((T1, A), (T, Y), X).

% The premierdepart/4 predicate finds the first departure time from station A1 to A2 given the current time (H).
premierdepart(A1, A2, H, (X, T)) :- listedepart(A1, A2, H, R), minimum(R, (X, T)),
                                    write('La ligne '), write(X), write(' part le plus tard possible de '), write(A1),
                                    write(' à destination de '), write(A2), write(' à l\'horaire :  '), addh(H, T, H2), affiche(H2).

% The ligtot/3 predicate finds the line (L) and its corresponding departure time (H) from station A1 to A2 given the current time (H).
ligtot(A1, A2, L, H) :- premierdepart(A1, A2, H, (L, _)).

% The somme/2 predicate calculates the sum (T) of the elements in a list.
somme([[]], 0).
somme([[_, X]], X).
somme([[_, X] | R], T) :- somme(R, X1), T is X + X1.

% The somme1/2 predicate calculates the sum (T) of the second elements in a list of pairs.
somme1([[_, _] | R], T) :- somme(R, T).

% The avan/3 predicate finds the sublist of elements in the given list (L) that occur before the element (X).
avan(X, [[X, Y] | Z], [[X, Y] | Z]).
avan(X, [_ | Z], R) :- avan(X, Z, R).

% The fin/3 predicate finds the sublist of elements in the given list (L) that occur after the element (X).
fin(X, L, R) :- reverse(L, L1), avan(X, L1, R1), reverse(R1, R).

% The trajet/4 predicate finds the list of stations (R) between two stations (A1 and A2) on a line (L).
trajet(A1, A2, L, R) :- ligne(L, _, LA, _, _), rang(A1, LA, RG1), rang(A2, LA, RG2),
                      RG1 < RG2, avan(A1, LA, R1), fin(A2, R1, R).
trajet(A1, A2, L, R) :- ligne(L, _, LA, _, _), rang(A1, LA, RG1), rang(A2, LA, RG2),
                      RG1 > RG2, reverse(LA, LAR), avan(A1, LAR, R1), fin(A2, R1, R).

% The tempstrajet/3 predicate calculates the total time (T) for a journey between two stations (A1 and A2) on a line (L).
tempstrajet(A1, A2, L, T) :- trajet(A1, A2, L, TR), somme1(TR, T).

% The arrivee/5 predicate calculates the arrival time (T) at station A2 given the departure time (H) from A1 and the line (L).
arrivee(A1, A2, L, H, T) :- ligne(L, _, LA, [H0, INT, _], _), tempstrajet(A1, A2, L, TPS),
                           addh(H, -(TPS), H1), diffh(H1, H0, M), TMP is (M mod INT),
                           addh(H1, -(TMP), H2), addh(H2, TPS, H3), diffh(H, H3, T).

% The listearrivee/4 predicate finds all possible arrival times (R) at station A2 from A1 given the departure time (H).
listearrivee(A1, A2, H, R) :- findall((X, T), (lig(A1, A2, X), arrivee(A1, A2, X, H, T)), R).

% The dernierearrivee/4 predicate finds the last arrival time (X, T) at station A2 from A1 given the departure time (H).
dernierearrivee(A1, A2, H, (X, T)) :- listearrivee(A1, A2, H, R),
                                      minimum(R, (X, T)),
                                      write('La ligne '), write(X),
                                      write(' arrive le plus tard à '), write(A2),
                                      write(' à l\'horaire :  '), addh(H, -(T), H2), affiche(H2).

% The ligtard/4 predicate finds the line (L) that arrives the latest at station A2 from A1 given the departure time (H).
ligtard(A1, A2, L, H) :- dernierearrivee(A1, A2, H, (L, _)).
%--------------------- EXO 03 ----------------------------
% The ajoute/3 predicate adds an element X to the beginning of a list Liste, resulting in a new list.
ajoute(X, Liste, [X|Liste]).

% The lister/2 predicate extracts the station names from a list of station-duration pairs.
lister([], Z).
lister([[X,D]|L], [X|Z]) :- lister(L, Z).

% The lister_tt/3 predicate extracts station names from a list of lists of station-duration pairs.
lister_tt([], B, B).
lister_tt([X|L], B, Z) :- lister(X, U), ajoute(U, B, T), lister_tt(L, T, Z).

% The li/3 predicate flattens a list of lists into a single list.
li([], B, B).
li([X|L], B, R) :- append(X, B, U), li(L, U, R).

% The sup/4 predicate removes elements A and A2 from a list.
sup(A, A2, [], []).
sup(A, A2, [X|L], Z) :- X = A2, sup(A, A2, L, Z).
sup(A, A2, [X|L], Z) :- X = A, sup(A, A2, L, Z).
sup(A, A2, [X|L], [X|Z]) :- sup(A, A2, L, Z).

% The recup_der/2 predicate retrieves the last element from a list.
recup_der([X], [X|Z]).
recup_der([X|L], Z) :- recup_der(L, Z).

% The inter1/3 predicate finds the intersection of two lists.
inter1([], A, []).
inter1([X|L], A, [X|L2]) :- member(X, A), !, inter1(L, A, L2).
inter1([X|L], A, L2) :- inter1(L, A, L2).

% The ajout_fin/3 predicate adds an element X to the end of a list.
ajout_fin(X, [], [X]).
ajout_fin(X, [Y|L1], [Y|L2]) :- ajout_fin(X, L1, L2).

% The ajout_debut/3 predicate adds an element X to the beginning of a list.
ajout_debut(X, L, [X|L]).

% The renverse_liste/3 predicate reverses a list.
renverse_liste([], L2, L2).
renverse_liste([[X,D]|L1], L2, L4) :- ajout_debut([X,D], L2, L3), renverse_liste(L1, L3, L4).

% The concatene/3 predicate concatenates two lists.
concatene([], L2, L2).
concatene([X|L1], L2, [X|R]) :- concatene(L1, L2, R).

% The concatene2/3 predicate concatenates a list of lists.
concatene2([], B, B).
concatene2([X|L1], B, T) :- concatene(X, [], R), append(R, B, S), concatene2(L1, S, T).

% The fact/4 predicate appends a station-duration pair [A,N] to a list B.
fact([A], [], B, B).
fact([A], [X|L], B, E) :- ajoute(A, X, K), ajoute(K, B, S), fact([A], L, S, E).

% The renverse/3 predicate reverses a list.
renverse(X, [[X,D]|L], [[X,D]|L]).
renverse(X, [[Y,D]|L], T) :- renverse_liste([[Y,D]|L], L2, T).

% The en_arret/1 predicate finds all stations in the metro network.
en_arret(L) :- findall(M, ligne(_, _, M, _, _), L).

% The ensemble_liste/1 predicate creates a list of all stations in the metro network.
ensemble_liste(L) :- en_arret(Z), lister_tt(Z, B, R), li(R, E, W), append(W, [], T), !, sort(T, L).

% The parcours_sup/4 predicate finds a list of stations that can be reached from a given station.
parcours_sup(A, [], L).
parcours_sup(A, [X|Y], [X|L]) :- lig(A, X, _), parcours_sup(A, Y, L).
parcours_sup(A, [X|Y], L) :- parcours_sup(A, Y, L).

% The liste_arret_conect/4 predicate finds a list of connected stations for a given list of stations.
liste_arret_conect([], E, K, K).
liste_arret_conect([X|L], E, K, Z) :- parcours_sup(X, E, U), ajoute(U, K, T), liste_arret_conect(L, E, T, Z).

% The liste_seif/4 predicate finds a list of stations that can be reached from A1 from station A given the current time H.
liste_seif(A, A1, L, H) :- ensemble_liste(W), sup(A, A1, W, Z), parcours_sup(A, Z, U),
                         append(U, [], T), liste_arret_conect(T, Z, K, L),
                         parcours_sup(A1, Z, I), append(I, [], T1),
                         liste_arret_conect(T1, Z, J, H).

% The o_list/4 predicate finds the list of stations that can be reached from A1 from station A.
o_list(A, A1, L, L2) :- liste_seif(A, A1, Z, U), li(Z, E, W), append(W, [], T), li(U, J, M),
                       append(M, [], T2), !, sort(T, L), sort(T2, L2).

% The extra_par/5 predicate finds additional stations that can be reached from A1 from station A.
extra_par(A, A1, [], B, B).
extra_par(A, A1, [X|L], B, R) :- parcours_aux(A, X, L2), parcours_aux(X, A1, L3),
                                  fact(L2, L3, I, Z), append(Z, B, U),
                                  extra_par(A, A1, L, U, R).

% The k_lig/3 predicate finds all lines connecting stations A and A1.
k_lig(A, [], R).
k_lig(A, [X|L], [X|R]) :- lig(X, A, N), k_lig(A, L, R).
k_lig(A, [X|L], R) :- k_lig(A, L, R).

% The extra_list/3 predicate finds additional stations that can be reached from A1 from station A.
extra_list(A, A1, R) :- o_list(A, A1, Z, U), inter1(U, Z, L), k_lig(A, L, Q),
                       extra_par(A, A1, Q, B, R).

% The par/5 predicate finds pairs of stations and their connected stations in both directions.
par(A, A1, [], B, B).
par(A, A1, [X|L], B, Z) :- lig(A, X, N), lig(X, A1, M), ajoute([[A, N, X], [X, M, A1]], B, U),
                           par(A, A1, L, U, Z).

% The parcours/3 predicate finds a list of connected stations between A and A1.
parcour(A, A1, L) :- ensemble_liste(W), sup(A, A1, W, Z), parcours_sup(A, Z, U),
                    parcours_sup(A1, Z, I), append(I, [], T), append(U, [], T1),
                    inter1(T, T1, L), !.

% The parcours_aux/3 predicate finds connected stations between A and A1 and adds them to a list.
parcours_aux(A, A1, L) :- lig(A, A1, N), ajoute([A, N, A1], [], L), !.
parcours_aux(A, A1, L) :- parcour(A, A1, Z), par(A, A1, Z, B, L).

% The Averif/4 predicate verifies if there are additional stations that can be reached from A1 from station A.
Averif(A, A1, [], L) :- extra_list(A, A1, L), !.
verif(A, A1, L, L).

% The parcours_metro/3 predicate finds connected stations between A and A1 in the metro network.
parcours_metro(A, A1, L) :- parcours_aux(A, A1, E), verif(A, A1, E, L).

% The parcours_metro22/5 predicate appends additional stations that can be reached from A1 from station A.
parcours_metro22(A, A1, [], B, B).
parcours_metro22(A, A1, [X|L], B, R) :- ajoute([[A, X, A1]], B, Z), parcours_metro22(A, A1, L, Z, R).

% The parcours_metro2/3 predicate finds additional stations that can be reached from A1 from station A in the metro network.
parcours_metro2(A, A1, L) :- lig(A, A1, M), findall(M, (ligne(M, _, M1, _, _), lig(A, A1, M)), L2),
                            parcours_metro22(A, A1, L2, B, L), !.
parcours_metro2(A, A1, L) :- parcours_metro(A, A1, L).

% The plutot/4 predicate finds the earliest connection between two stations given the current time.
plutot(A, [[X,N,V]|L], [H,M], [[X,N,V]|L]) :- ligtot(A, _, N, [H,M]), !.
plutot_list(A, [X|L], [H,M], T) :- plutot(A, X, [H,M], T), !.
plutot_list(A, [X|L], [H,M], T) :- plutot_list(A, L, [H,M], T).

% The itintot/4 predicate finds the earliest connection between two stations given the current time.
itintot(A, A1, [H,M], L) :- ligtot(A, A1, N2, [H,M]), ajoute([A, N2, A1], [], L), !.
itintot(A, A1, [H,M], L) :- parcours_metro(A, A1, Q), !, plutot_list(A, Q, [H,M], L), !.

% The plutard2/3 predicate finds the latest connection between two stations given the current time.
plutard2(A, [[X,N,V]], [H,M]) :- ligtard(A, _, N, [H,M]).
plutard(A, [[X,N,V]|L], [H,M], [[X,N,V]|L]) :- recup_der([[X,N,V]|L], Z), plutard2(A, Z, [H,M]), !.
plutard_list(A, [X|L], [H,M], T) :- plutard(A, X, [H,M], T), !.
plutard_list(A, [X|L], [H,M], T) :- plutard_list(A, L, [H,M], T).

% The itintard/4 predicate finds the latest connection between two stations given the current time.
itintard(A, A1, [H,M], L) :- ligtard(A, A1, N2, [H,M]), ajoute([A, N2, A1], [], L), !.
itintard(A, A1, [H,M], L) :- parcours_metro(A, A1, Q), !, plutard_list(A1, Q, [H,M], L), !.
%--------------------- EXO 04 ----------------------------
% Display the result for the earliest schedule
affiche_resultat_tot(A, B, [M,X]):- write('Earliest schedule:  '), affiche(X), nl,
					write('------------------------------------------------------------------------'), nl,
					write('By line '), write(M), nl, station(A, B, M).

% Display the result for the latest schedule
affiche_resultat_tard(A, B, [M,X]):- write('Latest schedule:  '), affiche(X), nl,
					write('------------------------------------------------------------------------'), nl,
					write('By line '), write(M), nl, station(A, B, M).

% Display stations for a given route with the minimum number of stations
affiche_station1(A, B, L, P, R):- ligaller1(A, B, L, RA, RB), !, affiche_station2(P, RA, RB, 1, [], R).
affiche_station1(A, B, L, P, R):- ligallerR(A, B, L, RA, RB), !, affiche_station2(P, RB, RA, 1, [], R).

% If Aux is between the positions RA and RB, include it in the list
affiche_station2([], _, _, _, Tmp, R):-  R = Tmp.
affiche_station2([P|Y], RA, RB, Aux, Tmp, R):- Aux >= RA, Aux =< RB, Aux1 is Aux + 1, addlist(P,Tmp, Tmp1), !, affiche_station2(Y, RA, RB, Aux1, Tmp1, R).
affiche_station2([_|Y], RA, RB, Aux, Tmp, R):- Aux1 is Aux + 1, !, affiche_station2(Y, RA, RB, Aux1, Tmp, R).
addlist(X,L,L1):- L1=[X|L].

% Display the list of stations
station(A, B, L):- ligne(L, _, P, _, _), !, affiche_station1(A, B, L, P, R), reverse(R, R1), nl, write('Stations~'), nl, affichage_station(R1,0).

% Display the station list
affichage_station([], Aux):- write('Total duration of the journey:  '), write(Aux), write(' min').
affichage_station([[A, T]|Y], Aux):- write('~ '), write(A), write(' duration '), write(T), write(' min'), nl, Aux1 is Aux + T, affichage_station(Y, Aux1).

% Add minutes to the given time
addh([X,Y],_,[X,Y]).
addh([X,Y],M,[R,S]):- Y1 is (Y + M) mod 60,
			A is (Y + M) // 60,
			X1 is X + A,
			X1=R, Y1=S.

% Return the time when adding M minutes to the given time
addh1([H1, M1], M, [H2, M2]) :- H3 is (M1 + M) // 60,
				H2 is (H1 + H3) mod 24,
				M2 is (M1 + M) mod 60.
% Connect stations A and B through the metro line M
ligar(_, _, [], _, L, L, _).
ligar(A, B, [X|Y], H, L, K, P):- ligaller(A, B, X), ligne(X, _, _, Time, _), addt(A, X, Time, H, Lr, P), time_n_ligne(X,Lr,R),
				ligar(A, B, Y, H, [R|L], K, P).
ligar(A, B, [X|Y], H, L, K, P):- ligne(X, _, _, _, Time), addtreverse(A, X, Time, H, Lr, P), time_n_ligne(X,Lr,R),
				ligar(A, B, Y, H, [R|L], K, P).

% Check if it's an outbound journey
ligaller(A,B,M):- ligne(M, _, P, _, _),
			calc(A, P, 1, R),
			calc(B, P, 1, S),!,
			truealler(R, S).

% Same as ligaller but also returns the positions of stations A and B
ligaller1(A, B, M, R, S):- ligne(M, _, P, _, _),
			calc(A, P, 1, R),
			calc(B, P, 1, S),!,
			truealler(R, S).

% Same as ligaller1 but without checking the outbound direction
ligallerR(A, B, M, R, S):- ligne(M, _, P, _, _),
			calc(A, P, 1, R),
			calc(B, P, 1, S).

% Calculate the position of station A in metro X relative to its initial position
calc(A,[[A,_]|_],Aux,R):- !, R = Aux.
calc(A,[_|Q],Aux,R):- Aux1 is Aux + 1,
			calc(A, Q, Aux1, R).

% Check if it's an outbound journey, i.e., station A with number R is less than station B with number S
truealler(R,S):- R < S.

% Check if the requested schedule is between the opening time + departure station + the accumulation of time needed for the first transport to reach the departure station
addt(A,Ligne,[Hdep,Acc,_],H, Hr, S):-  S == 0, ligne(Ligne, _, P, _, _),
				addNbstat(A, P, Hdep, Plustat),
				accumultot(Plustat, Acc, H, 0, Hr).
addt(A,Ligne,[Hdep,Acc,_],H, Hr, _):- ligne(Ligne, _, P, _, _),
				addNbstat(A, P, Hdep, Plustat),
				accumultard(Plustat, Acc, H, 0, Hr).

% Same as addt but the metro lines are reversed
addtreverse(A,Ligne,[Hdep,Acc,_],H, Hr, S):-  S == 0, write('la'),  ligne(Ligne, _, P, _, _), reverse(P, Rev),
				addNbstat(A, Rev, Hdep, Plustat),
				accumultot(Plustat, Acc, H, 0, Hr).
addtreverse(A,Ligne,[Hdep,Acc,_],H, Hr, _):- write('ici'), ligne(Ligne, _, P, _, _), reverse(P, Rev),
				addNbstat(A, Rev, Hdep, Plustat),
				accumultard(Plustat, Acc, H, 0, Hr).
% Accumulate the time for the number of stations to the departure station
% If the station is found, add the time traveled by the transport between the previous station and the current one
addNbstat(StatA,[[StatA,Acc]|_],[H,M],[Haux,Maux]):- Haux is H, Maux is M + Acc.
addNbstat(StatA,[[_,X]|P],Heure,Tstat):- addh1(Heure, X, Rheure),
					 addNbstat(StatA, P, Rheure, Tstat).

% Multiply the time by the minutes Acc as long as it does not exceed the given time T
% If compareMin is true, then store the previous time as a result
accumultard(Ht, _, T, Av, R):- compareMin(Ht, T),!,R = Av.
accumultard(Ht, Acc, T, _, R):- Av1 = Ht, addh1(Ht, Acc, Z),
				accumultard(Z, Acc, T, Av1, R).

% True if the schedule exceeds the given time
compareMin([H, _], [A, _]):- H > A.
compareMin([H, M], [A, B]):- H == A, M > B.

% Add the time by the minutes Acc (time needed before the next metro arrives) until it exceeds T
accumultot(Ht, _, T, _, R):- compareMin(Ht, T), R = Ht.
accumultot(Ht, Acc, T, _, R):-  addh1(Ht, Acc, Z),
				accumultot(Z, Acc, T, _, R).

% Return the smallest time
plustot([[L,[H,M]]|Y], R):- plustot1(Y, [L,[H,M]], R).

% Find the smallest time among the list L
plustot1([], A, A):- !.
plustot1([[Ligne,[H,M]]|Y],[_,[P,Q]], R):- H =< P, M < Q,!,
					plustot1(Y, [Ligne,[H, M]], R).
plustot1([_|Y], A, R):- plustot1(Y, A, R).

% Return the largest time in the list
plustard([[L,[H,M]]|Y], R):- plustard1(Y, [L,[H,M]], R).

% Find the largest time among the list L
plustard1([], A, A):- !.
plustard1([[Ligne,[H,M]]|Y],[_,[P,Q]], R):- H >= P, M > Q,!,
					plustard1(Y, [Ligne,[H, M]], R).
plustard1([_|Y], A, R):- plustard1(Y, A, R).

% Create an element containing the metro name with the nearest schedule for that metro
time_n_ligne(X,[Y1,Y2],Z):-  Z = [X,[Y1,Y2]].

% Find the earliest schedule considering the minimum number of stations
ligtot1(A,B,_,H,T,O):- O == 1, findall(X,(ligne(X, T,_,_,_),lig(A,B,X)), R),
		ligar1(A, B, R, H, [], Ltot, 0),
		nl, moinstat(Ltot, Htot), !,
		nl, affiche_resultat_stat(A, B, Htot).

% Find the earliest schedule without considering the minimum number of stations
ligtot1(A,B,L,H,T,_):- findall(X,(ligne(X, T,_,_,_),lig(A,B,X)), R),
		ligar(A, B, R, H, [], Ltot, 0),
		nl,plustot(Ltot, Htot), !,
		nl,affiche_resultat_tot(A, B, Htot),
		verif(Htot,L).
ligtot1(_,_,_,_,T,_):- write('No route for the requested means of transport  ==>  '), !, write(T), nl.

% Find the latest schedule considering the minimum number of stations
ligtard1(A,B,_,H,T,O):- O == 1, findall(X,(ligne(X,T,_,_,_),lig(A,B,X)),R),
		ligar1(A, B, R, H, [], Ltard, 1),
		nl, moinstat(Ltard, Htard),!,
		nl, affiche_resultat_stat_tard(A, B, Htard).

% Find the latest schedule without considering the minimum number of stations
ligtard1(A,B,L,H,T,_):- findall(X,(ligne(X,T,_,_,_),lig(A,B,X)),R),
		ligar(A, B, R, H, [], Ltard, 1),
		nl, plustard(Ltard, Htard),!,
		nl, affiche_resultat_tard(A, B, Htard),
		verif(Htard,L).
ligtard1(_,_,_,_,T,_):- write('No route for the requested means of transport  ==>  '), !,write(T), nl.


% Connect stations A and B through the metro line M, considering the minimum number of stations
ligar1(_, _, [], _, L, L, _).
ligar1(A, B, [X|Y], H, L, K, P):- ligaller(A, B, X), ligne(X, _, _, Time, _), addt(A, X, Time, H, Lr, P),
				nbstation(A, B, X, Nbs),
				time_n_ligne1(X, Lr, Nbs, R),
				ligar1(A, B, Y, H, [R|L], K, P).
ligar1(A, B, [X|Y], H, L, K, P):- ligne(X, _, _, _, Time), addtreverse(A, X, Time, H, Lr, P),
				nbstation(A , B, X, Nbs),
				time_n_ligne1(X, Lr, Nbs, R),
				ligar1(A, B, Y, H, [R|L], K, P).

% Count the number of stations
% abs = absolute value
nbstation(A,B,L,R):- ligne(L,_,P,_,_), calc(A,P,0,R1), calc(B,P,0,R2),
			R3 is R2 - R1, R is abs(R3).

% Add the line, the number of stations, and the time to a variable Z
time_n_ligne1(X,[Y1,Y2], S, Z):-  Z = [X,S,[Y1,Y2]].

% Find the line with the fewest stations
moinstat([[L,S,[H,M]]|Y], R):- moinstat1(Y, [L,S,[H,M]], R).

% Find the line with the fewest stations among the list L
moinstat1([], A, A):- !.
moinstat1([[M1, B, [H,M]]|Y], [_, A, _], R):-  B =< A, moinstat1(Y, [M1, B, [H, M]], R).
moinstat1([_|Y], A, R):- moinstat1(Y, A, R).

% Display results based on the number of stations
affiche_resultat_stat(A, B, [M,S,X]):- write('Earliest schedule:  '), affiche(X),nl,
				write('------------------------------------------------------------------------'), nl,
				write('With the minimum number of stations, the line is '), write(M),nl,
				write('The number of stations is: '), write(S), nl,
				station(A, B, M).

affiche_resultat_stat_tard(A, B, [M,S,X]):- write('Latest schedule:  '), affiche(X),nl,
				write('------------------------------------------------------------------------'), nl,
				write('With the minimum number of stations, the line is '), write(M),nl,
				write('The number of stations is: '), write(S), nl,
				station(A, B, M).
