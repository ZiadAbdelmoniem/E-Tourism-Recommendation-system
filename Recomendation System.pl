offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),bus).

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),10, 1), cabin).

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,50).

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,79).

length1([],0).
length1([_|Tail],N) :- length1(Tail,Prev),N is Prev+1.

ismember(Z,[Z|_]).
ismember(Z,[H|T]):-
ismember(Z,T).

intersect(X,[],[]).
intersect(X,[A|B],L3):-
(ismember(A,X),intersect(X,B,L4),L3=[A|L4]);
(\+ismember(A,X),intersect(X,B,L5),L3=L5).

remove(X,X,[]).
remove(X,[X|Y],Y).
remove(X,[A|B],G):-
X\==A,remove(X,B,G1),G=[A|G1].

insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).

perm([],[]).
perm([A|B],R):-
perm(B,G),insert(A,G,R).

subset([],[]).
subset([X|T1],[X|T2]):- subset(T1,T2).
subset([_|T1],T2):- subset(T1,T2).

possibleSubset([],[]).
possibleSubset([H|T],R):- subset([H|T],X),perm(X,R).

choosePreferences(A,B):-
findactivity(A,G),possibleSubset(G,R),
findactivity1(A,D),remove(D,A,A1),
possibleSubset([activity(R)|A1],B).

choosePreferences(A,B):-
\+findactivity(A,G),
possibleSubset(A,B).

preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :-
	ChosenPrefs=[H|T],
	(possibleSubset(ChosenPrefs,[means(M)]),\+possibleSubset(ChosenPrefs,[activity([A]),means(M)]),\+possibleSubset(ChosenPrefs,[mean([M]),accommodation(A1)]),\+possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1),means(M)]),customerPreferredMean(Customer,M,S));
	(possibleSubset(ChosenPrefs,[activity([A])]),\+possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1)]),\+possibleSubset(ChosenPrefs,[activity([A]),means(M)]),\+possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1),means(M)]),customerPreferredActivity(Customer,A,S));
	(possibleSubset(ChosenPrefs,[accommodation(A1)]),\+possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1)]),\+possibleSubset(ChosenPrefs,[mean([M]),accommodation(A1)]),\+possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1),means(M)]),customerPreferredAccommodation(Customer,A1,S)).
	
preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :-
	ChosenPrefs=[H|T],	
    (possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1)]),
	customerPreferredActivity(Customer,A,S1),
	customerPreferredAccommodation(Customer,A1,S2),
	S is S1+S2).
	
preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :-
	ChosenPrefs=[H|T],	
	(possibleSubset(ChosenPrefs,[activity([A]),means(M)]),
	customerPreferredActivity(Customer,A,S1),
	customerPreferredMean(Customer,M,S2),
	S is S1+S2).
	
preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :-
	ChosenPrefs=[H|T],
	(possibleSubset(ChosenPrefs,[mean([M]),accommodation(A1)]),
	customerPreferredMean(Customer,M,S1),
	customerPreferredAccommodation(Customer,A1,S2),
	S is S1+S2).
	
preferenceSatisfaction(Offer, Customer, ChosenPrefs, S) :-
	ChosenPrefs=[H|T],	
	(possibleSubset(ChosenPrefs,[activity([A]),accommodation(A1),means(M)]),
	customerPreferredActivity(Customer,A,S1),
	customerPreferredAccommodation(Customer,A1,S2),
	customerPreferredMean(Customer,M,S3),
	S is S1+S2+S3).

getnum(period(X-Y-Z,X1-Y1-Z1),period(I-J-K,I1-J1-K1),A1,D1):-
A is X*10000, B is Y*100,C is Z*1,A1 is (A+B+C),
D is X1*10000, E is Y1*100,F is Z1*1,D1 is (D+E+F).

getnum1(period(X-Y-Z,X1-Y1-Z1),period(I-J-K,I1-J1-K1),A1,D1):-
A is I*10000, B is J*100,C is K*1,A1 is (A+B+C),
D is I1*10000, E is J1*100,F is K1*1,D1 is (D+E+F).

list(X,X,[X]).
list(X,Y,[H|T]):-
X<Y,
H=X,
X1 is X +1,
list(X1,Y,T).

makelist(A,B,C,D,L1,L2):-
list(A,B,L1),list(C,D,L2).

doesit(L1,L2):-
intersect(L1,L2,B),B\==[].

notover(period(X-Y-Z,X1-Y1-Z1),period(I-J-K,I1-J1-K1)):-
getnum(period(X-Y-Z,X1-Y1-Z1),period(I-J-K,I1-J1-K1),A,B),
getnum1(period(X-Y-Z,X1-Y1-Z1),period(I-J-K,I1-J1-K1),C,D),
((D<A,D<B,C<A,C<B) ; (A<C,A<D,B<C, B<D)).

overlapPeriod(A,B):-
\+notover(A,B).

findperiod([period(X-Y-Z,X1-Y1-Z1)|T],period(X-Y-Z,X1-Y1-Z1)).
findperiod([H|T],P):-findperiod(T,P).

finddest([dest(R)|T],R).
finddest([H|T],P):-finddest(T,P).

findbudget([budget(D)|T],budget(D)).
findbudget([H|T],P):-findbudget(T,P).

findactivity([activity(C)|T],C).
findactivity([H|T],P):-findactivity(T,P).

findactivity1([activity(C)|T],activity(C)).
findactivity1([H|T],P):-findactivity1(T,P).

findmean([means(X)|T],X).
findmean([H|T],G):-findmean(T,G).

findacc([accommodation(A)|T],A).
findacc([H|T],G):-findacc(T,G).

from_offer_to_list(offer(Destination, Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests),Mean,Acc,L):-
insert(dest(Destination),[],U1),
insert(activity(Activities),U1,U2),
insert(budget(Cost),U2,U3),
insert(means(Mean),U3,U4),
insert(accommodation(Acc),U4,L).

work(L,M,A,L1):-
from_offer_to_list1(L,L2),
insert(means(M),L2,L3),
insert(accommodation(A),L3,L1).

from_offer_to_list1(offer(Destination, Activities, Cost, ValidFrom, ValidTo, Period, Duration, NoOfGuests),L):-
insert(dest(Destination),[],I),
insert(activity(Activities),I,K),
insert(budget(Cost),K,L).

getOffer(ChosenPrefs,F):-
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findactivity1(ChosenPrefs,A3),
findactivity(ChosenPrefs,A2),possibleSubset(A,A2),remove(A3,ChosenPrefs,N),
findperiod(N,Pls),overlapPeriod(Pls,P),wiggle(N,New),
findbudget(New,H),H=budget(I),(I>C ;I=C),remove(H,New,New1),
possibleSubset(L,New1),
F=T.

getOffer(ChosenPrefs,F):-
\+findperiod(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findactivity(ChosenPrefs,A2),possibleSubset(A,A2),remove(activity(X),ChosenPrefs,New),
findbudget(New,H),H=budget(I),(I>C ;I=C),remove(H,New,New1),
possibleSubset(L,New1),
F=T.

getOffer(ChosenPrefs,F):-
\+findactivity(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findperiod(ChosenPrefs,Pls),overlapPeriod(Pls,P),wiggle(ChosenPrefs,New),
findbudget(New,H),H=budget(I),(I>C ;I=C),remove(H,New,New1),
possibleSubset(L,New1),
F=T.

getOffer(ChosenPrefs,F):-
\+findperiod(ChosenPrefs,S),
\+findactivity(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findbudget(ChosenPrefs,H),H=budget(I),(I>C ;I=C),remove(H,ChosenPrefs,New1),
possibleSubset(L,New1),
F=T.

getOffer(ChosenPrefs,F):-
\+findperiod(ChosenPrefs,S),
\+findbudget(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findactivity(ChosenPrefs,A2),possibleSubset(A,A2),remove(activity(X),ChosenPrefs,New),
possibleSubset(L,New),
F=T.

getOffer(ChosenPrefs,F):-
\+findactivity(ChosenPrefs,S),
\+findbudget(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
findperiod(ChosenPrefs,Pls),overlapPeriod(Pls,P),wiggle(ChosenPrefs,New),
possibleSubset(L,New),
F=T.

getOffer(ChosenPrefs,F):-
\+findperiod(ChosenPrefs,S),
\+findactivity(ChosenPrefs,S),
\+findbudget(ChosenPrefs,S),
offerMean(T,M),
offerAccommodation(T,Acc),
T=offer(D,A,C,_,_,P,_,_),
work(T,M,Acc,L),
possibleSubset(L,ChosenPrefs),
F=T.

wiggle(A,B):-
findperiod(A,P),remove(P,A,B).

recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
choosePreferences(Prefs,ChosenPrefs),
getOffer(ChosenPrefs,O).

is_sorted([]).
is_sorted([_]).
is_sorted([X,Y|T]):-Y=<X,is_sorted([Y|T]).

% preferenceSatisfaction(Offer, Customer, ChosenPrefs, S)

findminsat(Offer,[(H,S)],Min):-
(S<Min, Min is S);(Min=<S).

findminsat(Offer,[H|T],Min):-
H=(G,S),
(S<Min , Min is)

list_min([(H,S)|Ls], Min) :-
    list_min(Ls, (H,S), Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).

helper1(Offer,L1,L2).


helper(_,[],[],K,K).
helper(Offer,[H|T],[H1|T1],U,N):-
preferenceSatisfaction(Offer,H,H1,S),
insert((H,S),U,U1),
helper(Offer,T,T1,U1,N).

anewday(Customers, PreferenceList, Offer, CustomersChosen):-
offerMean(Offer,Y),
helper(Offer,Customers,PreferenceList,[],NewList),
helper1(NewList,SortedList),
CustomersChosen=SortedList.


recoo([],[],O,K,K).

recoo(List_of_customers,Listoflists,Offer,List_of_Custmers_Chosen,L):-
List_of_customers=[H|T],
Listoflists=[H1|T1],
recommendOfferForCustomer(H1,Satisfiedprefs,Offer),
(
(length1(Satisfiedprefs,N),N>0,insert(H,L,L1),recoo(T,T1,Offer,List_of_Custmers_Chosen,L1));
(length1(Satisfiedprefs,N),N=0,recoo(T,T1,Offer,List_of_Custmers_Chosen,L1))).

recommendOffer(A,B,C,D):-
recoo(A,B,C,D,[]), C=offer(_,_,_,_,_,_,_,NoOfGuests),length1(D,N),
(NoOfGuests>N ; NoOfGuests=N).


riri(A,B,O,R):-
plsba7ebak(A,B,O,R),
O=offer(_,_,_,_,_,_,_,N),
length1(R,W),
((N>W);(N=W)).

riri(A,B,O,R):-
plsba7ebak(A,B,O1,R1),
O=offer(_,_,_,_,_,_,_,N),
length1(R,W),
(W>N),
riri(A,B,O,R),O\=O1.

edini(Customers,PrefrenceList,Offer,CustomersChosen):-
Customers=[H|T];
PrefrenceList=[H1|T1];
CustomersChosen=[],
recommendOfferForCustomer(PrefrenceList,ChosenPrefs,Offer),
(preferenceSatisfaction(Offer,H,H1,S,),S>0,insert(H,CustomersChosen,[H|CustomersChosen])).


% recommendOffer([customer(ahmed, aly, 1993-01-30, single, 0, student)],[[period(2020-9-01,2020-9-15),activity([snorkeling]), budget(15000)],[dest(dahab), activity([diving, snorkeling]),budget(8000)]],O,Cust)
	

%  recommendOfferForCustomer([means(plane),accommodation(cabin),dest(dahab),period(2020-04-01, 2020-09-15), activity([diving,snorkling]), budget(15000)],C,O)	
	
% recommendOffer([customer(ahmed, aly, 1993-01-30, single, 0, student),customer(mohamed, elkasad, 1999-01-30, single, 0, student)],[[period(2020-9-01,2020-9-15),activity([snorkeling]), budget(15000)],[dest(dahab), activity([diving, snorkeling]),budget(8000)]],O,Cust)

% choosePreferences([dest(dahab), period(2020-04-01, 2020-06-15),accommodation(hotel),activity([diving, snorkelling]), budget(5000)],R)

% recommendOffer([customer(ahmed, aly, 1993-01-30, single, 0, student),customer(mohamed, elkasad, 1999-01-30, single, 0, student)],[[period(2020-9-01,2020-9-15),activity([snorkeling]), budget(15000)],[dest(dahab), activity([diving, snorkeling]),budget(8000)]],O,Cust).
% O = offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-2-12, 2020-3-12,
% period(2020-3-15, 2020-4-15), 10, 5),
% Cust = [customer(ahmed, aly, 1993-1-30, single, 0, student), customer(mohamed, elkasad,1999-1-30, single, 0, student)] ;
% O = offer(taba, [diving], 1000, 2020-2-12, 2020-3-12, period(2020-6-1, 2020-8-31), 10, 1),
% Cust = [customer(mohamed, elkasad, 1999-1-30, single, 0, student)] .

% recommendOffer([customer(ahmed, aly, 1993-01-30, single, 0, student),customer(mohamed, elkasad, 1999-01-30, single, 0, student)],[[period(2020-9-01,2020-9-15),activity([snorkeling]), budget(15000)],[dest(dahab), activity([diving, snorkeling]),budget(8000)]],O,Cust)