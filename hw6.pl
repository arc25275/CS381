/* Part 1: Database Application */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

%% Schedule predicate, to get a full schedule. 
% (Student, Place, Time) : (X=Class Number)
schedule(S, P, T) :- enroll(S,X), where(X,P), when(X,T).

%% Usage predicate to find what classes happen where and at what time. 
% (Place, Time) : (X=Class Number)
usage(P,T) :- where(X,P), when(X,T).

%% Conflict predicate to see if there are any classes that will happen in the same place and time. 
% (Class 1, Class 2) : (P=Common Place,T=Common Time)
conflict(C1,C2) :- where(C1,P), where(C2,P), when(C1,T),when(C2,T), C1\=C2.

%% Meet Predicate to tell when students might meet during or in between classes.
%(Student 1, Student 2) : (P=Common Class, T1,T2 = Times of Classes for Students. Can be the same, or back to back)
meet(S1,S2) :- schedule(S1,P,T1),schedule(S2,P,T2),S1\=S2,(T1=:=T2;T1=:=T2+1;T1=:=T2-1).

/* Part 2: List Predicates and Arithmetic */

%% rdup predicate to remove duplicates from an ordered list. 
% (Ordered List) : (M = List without duplicates)

% Base Case (Both Empty)
rdup([],[]).

% Base Case (Only one element)
rdup([X],[X]).

% If X == next, continue through list and don't add
rdup([X, X|XS],M) :- rdup([X|XS],M).

% If X != next, add X to list.
rdup([Y, X|XS],[Y|M]) :- X\=Y,rdup([X|XS],M).


%% Flat Predicate to combines all sublists into one.
% (List, Flattened List) : (X=Head,XS=Rest of list, FX=Flattened X, FXS=Flattened XS)

% Base Case (Both Empty)
flat([],[]).

% If X is not a list, append X to F and continue.
flat([X|XS],[X|F]) :- \+ is_list(X),flat(XS,F).

% If X is a list, flatten X first, and then append it with the flattened XS and get F
flat([X|XS],F) :- is_list(X),flat(X,FX),flat(XS,FXS),append(FX,FXS,F).

%% Project predicate to select elements from a list based on position.

% Helper to decrement by 1
sub_one(X, Y) :- Y is X-1.

% Base Case (No Indices)
project([],_,[]).

% Base Case (Indices out of bounds, return what is collected already)
project(_,[],[]).

% If current index is 1, add X to result, decrement remaining indices, and continue on with next list item.
project([I|IS],[X|XS],[X|R]) :- I=1,maplist(sub_one,IS, IM1),project(IM1,XS,R).

% If current index is not 1, decrement all indices, and continue on with next list item.
project([I|IS],[_|XS],R) :- I\=1,maplist(sub_one,[I|IS], IM1),project(IM1,XS,R).

