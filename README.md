# Overview

### Formulate Queries (10%)
```pl
age(5). age(10). age(15).

    % ?- age(A), not((age(B), A < B)).
    % => A = 15
```

### Structures (8%)
#### Binary Tree
```pl
tree(0, 
    tree2(a,
        2, tree2(b, 0, none, 0, none),
        7, tree2(c,
            3, tree2(d, 0, none, 0, none),
            8, tree2(e, 0, none, 0, none)
        )
    )
).

traverse(tree2(S, 0, none, 0, none)).
traverse(tree2(S, AVal, A, 0, none)) :- traverse(A), write(AVal), nl.
traverse(tree2(S, 0, none, BVal, B)) :- traverse(B), write(BVal), nl.
traverse(tree2(S, AVal, A, BVal, B)) :-
    traverse(tree2(S, AVal, A, 0, none)),
    traverse(tree2(S, 0, none, BVal, B)).

    % ?- tree(0, T), traverse(T). % THIS IS FOR INORDER TRAVERSAL !
    % => 2, 3, 7, 8
    %
    %       /\
    %      2  7
    %        /\
    %       3  8
```
#### Linked List
```pl
list(0, node(3, node(11, node(5, nil)))).

sumNodes(nil, 0).
sumNodes(node(H, T), N) :- sumNodes(T, R), N is R + H.
    % ?- list(0, L), sumNodes(L, S).
    % => S = 19
```

### Recursion Over Lists and Terms (15%)
```pl
append([], L2, L2).
append([H|T], L2, [H|R]) :- append(T, L2, R).

member(E, [E|_]).
member(E, [_|T]) :- member(E, T).

length([], 0).
length([_|T], N) :- length(T, R), N is R + 1.

sum([], 0).
sum([H|T], N) :- sum(T, R), N is R + H.
```
```pl
all_diff([]).
all_diff([N|L]) :- not(member(N,L)), all_diff(L).

replaceOne(_, _, [], []).
replaceOne(X, Y, [X|T], [Y|L]) :- replaceOne(X, X, T, L).
replaceOne(X, Y, [H|T], [H|L]) :- not(X = H), replaceOne(X, Y, T, L).

replaceAll(_, _, [], []).
replaceAll(X, Y, [X|T], [Y|L]) :- replaceAll(X, Y, T, L).
replaceAll(X, Y, [H|T], [H|L]) :- not(X = H), replaceAll(X, Y, T, L).

reversePosNeg([], [], []).
reversePosNeg([H|T], P, R) :- H < 0, reversePosNeg(T, P, N), append(N, [H], R).
reversePosNeg([H|T], R, N) :- H >= 0, reversePosNeg(T, P, N), append(P, [H], R).
```
#### Blocks World
```pl
on(b1,b2).
on(b3,b4).
on(b4,b5).
on(b5,b6).
just_left(b2,b6).
just_left(b6,b7).
above(X,Y) :- on(X,Y).
above(X,Y) :- on(X,Z), above(Z,Y).
left(X,Y) :- above(X,Z), left(Z,Y). % X is high
left(X,Y) :- above(Y,Z), left(X,Z). % X is low
left(X,Y) :- just_left(X,Y).
left(X,Y) :- just_left(X,Z), left(Z,Y).
right(X,Y) :- left(Y,X).

    % ?- above(b1,b6).
    % => false
```

### Constraints / CSP (12%)
```pl
% Example: SEND + MORE = MONEY

dig(0). dig(1). dig(2). dig(3). dig(4).
dig(5). dig(6). dig(7). dig(8). dig(9).
```
#### Pure Generate and Test
```pl
solve([S,E,N,D,M,O,R,Y]) :-
    dig(S), dig(E), dig(N), dig(D),
    dig(M), dig(O), dig(R), dig(Y),
    S > 0, M > 0,
    Y is (D+E) mod 10, C1 is (D+E) // 10,
    E is (N+R+C1) mod 10, C10 is (N+R+C1) // 10,
    N is (E+O+C10) mod 10, C100 is (E+O+C10) // 10,
    O is (S+M+C100) mod 10, M is (S+M+C100) // 10,
    all_diff([S,E,N,D,M,O,R,Y]).
    % ?- solve(L).
    % => 167 minutes of computing time ...
```
#### Interleaving of Generate and Test
```pl
solveBest([S,E,N,D,M,O,R,Y]) :-
    dig(D), dig(E),
    Y is (D+E) mod 10, C1 is (D+E) // 10,
    dig(N), dig(R),
    E is (N+R+C1) mod 10, C10 is (N+R+C1) // 10,
    dig(E), dig(O),
    N is (E+O+C10) mod 10, C100 is (E+O+C10) // 10,
    dig(S), S > 0, dig(M),
    O is (S+M+C100) mod 10, M is (S+M+C100) // 10,
    M > 0,
    all_diff([S,E,N,D,M,O,R,Y]).
    % ?- solveBest(L).
    % => L = [9, 5, 6, 7, 1, 0, 8, 2]
```

### Natural Language Processing (20%)
#### Concepts
- **Database:** stores facts, defines model of world
- **Lexicon:** specifies vocabulary and words of interest
- **Parser:** grammatical rules to form statements (uses a *Grammar*)
#### Database
```pl
person(john). job(john, doctor). size(john, small).
person(grace). job(grace, doctor). size(grace, big).
person(mark). job(mark, programmer). size(mark, small).
person(linda). job(linda, programmer). size(linda, small).

park(qbeach). park(queens_park).
tree(tree01). size(tree01, big).
tree(tree02). size(tree02, small).
tree(tree03). Size(tree03, small).
...
```
#### Lexicon
```pl
article(a).
article(the).

common_noun(park, X) :- park(X).
common_noun(tree, X) :- tree(X).
common_noun(hat, X) :- hat(X).
common_noun(programmer, X) :- job(X, programmer).
common_noun(doctor, X) :- job(X, doctor).
common_noun(doc, X) :- job(X, doctor).
common_noun(doctor, X) :- hasPhD(X).
common_noun(medical_doctor, X) :- job(X, doctor), hasMD(X).

proper_noun(Noun) :- person(Noun).

adjective(big, X) :- size(X, big).
adjective(small, X) :- size(X, small).
adjective(red, X) :- colour(X, red).
adjective(blue, X) :- colour(X, blue).

preposition(on, X, Y) :- on(X, Y).
preposition(in, X, Y) :- in(X, Y).
preposition(beside, X, Y) :- beside(X, Y).
preposition(beside, X, Y) :- beside(Y, X).

preposition(with, X, Y) :- on(X, Y).
preposition(with, X, Y) :- in(X, Y).
preposition(with, X, Y) :- beside(X, Y).
preposition(with, X, Y) :- beside(Y, X).
```
#### Grammar
```
1   S -> NP VP
2   VP -> copula_verb Mods
3   VP -> transitive verb NP Mods
4   VP -> intransitive verb Mods
5   NP -> proper_noun
6   NP -> article NP2
7   NP2 -> adjective NP2
8   NP2 -> common_noun Mods
9   Mods -> []
10  Mods -> PP Mods
11  PP -> preposition NP
```
#### Parser
```pl
% How we are going to ask Qs (driver)
who(Words, Ref) :- np(Words, Ref).

% NP -> proper_noun
np([Name], Name) :- proper_noun(Name).

% NP -> article NP2
np([Art | Rest], Who) :- article(Art), np2(Rest, Who).

% NP2 -> adjective NP2
np2([Adj | Rest], Who) :- adjective(Adj, Who), np2(Rest, Who). 

% NP2 -> common_noun Mods
Np2([Noun | Rest], Who) :- common_noun(Adj, Who), mods(Rest, Who). 

% Mods -> []
mods([], Who). 

% Mods -> PP Mods
mods(Words, Who) :- append(Start, End, Words), 
pp(Start, Who), mods(End, Who).

% PP -> preposition NP
pp([Prep | Rest], Who) :- preposition(Prep, Who, Who2), np(Rest, Who2). 
```
#### Querying
Normal Query
```pl
?- who([a, doctor, with, a, big, hat], X).
X = grace
```
Competion & Generation
```pl
?- who([the, W, on, john], hat01).
W = hat

?- who(L, linda).
L = linda ;
Execution gets stuck in recursion constructing "a small small small ..."

?- L = [a, _, _, _, _], who(L, linda).
L = [a, small, small, small, programmer]
L = [a, small, programmer, in, queens_park]
L = [a, small, programmer, with, mark]
...

```
Interrogative Sentences
```pl
WH -> wh_word copula_verb NP    % What is X?
WH -> wh_word copula_verb PP    % Who is in X?

```
Yes/No Questions
```pl
YN -> copula_verb NP NP    % Is X a Y?
YN -> copula_verb NP PP    % Is X with Y?
```

### Short Essay General Question (5%)

N/A

### Problem Solving / Planning (25%)

### Bayesian Networks (15%)
