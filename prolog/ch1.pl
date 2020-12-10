  % predicate (argument, argument ...)

  likes(bharath, lisp).
  likes(mary, jane).
  likes(jane, john).
  likes(mary, wine).
  likes(steph, wine).

  % rule = head :- body
  % , for conjunction
  likes(john, X) :- female(X), likes(X, wine).

  valuable(gold).
  valuable(silver).
  owns(jane, gold).
  father(john, mary).
  father(john, jane).
  gives(john,book,mary).

  male(albert).
  male(edward).

  female(jane).
  female(mary).
  female(alice).
  female(victoria).

  parents(edward, victoria, albert).
  parents(alice, victoria, albert).
  parents(bob, jane, edward).

  sister_of(X, Y) :-
    female(X),
    parents(X, M, F),
    parents(Y, M, F).

  is_mother(X) :-
      female(X),
      parents(_, X, _).
  is_father(X) :-
      male(X),
      parents(_, _, X).
  is_son(X) :-
    male(X),
    parents(X, _, _).
  grandpa_of(X, Y) :-
    male(X),
    parents(C, _, X),
    parents(Y, C, _) ; parents(Y, _, C).
