
% Referenzimplementierung für die Differenzenrechnung
% Getestet mit SWI-Prolog

is_variable(e(_)).
is_variable(de(_)).
is_variable(p(_)).
is_variable(dp(_)).
is_variable(n(_)).
is_variable(dn(_)).
is_variable(x(_)).
is_variable(dx(_)).
is_variable(dt(_)).

is_variable(a).
is_variable(da).
is_variable(b).
is_variable(db).
is_variable(c).
is_variable(dc).
is_variable(d).
is_variable(dd).
is_variable(x).
is_variable(dx).
is_variable(y).
is_variable(dy).
is_variable(z).
is_variable(dz).


diff(e(X), de(X)).
diff(p(X), dp(X)).
diff(n(X), dn(X)).
diff(x(X), dx(X)).
diff(dt(X), dt(X)).

diff(a, da).
diff(b, db).
diff(c, dc).
diff(x, dx).
diff(y, dy).
diff(z, dz).


diff(F + G, DF + DG) :- diff(F, DF), diff(G, DG).
diff(F - G, DF - DG) :- diff(F, DF), diff(G, DG).

diff(P * H, (DP * H) + (P * DH) + (DP * DH))
  :- diff(P, DP), diff(H, DH).

diff(P / H, (DP / H) - (P * DH / (DH * H + H*H)) - (DP * DH / (DH * H + H*H)))
  :- diff(P, DP), diff(H, DH).


only_factors(X) :- is_variable(X).
only_factors(_ + _) :- !, false.
only_factors(X * Y) :- only_factors(X), only_factors(Y).
only_factors(X / _) :- only_factors(X).


ass(X + _, X).

% Pushmult muss man bis zum fix-punkt iterieren.
push_mult(X, X) :- only_factors(X), !.

push_mult(X + Y, X1 + Y1)
  :- !, push_mult(X, X1), push_mult(Y, Y1).
push_mult(X - Y, X1 - Y1)
  :- !, push_mult(X, X1), push_mult(Y, Y1).

push_mult(X * (Y + Z), Y1 + Z1)
  :- !, push_mult(X*Y, Y1), push_mult(X*Z, Z1).
push_mult(X * (Y - Z), Y1 - Z1)
  :- !, push_mult(X*Y, Y1), push_mult(X*Z, Z1).
push_mult((X + Y) * Z, X1 + Y1)
  :- !, push_mult(X*Z, X1), push_mult(Y*Z, Y1).
push_mult((X - Y) * Z, X1 - Y1)
  :- !, push_mult(X*Z, X1), push_mult(Y*Z, Y1).
push_mult(X * Y, U)
  :- !, push_mult(X, X1), push_mult(Y, Y1), push_mult(X1*Y1, U).

push_mult(X / Y, U+X2/Y)
  :- push_mult(X, X1 + X2), !, push_mult(X1/Y, U).

push_mult(X, X) :- !.

split_terms(X + Y, Z)
  :- !, split_terms(X, X1), split_terms(Y, Y1), append(X1, Y1, Z).
split_terms(X - Y, Z)
  :- !, split_terms(X, X1), split_terms(Y, Y1), append(X1, Y1, Z).
split_terms(X, [X]).

additive_terms(X, Y)
  :- !, diff(X, U),
	push_mult(U, V),
	split_terms(V, Y).


% Test e_0.0_0.2 entnommen aus aus example/elementary/symbolicSolven:

% ?- additive_terms(p(3.2)*n(3.2)/(x(2.3)*n(0.2)) - p(3.2)*n(3.2)/ n(0.2), Y), length(Y, N).

% Y = [dp(3.2)*n(3.2)/ (x(2.3)*n(0.2)), p(3.2)*dn(3.2)/ (x(2.3)*n(0.2)), dp(3.2)*dn(3.2)/ (x(2.3)*n(0.2)), p(3.2)*n(3.2)* (dx(2.3)*n(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), p(3.2)*n(3.2)* (x(2.3)*dn(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), p(3.2)*n(3.2)* (dx(2.3)*dn(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), dp(3.2)*n(3.2)* (dx(2.3)*n(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), p(3.2)*dn(3.2)* (dx(2.3)*n(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), dp(3.2)*dn(3.2)* (dx(2.3)*n(0.2))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), (dp(3.2)*n(3.2)* (x(2.3)*dn(0.2))+p(3.2)*dn(3.2)* (x(2.3)*dn(0.2))+dp(3.2)*dn(3.2)* (x(2.3)*dn(0.2)))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), (dp(3.2)*n(3.2)* (dx(2.3)*dn(0.2))+p(3.2)*dn(3.2)* (dx(2.3)*dn(0.2))+dp(3.2)*dn(3.2)* (dx(2.3)*dn(0.2)))/ ((dx(2.3)*n(0.2)+x(2.3)*dn(0.2)+dx(2.3)*dn(0.2))* (x(2.3)*n(0.2))+x(2.3)*n(0.2)* (x(2.3)*n(0.2))), dp(3.2)*n(3.2)/n(0.2), p(3.2)*dn(3.2)/n(0.2), dp(3.2)*dn(3.2)/n(0.2), p(3.2)*n(3.2)*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2)), dp(3.2)*n(3.2)*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2)), p(3.2)*dn(3.2)*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2)), dp(3.2)*dn(3.2)*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2))],

% N = 18.
