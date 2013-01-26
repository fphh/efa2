
% Referenzimplementierung für die Differenzenrechnung
% Getestet mit SWI-Prolog

diff(e(X), de(X)).
diff(p(X), dp(X)).
diff(n(X), dn(X)).
diff(x(X), dx(X)).
diff(dt(X), dt(X)).
diff(F + G, DF + DG) :- diff(F, DF), diff(G, DG).
diff(F - G, DF - DG) :- diff(F, DF), diff(G, DG).

diff(P * H, (DP * H) + (P * DH) + (DP * DH))
  :- diff(P, DP), diff(H, DH).

diff(P / H, (DP / H) + (P * DH / (DH * H + H*H)) + (DP * DH / (DH * H + H*H)))
  :- diff(P, DP), diff(H, DH).


% Test e_0.0_0.2 entnommen aus aus example/elementary/symbolicSolven:

% diff((p(3.2)*n(3.2)*dt(0)/x(2.3) - p(3.2)*n(3.2)*dt(0)) / n(0.2), Y).

% Antwort:
% Y = (((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0))/x(2.3)+p(3.2)*n(3.2)*dt(0)*dx(2.3)/ (dx(2.3)*x(2.3)+x(2.3)*x(2.3))+ ((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0))*dx(2.3)/ (dx(2.3)*x(2.3)+x(2.3)*x(2.3))- ((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)))/n(0.2)+ (p(3.2)*n(3.2)*dt(0)/x(2.3)-p(3.2)*n(3.2)*dt(0))*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2))+ (((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0))/x(2.3)+p(3.2)*n(3.2)*dt(0)*dx(2.3)/ (dx(2.3)*x(2.3)+x(2.3)*x(2.3))+ ((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0))*dx(2.3)/ (dx(2.3)*x(2.3)+x(2.3)*x(2.3))- ((dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)+p(3.2)*n(3.2)*dt(0)+ (dp(3.2)*n(3.2)+p(3.2)*dn(3.2)+dp(3.2)*dn(3.2))*dt(0)))*dn(0.2)/ (dn(0.2)*n(0.2)+n(0.2)*n(0.2)) ;
