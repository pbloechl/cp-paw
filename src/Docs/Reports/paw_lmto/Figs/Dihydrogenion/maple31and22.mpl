
g := proc (x) options operator, arrow, function_assign; 1-(x+1)*exp(-2*x) end proc;
y := proc (n, x) options operator, arrow, function_assign; -(1+x*n)*exp(-x*n)/n^2 end proc;
h := proc (n, z, x) options operator, arrow, function_assign; g(x)*exp(-(2-n)*x)*(y(n, x+z)-y(n, z-x)) end proc;
k := proc (n, z, x) options operator, arrow, function_assign; g(x)*exp(-(2-n)*x)*(y(n, x+z)-y(n, x-z)) end proc;
q := proc (n, z) options operator, arrow, function_assign; -2*(int(h(n, z, x), x = 0 .. z)+int(k(n, z, x), x = z .. infinity))/z end proc;

int(h(2, z, x), x = 0 .. z);
limit( , z = infinity );
plot((1-(a+1)*exp(-2*a))/a);
smartplot();
int(k(2, z, x), x = z .. infinity);
2*(int(h(1, z, x), x = 0 .. z)+int(k(1, z, x), x = z .. infinity))/z;
# (->);
simplify(2*((1/32*(16*z^2*exp(4*z)+2*z*exp(4*z)-19*exp(4*z)+40*exp(2*z)*z+32*exp(2*z)-16*z^2-30*z-13))*exp(-5*z)+(1/32*(24*exp(4*z)-42*exp(2*z)*z-37*exp(2*z)+16*z^2+30*z+13))*exp(-5*z))/z, size);




2*(int(h(2, z, x), x = 0 .. z)+int(k(2, z, x), x = z .. infinity))/z;
# (->);
simplify(2*(-(1/96*(8*z^3*exp(4*z)-24*exp(6*z)+36*z^2*exp(4*z)+57*z*exp(4*z)+36*exp(4*z)-48*exp(2*z)*z-24*exp(2*z)+24*z^2+39*z+12))*exp(-6*z)-(1/32*(-8*exp(6*z)+3*z*exp(4*z)+4*exp(4*z)+16*exp(2*z)*z+8*exp(2*z)-8*z^2-13*z-4))*exp(-6*z))/z, size);

