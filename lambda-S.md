# $\lambda_S$

## Syntax

$$
e ::= v | x | (e_1 \ e_2) \\

v ::= n | \lambda x.e \\
$$



## Semantics

$$
\\
% rule for val
v \Downarrow v (E-val) \\

\\
% rule for application
\frac{
   e_f \Downarrow \lambda x.e_b \hspace{1cm}
   e_a \Downarrow v_a \hspace{1cm}
   e_b[x \mapsto v_a] \Downarrow v_b
}
{
   (e_f \ e_a) \Downarrow v_b
}(E-app) \\

\\
% rules for variable
% n stands for number
% x y stands for variable
n[x \mapsto v] = n \\

x[x \mapsto v] = v \\

y[x \mapsto v] = y \hspace{1cm} if\ x \neq y \\

% if a function declaration has a arg named x
% and because of scope, we can not replace the x in the function body
% so we can only get the function daclartion itself
\lambda x.e[x \mapsto v] = \lambda x.e \\

% if a function declaration doesn't contain any arg named x
% then we need to replace all x in the function body
\lambda y.e[x \mapsto v] = \lambda y.e[x \mapsto v] \hspace{1cm} if\ x \neq y \\

(e_1\ e_2)[x \mapsto v] = (e_1[x \mapsto v]\ e2[x \mapsto v]) \\
$$



# $\lambda_E$

Decreasing the run time of substitution

-   Use a lookup-table to bookkeep the variable bindings
-   Introduce closures / environments

introduce the evaluation of expressions down to values, parameterized by environment:
$$
% e stands for expression
% E stands for environment
e \Downarrow_E v
$$

-   Evaluation $e \Downarrow_E v$ is implemented as function `(e:eval env exp)` that returns a value `e:value` , an environment `env` is a hash table, and expression `exp` is an `e:expression`
-   functions and structs prefixed with `s:` correspond to the $\lambda_S$ language
-   functions and structs prefixed with `e:` correspond to the $\lambda_E$ language

## Syntax

$$
e ::= v | x | (e_1\ e_2) | \lambda x.e \\

v ::= n | (E,\lambda x.e) \\
$$

## Semantics

$$
\\
% rule for val
v \Downarrow_E v (E-val) \\

% rule for var
v \Downarrow_E E(x) (E-var) \\

\lambda x.e \Downarrow_E (E, \lambda x.e) (E-clos) \\

\\
% rule for application
\frac{
   e_f \Downarrow_E (E_b, \lambda x.e_b) \hspace{1cm}
   e_a \Downarrow_E v_a \hspace{1cm}
   e_b \Downarrow_{E_b[x \mapsto v_a]} v_b
}
{
   (e_f \ e_a) \Downarrow_E v_b
}(E-app) \\
$$



$$
\mapsto \\

\Downarrow \\

\frac{a}{b} \\
$$
