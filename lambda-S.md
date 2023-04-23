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

v ::= n | \lang E,\lambda x.e \rang \\
$$

## Semantics

$$
\\
% rule for val
v \Downarrow_E v (E-val) \\

% rule for var
v \Downarrow_E E(x) (E-var) \\

\lambda x.e \Downarrow_E \lang E, \lambda x.e \rang (E-clos) \\

\\
% rule for application
\frac{
   e_f \Downarrow_E \lang E_b, \lambda x.e_b \rang \hspace{1cm}
   e_a \Downarrow_E v_a \hspace{1cm}
   e_b \Downarrow_{E_b[x \mapsto v_a]} v_b
}
{
   (e_f \ e_a) \Downarrow_E v_b
}(E-app) \\
$$



# $\lambda_F$

## Syntax

$$
v ::= n | \lang E,\lambda x.t \rang | void \\

e ::= v | x | (e_1\ e_2) | \lambda x.t \\

t ::= e | t;t | (define\ x\ e) \\
$$



### Semantics

$$
\\
% rule for val
v \Downarrow_E v \ (E-val) \\

% rule for var
v \Downarrow_E E(x) \ (E-var) \\

\lambda x.t \Downarrow_E \lang E, \lambda x.t \rang \ (E-lam) \\

\frac {
  e \Downarrow_E v
}{
  e \Downarrow_E \lang E, v \rang
}\ (E-exp) \\

% rule for application
\frac{
   e_f \Downarrow_E \lang E_b, \lambda x.t_b \rang \hspace{1cm}
   e_a \Downarrow_E v_a \hspace{1cm}
   e_b \Downarrow_{E_b[x \mapsto v_a]} v_b
}
{
   (e_f \ e_a) \Downarrow_E v_b
}(E-app) \\

\frac {
  e \Downarrow_E v
}{
  (define\ x\ e) \Downarrow_E \lang E[x \mapsto v], void \rang
}\ (E-def) \\

\frac {
  t1 \Downarrow_{E_1} \lang E_2, v_1 \rang \hspace{1cm}
  t2 \Downarrow_{E_2} \lang E_3, v_2 \rang
}{
  t1;t2 \Downarrow_{E_1} \lang E_3, v_2 \rang
}\ (E-seq) \\
$$

# $\lambda_D$

## Syntax

$$
v ::= n | \lang E,\lambda x.t \rang | void \\

e ::= v | x | (e_1\ e_2) | \lambda x.t \\

t ::= e | t;t | (define\ x\ e) \\
$$



## Semantics

$$
\\
% rule for val
v \Downarrow_E v \ (E-val) \\

% rule for var
x \Downarrow_E E(x) \ (E-var) \\

\lambda x.t \Downarrow_E \lang E, \lambda x.t \rang \ (E-lam) \\

\frac {
  e \Downarrow_E v
}{
  e \Downarrow_E \lang E, v \rang
}\ (E-exp) \\

% rule for application
\frac{
   e_f \Downarrow_{E} \lang E_f, \lambda x.t_b \rang \ \triangleright \ 
   e_a \Downarrow_E v_a \ \triangleright \ 
   E_b \leftarrow E_f + [x := v_a] \ \triangleright \ 
   t_b \Downarrow_{E_b} v_b
}
{
   (e_f \ e_a) \Downarrow_E v_b
}(E-app) \\

\frac {
  e \Downarrow_E v \hspace{0.5cm} \triangleright \hspace{0.5cm} 
  E \leftarrow [x := v]
}{
  (define\ x\ e) \Downarrow_E void
}\ (E-def) \\

\frac {
  t1 \Downarrow_{E} v_1 \hspace{0.5cm} \triangleright \hspace{0.5cm}
  t2 \Downarrow_{E}  v_2
}{
  t1;t2 \Downarrow_{E} v_2
}\ (E-seq) \\
$$

### Put

Take a ref to an env $E$ and mutate its contents, by adding a new binding.
$$
E \leftarrow [x := v]
$$

### Push

Create a new env ref by $E$ which copies the elements of $E'$ and also adds a new binding.
$$
E \leftarrow E' + [x := v]
$$


(define hp-env (d:builtin-env))

(define hp (eff-state hp-env))

(define env (eff-res hp-env))

(d:eval-all (list (d:define 'f (d:value 10)) (d:variable 'f)) env hp)
