# $\mu - JavaScript$

## Syntax

$$
p ::= x = e; | while (e)\{ p \} | console.log(e); | p\ p
$$

## Semantics

$$
(m, p) \Downarrow m' (E-prog)\\

\frac{
  e \Downarrow_m v
}{
  (m, x = e) \Downarrow m[x \mapsto v]
} (E-assign)\\

\frac{
  e \Downarrow_m v \hspace{1cm} log(v) 
}{
  (m, console.log(e);) \Downarrow m
} (E-log)\\

\frac{
  (m_1, p_1) \Downarrow_{m_1} m2 \hspace{1cm}
  (m_2, p_2) \Downarrow_{m_2} m3
}{
  (m_1, p_1\ p_2) \Downarrow m_3
} (E-seq)\\

\frac{
  e \Downarrow_m v \hspace{1cm} v = 0 \hspace{1cm} m
}{
  (m, while(e) \{ p \}) \Downarrow m
} (E-while)\\

\frac{
  e \Downarrow_m v \hspace{0.5cm} v \neq 0 \hspace{0.5cm}
  (m, p\ while (e)\{ p \}) \Downarrow m'
}{
  (m, while(e) \{ p \}) \Downarrow m'
} (E-while)\\
$$

