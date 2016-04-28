⍝ Power allows us to calculate a Fibonacci series 
fibo ← {
  a ← ((⊃⍴⍵)⍴⍵)
  a,+/¯2↑a
}

fib ← {(fibo⍣⍵) 0 1}

list ← fib 10

+/ list  ⍝ 232