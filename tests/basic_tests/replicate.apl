a ← ⍳ 4

b ← a / a    ⍝ --> 1 2 2 3 3 3 4 4 4 4
T1 ← 30=+/ b         ⍝ --> 30


T2 ← ∧/'accdeee'=1 0 2 1 3 / 'abcde'   ⍝ 'accdeee'

T3 ← ∧/'acc  eee'=1 0 2 ¯2 3 / 'abcde'   ⍝ 'acc  eee'

T4 ← ∧/'aaaaa'=5/'a'

∧ / T1 T2 T3 T4
