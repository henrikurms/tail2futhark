
a ← 2 * 3

T1 ← 8=a   ⍝ --> [](8.0)

test ← { 0.000001>|⍺-⍵ }

a ← * 3

T2 ← 20.085536 test a       ⍝ --> e^3 =~= 20.085536

∧/ T1 T2