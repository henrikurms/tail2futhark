out ← {
  A ← ⍺
  B ← ⍵
  T ← (⊃(⍴⍴B))⌽⍳⊃⍴(⍴B),⍴A
⍝   T ← (⊃(⍴⍴B))⌽⍳⊃(⍴⍴B)+⍴⍴A
  x ← T ⍉ ((⍴B),(⍴A)) ⍴ A
  y ← ((⍴A),(⍴B)) ⍴ B
  z ← x ⍺⍺ y
}

res ← (⍳ 10) × out (⍳ 10)

+/+/res         ⍝ → [0](3025)