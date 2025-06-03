fori=1to10:?:?#3,i:forj=1toi:?#4,j,:.j:.i

fori=1to10:@(i)=20-i:.i:fori=1to10:?i,@(i):.i: rem Array is "@"

0rem---
10 fori=1to10
20  ?:?#3,i
30  forj=1toi
40    gos.100
50  .j:.i
60 stop
100 ?#4,j,
110 ret.

0rem----
10 rem LED長いピン(A)をCN1の1、短いピン(K)を3に挿す。 PPI-0 port B-6
100 out $33,$80 :rem PPI-0 All ports output
110 out $31,$00 :rem out $00 -> PPI0-B
120 fori=1to2000:.i
130 out $31,$40 :rem out $40 -> PPI0-B
140 fori=1to2000:.i
150 g.110
0rem----
