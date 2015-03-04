We solve the following

* The problem consists in taking two polyominoes, say a tetromino and a pentamino,
  and finding a figure which can be tessellated indipendently by both the polyominoes.

(see <http://www.iread.it/Poly/index.php>)

via SAT encoding (using Satchmo <http://hackage.haskell.org/package/satchmo>
and Minisat <http://www.minisat.se/> <http://hackage.haskell.org/package/minisat>)

Build:

```
cabal install
```

Use:

```
pento-pento LX 20
```
finds the LX solution (see <http://www.iread.it/Poly/P55.html>) in 2 min 15 sec on my machine.
The CNF has 64132 variables and 217389 clauses.
