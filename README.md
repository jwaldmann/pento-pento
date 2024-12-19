We solve the following

* The problem consists in taking two polyominoes, say a tetromino and a pentamino,
  and finding a figure which can be tessellated indipendently by both the polyominoes.

(see <https://web.archive.org/web/20170519200654/http://www.iread.it/Poly/P55.html>)

via SAT encoding (using Satchmo <http://hackage.haskell.org/package/satchmo>
and Minisat <http://www.minisat.se/> <http://hackage.haskell.org/package/minisat>)

Build/Run

```
cabal run --allow-newer -w /opt/ghc/ghc-9.4.7/bin/ghc pento-pento LX 20
```

finds the LX solution in 45 sec on my machine.
The CNF has 41308 variables and 126381 clauses.

(Newer GHC would need changes in `satchmo`.)