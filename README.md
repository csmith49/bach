# Bach

Use ```make``` to build. Result is sym-linked as ```bach.native```.

Benchmark file structure is as follows:

```
./benchmarks
    /finitefield
        /facts
            add.facts
            mult.facts
            ...
        finitefield.sexp
    /dict
        /facts
            insert.facts
            keys.facts
            ...
        dict.sexp
    ...    
```
 In particular, the ```facts/``` folder contains lines of tab-separated (this is critical) items in the relevant domain encoding the execution,
 and ```test.sexp``` contains the parameters that define the search.
