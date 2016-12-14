cycle-roll
==========

a haskell implementation of an algorithm created by Evgeny Kluev to
detect cycles in data. see 
[this SO question](http://stackoverflow.com/questions/19359259/explanation-for-the-loop-rolling-algorithm)
for more details.

## usage 

```
$> echo 'abcabababcababcefefefghefgh' | .cabal-sandbox/bin/cycle-roll
("abc", "ab"*2)*2, "c", "ef"*2, "efgh"*2, "\n\NUL"
```
