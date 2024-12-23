# battleship

This is an automated solver from the game battleship.
It tries to find the most probable cell that contains battleship
by constructing a number of boards that fit the resulting data (Hit, HitSink or Miss on cells).

Right now it works properly only on a small boards (7 x 7) with no more than 5-6 large ships.

To solve run:
```
cabal run exe:battleship
```
