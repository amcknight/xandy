# Changelog for xandy

### v0.0.1
* Can synthesize SKI combinators from lambda calulus, including S in ~20s
* Uses de Brujin indices to prevent checkingof isomorphic programs
* Considers lambda terms that take too long to evaluate to be counter-examples

### v0.0.0
* Can synthesize an Or gate from And and Not gates. Takes a whole minute.
* Can synthesize Lambda Calculus expressions. Learns K in 0.01s but S is taking over an hour... We'll see if it finds a non-terminating expression before S.