---
status: Still in research (please don't share)
---

# Modular state machines

## Motivation

* One big state machine can become difficult to understand

* How can we break a state machine into parts such that when combined they form
  the whole?

## Products of states

* The Cartesian product of states can be used when we need to keep track of two
  states running in parallel.

* There are different two different ways we can advance two state machines that
  run in parallel, either by stepping one of them and leaving the other alone or
  stepping both of them in lockstep.

* Angelic product, allows of stepping one of the state machines:

```
type SM s i o = i -> s -> (s, o)

angelic : SM s i o -> SM t j p -> SM (s, t) (i + j) (o + p)
angelic f g ij (s, t) case ij of
  Left i  -> let (s', o) = f i s in ((s', t), Left o)
  Right j -> let (t', p) = g j t in ((s, t'), Right p)

video : SM {playing, stopped} {stop, play} ()
audio : SM {mutued, unmuted} {mute, unmute} ()

player = angelic video audio
```

* Tensor product, allows us to step both state machines in lockstep:

```
tensor : SM s i o -> SM t j p -> SM (s, t) (i, j) (o, p)
tensor f g (i, j) (s, t) =
  let
    (o, s') = f i s
    (p, t') = g j t
  in
    ((s', t'), (o, p))
```

See also:

* ["Concurrent state
  machines"](http://gameprogrammingpatterns.com/state.html#concurrent-state-machines)
  in Nystrom;
* [Orthogonal
  regions](https://en.wikipedia.org/wiki/UML_state_machine#Orthogonal_regions)
  in UML state machines;
* [Programming interfaces and basic topology](https://arxiv.org/abs/0905.4063)
  by Peter Hancock and Pierre Hyvernat (2009).

## The state pattern

* http://gameprogrammingpatterns.com/state.html#the-state-pattern

## Hierarchical states

* http://gameprogrammingpatterns.com/state.html#hierarchical-state-machines

* [Hierarchically nested
  states](https://en.wikipedia.org/wiki/UML_state_machine#Hierarchically_nested_states)
  in UML state machines

## Stack of states / pushdown automaton

* http://gameprogrammingpatterns.com/state.html#pushdown-automata

## See also

* [Game Programming Patterns](http://gameprogrammingpatterns.com/state.html) by
  Robert Nystrom (2014, chapter 7);
* Development and Deployment of Multiplayer Online Games, Vol. II by Sergey
  Ignatchenko (2020, chapter 5);
* [*Statecharts: A visual formalism for complex
  systems*](http://www.wisdom.weizmann.ac.il/~dharel/SCANNED.PAPERS/Statecharts.pdf)
  by David Harel (1987);
* [UML state machines](https://en.wikipedia.org/wiki/UML_state_machine).
