---
status: research (please don't share)
---

# Domain-specific debuggers

## Motivation

* General purpose debuggers such as [`gdb`](https://www.sourceware.org/gdb/) are underused

* Theory: people who use general purpose debuggers also care about memory layout, e.g.:

  - John Carmack on using
  [debuggers](https://www.youtube.com/watch?v=tzr7hRXcwkw) and his old
  [.plan](https://github.com/oliverbenns/john-carmack-plan/blob/53c00aedaeeb23dee06b28ba985c3b3b4d61107c/archive/1998-10-14.md)
  (1998);

  - Martin Thompson also [says](https://youtu.be/1KRYH75wgy4?t=411) "step
    through your code using a debugger".

* Possible fix: domain-specific debuggers, that focus on displaying your
  application state at the level of abstraction that you think of it, rather
  than how the programming language that you are using happens to lay it out in
  memory

## Idea

* Assumptions: determinism, state machine
* Record inputs (and states) in circular buffer, dump to disk/SQLite db on error

## Examples

### Distributed systems

* TigerbeetleDB's demo https://youtu.be/w3WYdYyjek4?t=3175
* https://spritely.institute/news/introducing-a-distributed-debugger-for-goblins-with-time-travel.html

### Games

* [Tomorrow Corporation Tech Demo](https://www.youtube.com/watch?v=72y2EC5fkcE)

## Optimisations

* Avoid storing the state in the trace (if deterministic)
* Avoiding infinite traces via state snapshots
* Turn off logging (if deterministic)
* Audit trails

## Contributing

* General purpose formats for states, inputs and outputs against which generic
  debuggers can be written against? Structured JSON? Binary?

## See also

* The history of [time traveling
  debuggers](http://jakob.engbloms.se/archives/1564)
* https://werat.dev/blog/what-a-good-debugger-can-do/

* Mozilla's [`rr` debugger](https://github.com/rr-debugger/rr)

* [Visualising application state](https://youtu.be/-HhI3BEIqWw?t=1199)

* Command sourcing

* Jamie Brandon's post [*Local state is
  harmful*](https://www.scattered-thoughts.net/writing/local-state-is-harmful/)
  (2014)
