# Armstrong distributed systems

How do we build reliable, scalable and maintainable computer systems?

This repository contains notes on how, I think, we can improve on the state of
development, documentation, testing, deployment, observability, debugging, and
upgrading of distributed systems. Most of the ideas are stolen from others, many
from Erlang and Joe Armstrong. Over time I hope to turn this into a more
coherent text, for now think of it a crude blog or some basic scaffolding for me
to hang my thoughts on.

If any of this interests you as well, please do get in touch -- one of the
reasons I'm writing this down is to find people to collaborate with.

## Development

* [Erlang's not about lightweight processes and message
  passing...](docs/erlang-is-not-about.md)

* [Implementing
  behaviours](https://github.com/stevana/armstrong-distributed-systems/blob/implementing-behaviours/docs/implementing-behaviours.md)

* [Modular state
  machines](https://github.com/stevana/armstrong-distributed-systems/blob/modular-state-machines/docs/modular-state-machines.md)

* Implementing `gen_event` using the LMAX disruptor
  - https://github.com/stevana/pipelined-state-machines
  - Shard on: "people, stuff or deals",
    [says](https://youtu.be/1KRYH75wgy4?t=2781) Martin Thompson.

* [State machines with of async I/O](https://github.com/stevana/coroutine-state-machines)

* Persisted/`mmap`ped lock-free concurrent data structures
  - [Working with binary data](https://github.com/stevana/bits-and-bobs)
  - bytebuffer
  - journal
  - hashmap
  - arena allocator
  - buffered actions
  - [Efficient Tree-Traversals: Reconciling Parallelism and
    Dense](https://arxiv.org/pdf/2107.00522.pdf)

* [On the role of practice in
  programming](https://github.com/stevana/armstrong-distributed-systems/blob/practice-in-programming/docs/practice-in-programming.md)

## Documentation

* Specification language
  - Interfaces
  - Messages
  - Compression
  - Protocols
  - Usage model or operational profile

* Joe's idea of ["the bigger picture"](https://youtu.be/h8nmzPh5Npg?t=1220) in
  particular the "research" part, not just the end result (code) but how you got
  to the result.

## Testing

* [Simulation testing using state
  machines](https://github.com/stevana/property-based-testing-stateful-systems-tutorial)

* Usage model or operational profile

* Load/soak testing

## Deployment

* [Deploying and restarting state machines using supervisor
  trees](https://github.com/stevana/supervised-state-machines)

* Elasticity and scalability
  - https://www.youtube.com/watch?v=BOKqGPWXwk0
  - [SEDA: An Architecture for Well-Conditioned Scalable Internet
    Services](https://people.eecs.berkeley.edu/~brewer/papers/SEDA-sosp.pdf)
  - https://people.eecs.berkeley.edu/~brewer/papers/SEDA-sosp.pdf
  - Did something else supersede SEDA yet?

## Observability

* Logs
* Metrics
* Traces

## Debugging

* Motivation:
  - John Carmack on using
    [debuggers](https://www.youtube.com/watch?v=tzr7hRXcwkw);
  - Martin Thompson also [says](https://youtu.be/1KRYH75wgy4?t=411) "step through
    your code using a debugger".
* Idea: Record inputs (and states) in circular buffer, dump to disk/SQLite db on error

* The history of [time traveling
  debuggers](http://jakob.engbloms.se/archives/1564)
* [Tomorrow Corporation Tech Demo](https://www.youtube.com/watch?v=72y2EC5fkcE)

## Upgrading

* [Hot-code swapping ?? la Erlang with `Arrow`-based state
  machines](https://github.com/stevana/hot-swapping-state-machines)

* Version everything
