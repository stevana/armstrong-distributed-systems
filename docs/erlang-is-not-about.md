---
date: 2023-01-18
---

# Erlang's not about lightweight processes and message passing...

I used to think that the big idea of Erlang is its lightweight processes and
message passing. Over the last couple of years I've realised that there's a
bigger insight to be had, and in this post I'd like to share it with you.

## Background

Erlang has an interesting history. If I understand things correctly, it started
off as a Prolog library for building reliable distributed systems, morphed into a
Prolog dialect, before finally becoming a language in its own right.

The goal seemed to have always been to solve the problem of building reliable
distributed systems. It was developed at Ericsson and used to program their
telephone switches. This was sometime in the 80s and 90s, before internet use
become widespread. I suppose they were already dealing with "internet scale"
traffic, i.e. hundreds of millions of users, with stricter SLAs than most
internet services provide today. So in a sense they were ahead of their time.

In 1998 Ericsson decided to ban all use of Erlang[^0]. The people responsible
for developing it argued that if they were going to ban it, then they might as
well open source it. Which Ericsson did and shortly after most of the team that
created Erlang quit and started their own company.

One of these people was Joe Armstrong, which also was one of the main people
behind the design and implementation of Erlang. The company was called Bluetail
and they got bought up a couple of times but in the end Joe got fired in 2002.

Shortly after, still in 2002, Joe starts writing his PhD thesis at the Swedish
Institute of Computer Science (SICS). Joe was born 1950, so he was probably 52
years old at this point. The topic of the thesis is *Making reliable distributed
systems in the presence of software errors* and it was finished the year after
in 2003.

It's quite an unusual thesis in many ways. For starters, most theses are written
by people in their twenties with zero experience of practical applications.
Whereas in Joe's case he has been working professionally on this topic since the
80s, i.e. about twenty years. The thesis contains no math nor theory, it's
merely a presentation of the ideas that underpin Erlang and how they used Erlang
to achieve the original goal of building reliable distributed systems.

I highly commend reading his
[thesis](http://kth.diva-portal.org/smash/record.jsf?pid=diva2%3A9492&dswid=-1166)
and forming your own opinion, but to me it's clear that the big idea there isn't
lightweight processes[^1] and message passing, but rather the generic components
which in Erlang are called *behaviours*.

## Behaviours

I'll first explain in more detail what behaviours are, and then I'll come back
to the point that they are more important than the idea of lightweight processes.

Erlang behaviours are like interfaces in, say, Java or Go. It's a collection of
type signatures which can have multiple implementations, and once the programmer
provides such an implementation they get access to functions written against
that interface. To make it more concrete here's a contrived example in Go:

```go
// The interface.
type HasName interface {
        Name() string
}

// A generic function written against the interface.
func Greet(n HasName) {
    fmt.Printf("Hello %s!\n", n.Name())
}

// First implementation of the interface.
type Joe struct {}

func (_ *Joe) Name() string {
        return "Joe"
}

// Second implementation of the interface.
type Mike struct {}

func (_ *Mike) Name() string {
        return "Mike"
}

func main() {
        joe := &Joe{}
        mike := &Mike{}
        Greet(mike)
        Greet(joe)
}
```

Running the above program will display:

```
Hello Mike!
Hello Joe!
```

This hopefully illustrates how `Greet` is generic in, or parametrised by, the
interface `HasName`.

### Generic server behaviour

Next lets have look at a more complicated example in Erlang taken from Joe's
thesis (p. 136). It's a key-value store where we can `store` a key value pair or
`lookup` the value of a key, the `handle_call` part is the most interesting:

```erlang
-module(kv).
-behaviour(gen_server).

-export([start/0, stop/0, lookup/1, store/2]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
  gen_server:start_link({local,kv},kv,arg1,[]).

stop() -> gen_server:cast(kv, stop).

init(arg1) ->
  io:format("Key-Value server starting~n"),
  {ok, dict:new()}.

store(Key, Val) ->
  gen_server:call(kv, {store, Key, Val}).

lookup(Key) -> gen_server:call(kv, {lookup, Key}).

handle_call({store, Key, Val}, From, Dict) ->
  Dict1 = dict:store(Key, Val, Dict),
  {reply, ack, Dict1};
handle_call({lookup, crash}, From, Dict) ->
  1/0; %% <- deliberate error :-)
handle_call({lookup, Key}, From, Dict) ->
  {reply, dict:find(Key, Dict), Dict}.

handle_cast(stop, Dict) -> {stop, normal, Dict}.

terminate(Reason, Dict) ->
  io:format("K-V server terminating~n").
```

This is an implementation of the `gen_server` behaviour/interface. Notice how
`handle_call` updates the state (`Dict`) in case of a `store` and `lookup`s the
key in the state. Once `gen_server` is given this implementation it will provide
a server which can handle concurrent `store` and `lookup` requests, similarly to
how `Greet` provided the displaying functionality.

At this point you might be thinking "OK, so what? Lots of programming languages
have interfaces...". That's true, but notice how `handle_call` is completely
sequential, i.e. all concurrency is hidden away in the generic `gen_server`
component. "Yeah, but that's just good engineering practice which can be done in
any language" you say. That's true as well, but the thesis pushes this idea
quite far. It identifies six behaviours: `gen_server`, `gen_event`, `gen_fsm`,
`supervisor`, `application`, and `release` and then says these are enough to
build reliable distributed systems. As a case study Joe uses one of Ericsson's
telephone switches (p. 157):

> When we look at the AXD301 project in chapter 8, we will see that there were
> 122 instances of gen_server, 36 instances of gen_event and 10 instances of
> gen_fsm. There were 20 supervisors and 6 applications. All this is packaged
> into one release.

Joe gives several arguments for why behaviour should be used (p. 157-158):

  1. The application programmer only has to provide the part of the code which
     defines the *semantics* (or "business logic") of their problem, while the
     *infrastructure* code is provided automatically by the behaviour;

  2. The application programmer writes sequential code, all concurrency is hidden
     away in the behaviour;

  3. Behaviours are written by experts, and based on years of experience and
     represent "best practices";

  4. Easier for new team members to get started: business logic is sequential,
     similar structure that they might have seen before elsewhere;

  5. If whole systems are implemented reusing a small set of behaviours: as
     behaviour implementations improve the whole systems will improve without
     requiring any code changes;

  6. Sticking to only using behaviours enforces structure, which in turn makes
     testing and formal verification much easier.

We'll come back to this last point about testing later.

### Event manager behaviour

Lets come back to the behaviours we listed above first. We looked at
`gen_server`, but what are the others for? There's `gen_event` which is a
generic event manager, which lets you register event handlers that are then run
when the event manager gets messages associated with the handlers. Joe says this
is useful for, e.g., error logging and gives the following example of an simple
logger (p. 142):

```erlang
-module(simple_logger).
-behaviour(gen_event).

-export([start/0, stop/0, log/1, report/0]).

-export([init/1, terminate/2,
         handle_event/2, handle_call/2]).

-define(NAME, my_simple_event_logger).

start() ->
  case gen_event:start_link({local, ?NAME}) of
    Ret = {ok, Pid} ->
      gen_event:add_handler(?NAME,?MODULE,arg1),
      Ret;
  Other ->
    Other
  end.

stop() -> gen_event:stop(?NAME).

log(E) -> gen_event:notify(?NAME, {log, E}).

report() ->
  gen_event:call(?NAME, ?MODULE, report).

init(arg1) ->
  io:format("Logger starting~n"),
  {ok, []}.

handle_event({log, E}, S) -> {ok, trim([E|S])}.

handle_call(report, S) -> {ok, S, S}.

terminate(stop, _) -> true.

trim([X1,X2,X3,X4,X5|_]) -> [X1,X2,X3,X4,X5];
trim(L) -> L.
```

The interesting part is `handle_event`, `trim` and `report`. Together they let
the user log, keep track and display the last five error messages.

### State machine behaviour

The `gen_fsm` behavior has been renamed to `gen_statem` (for state machine)
since thesis was written. It's very similar to `gen_server`, but more geared
towards implementing protocols, which often are specified as state machines. I
believe any `gen_server` can be implemented as a `gen_statem` and vice versa so
we won't go into the details of `gen_statem`.

### Supervisor behaviour

The next interesting behavior is `supervisor`. Supervisors are processes which
sole job is to make sure that other processes are healthy and doing their job.
If a supervised process fails then the supervisor can restart it according
to some predefined strategy. Here's an example due to Joe (p. 148):

```erlang
-module(simple_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
  supervisor:start_link({local, simple_supervisor},
  ?MODULE, nil).

init(_) ->
  {ok,
  {{one_for_one, 5, 1000},
  [
   {packet,
     {packet_assembler, start, []},
     permanent, 500, worker, [packet_assembler]},
   {server,
     {kv, start, []},
     permanent, 500, worker, [kv]},
   {logger,
     {simple_logger, start, []},
     permanent, 500, worker, [simple_logger]}]}}.
```

The `{one_for_one, 5, 1000}` is the restart strategy. It says that if one of the
supervised processes (`packet_assembler`, `kv`, and `simple_logger`) fail then
only restart the failing process (`one_for_one`). If the supervisor needs to
restart more than `5` times in `1000` seconds then the supervisor itself should
fail.

The `permanent, 500, worker` part means that this is a worker process which
should be permanently kept alive and its given 500 milliseconds to gracefully
stop what it's doing in case the supervisor wants to restart it.

"Why would the supervisor want to restart it if it's not dead already?", one
might wonder. Well, there are other restart strategies than `one_for_one`. For
example, `one_for_all` where if one process fails then the supervisor restarts
all of its children.

If we also consider that supervisors can supervise supervisors, which are not
necessarily running on the same computer, then I hope that you get an idea of
powerful this behaviour can be. And, no, this isn't "just Kubernetes", because
it's at the thread/lightweight process level rather than docker container level.

The idea for supervisors and their restart strategies comes from the observation
that often a restart appears to fix the problem, as captured in the *Have You
Tried Turning It Off And On Again?* sketches from IT Crowd.

Knowing that failing processes will get restarted coupled with Jim Gray's idea
of failing fast, that's either produce the output according to the specification
or signal failure and stop operating, leads to Joe's slogan: "Let it crash!" (p.
107). Another way to think of it is that a program should only express its
"happy path", should anything go wrong on its happy way it should crash, rather
than trying to be clever about it and try to fix the problem (potentially making
it worse), and another program higher up the supervisor tree will handle it.

Supervisors and the "let it crash" philosophy, appear to produce reliable
systems. Joe uses the Ericsson AXD301 telephone switch example again (p. 191):

> Evidence for the long-term operational stability of the system had also not
> been collected in any systematic way. For the Ericsson AXD301 the only
> information on the long-term stability of the system came from a power-point
> presentation showing some figures claiming that a major customer had run an 11
> node system with a 99.9999999% reliability, though how these figure had been
> obtained was not documented.

To put this in perspective, five nines (99.999%) reliability is considered good
(5.26 minutes of downtime per year). "59% of Fortune 500 companies experience a
minimum of 1.6 hours of downtime per week", according to some
[report](https://courseware.cutm.ac.in/wp-content/uploads/2020/06/Assessing-the-Financial-Impact-of-Downtime-UK.pdf)
from a biased company. Notice per *year* vs per *week*, but as we don't know how
either reliability numbers are obtained its probably safe to assume that the
truth is somewhere in the middle -- still a big difference, but not 31.56
milliseconds (nine nines) of downtime per year vs 1.6 hours of downtime per
week.

### Application and release behaviours

I'm not sure if `application` and `release` technically are behaviours, i.e.
interfaces. They are part of the same chapter as the other behaviours in the
thesis and they do provide a clear structure which is a trait of the other
behaviours though, so we'll include them in the discussion.

So far we've presented behaviours from the bottom up. We started with "worker"
behaviours `gen_server`, `gen_statem` and `gen_event` which together capture the
semantics of our problem. We then saw how we can define `supervisor` trees whose
children are other supervisor trees or workers, to deal with failures and
restarts.

Next level up is an `application` which consists of a supervisor tree together
with everything else we need to deliver a particular application.

A system can consist of several `application` and that's where the final
"behaviour" comes in. A `release` packages up one or more applications. They
also contain code to handle upgrades. If the upgrade fails, it must be able to
rollback to the previous stable state.

## How behaviours can be implemented

I hope that by now I'm managed to convince you that it's not actually the
lightweight processes and message passing by themselves that make Erlang great
for building reliable systems.

At best one might be able to claim that lightweight processes and supervisors
are the key mechanisms at play[^2], but I think it would be more honest to recognise
the structure that behaviours provide and how that ultimately leads to reliable
software.

I've not come across any other language, library, or framework which provides
such relatively simple building blocks that compose into big systems like the
AXD301 ("over a million lines of Erlang code", p. 167).

This begs the question: why aren't language and library designers stealing the
structure behind Erlang's behaviours, rather than copying the ideas of
lightweight processes and message passing?

Let's take a step back. We said earlier that behaviours are interfaces and many
programming languages have interfaces. How would we go about starting to
implement behaviours in other languages?

Lets start with `gen_server`. I like to think its interface signature as being:

```haskell
Input -> State -> (State, Output)
```

That's it takes some input, its current state and produces a pair of the new
updated state and an output.

How do we turn this sequential signature into something that can handle
concurrent requests? One way would be to fire up a HTTP server which transforms
requests into `Input`s and puts them on a queue, have an event loop which pops
inputs from the queue and feeds it to the sequential implementation, then
writing the output back to the client response. It wouldn't be difficult to
generalise this to be able to handle multiple `gen_server`s at the same time, by
giving each a name and let the request include the name in addition to the
input.

`gen_event` could be implemented by allowing registration of callbacks to
certain types of event on the queue.

`supervisor`s is more interesting, one simple way to think of it is: when we
feed the `gen_server` function the next input from the queue, we wrap that call
in an exception handler, and should it throw we notify its supervisor. It gets a
bit more complicated if the supervisor is not running on the same computer as
the `gen_server`.

I haven't thought about `application` and `release`s much yet, but given that
configuration, deployment and upgrades are difficult problems they seem
important.

## Correctness of behaviours

Writing a post solely about stealing from Erlang doesn't seem fair, even though
it's the right thing to do, so I'd like to finish off with how we can build upon
the insights of Joe and the Erlang community.

I've been interesting in testing for a while now. Most recently I've been
looking into [simulation
testing](https://github.com/stevana/property-based-testing-stateful-systems-tutorial)
distributed systems à la
[FoundationDB](https://www.youtube.com/watch?v=4fFDFbi3toc).

Simulation testing in a nutshell is running your system in a simulated world,
where the simulation has full control over which messages get sent when over the
network.

FoundationDB built their own programming language, or dialect of C++ with
actors, in order do the simulation testing. Our team seemed to be able to get
quite far with merely using state machines of type:

```haskell
Input -> State -> (State, [Output])
```

where `[Output]` is a sequence of outputs.

The idea being that the simulator keeps track of a priority queue of messages
sorted by their arrival time, it pops a message, advances the clock to the
arrival time of that message, feeds the message to the receiving state machine,
generates new arrival times for all output messages and puts them back into the
priority queue, rinse and repeat. As long as everything is deterministic and the
arrival times are generated using a seed we can explore many different
interleavings and get reproducible failures. It's also much faster than Jepsen,
because messaging is done in-memory and we advance the clock to the arrival
time, thereby triggering any timeouts without having to wait for them.

We used to say that programs of this state machine type where written in
"network normal form", and conjectured that every program which can receive and
send stuff over the network can be refactored into this shape[^3]. Even if we
had a proof, "network normal form" always felt a bit arbitrary. But then I read
Joe's thesis and realised that `gen_server` and `gen_statem` basically have the
same type, so I stopped being concerned about it. As I think that if a structure
is found to be useful by different people, then it's usually a sign that it
isn't arbitrary.

Anyway, in, at least, one of Joe's [talks](https://youtu.be/cNICGEwmXLU?t=1439)
he mentions how difficult it's to correctly implement distributed leader
election.

I believe this is a problem that would be greatly simplified by having access to
a simulator. A bit like I'd imagine having access to a wind tunnel would make
building an airplane easier. Both lets you test your system under extreme
conditions, such as unreliable networking or power loss, before they happen in
"production". Furthermore, this simulator can be generic in, or parametrised by,
behaviours. Which means that the developer gets it for free while the complexity
of the simulator is hidden away, just like the concurrent code of `gen_server`!

FoundationDB is a good example of simulation testing working, as witnessed by
this [tweet](https://twitter.com/aphyr/status/405017101804396546) where somebody
asked Kyle "aphyr" Kingsbury to Jepsen test FoundationDB:

> “haven’t tested foundation[db] in part because their testing appears to be
> waaaay more rigorous than mine.”

Formal verification is also made easier if the program is written a state
machine. Basically all of Lamport's model checking
[work](https://www.microsoft.com/en-us/research/publication/computation-state-machines/)
with TLA+ assumes that the specification is a state machine. Also more recently
Kleppmann has
[shown](https://lawrencecpaulson.github.io/2022/10/12/verifying-distributed-systems-isabelle.html)
how to exploit the state machine structure to do proof by (structural) induction
to solve the state explosion problem.

So there you have it, we've gone full circle. We started by taking inspiration
from Joe and Erlang's behaviours, and ended up using the structure of the
`gen_server` behaviour to make it easier to solve a problem that Joe used to
have.

## Contributing

There are a bunch of related ideas that I have started working on:

  * Stealing ideas from Martin Thompson's work on the LMAX Disruptor and
    [aeron](https://github.com/real-logic/aeron) to make a fast event loop, on
    top of which the behaviours run;
  * Enriching the state machine type with [async
    I/O](https://github.com/stevana/coroutine-state-machines);
  * How to implement supervisors in more detail;
  * Hot code swapping of state machines.

I hope to write about these things separately at some later point.

Meanwhile feel free to get in touch, if you find any of this interesting and
would like to get involved, or if you have have comments, suggestions or
questions.

## See also

* Chapter 6.1 on behaviours in Joe Armstrong's
  [thesis](http://kth.diva-portal.org/smash/record.jsf?pid=diva2%3A9492&dswid=-1166),
  p. 129;
* [OTP design principles](https://www.erlang.org/doc/design_principles/des_princ.html);
* The documentation for behaviours:
    - [`gen_server`](https://www.erlang.org/doc/man/gen_server.html);
    - [`gen_event`](https://www.erlang.org/doc/man/gen_event.html);
    - [`gen_statem`](https://www.erlang.org/doc/man/gen_statem.html);
    - [`supervisor`](https://www.erlang.org/doc/man/supervisor.html);
    - [`application`](https://www.erlang.org/doc/man/application.html);
    - [release](https://www.erlang.org/doc/design_principles/release_structure.html).
* [Hewitt, Meijer and Szyperski: The Actor Model (everything you wanted to know,
  but were afraid to ask)](https://youtube.com/watch?v=7erJ1DV_Tlo) (2012);
* Erlang the [movie](https://www.youtube.com/watch?v=xrIjfIjssLE) (1990);
* [Systems that run forever self-heal and
  scale](https://www.youtube.com/watch?v=cNICGEwmXLU) by Joe Armstrong (Strange
  Loop, 2013);
* [The Do's and Don'ts of Error
  Handling](https://www.youtube.com/watch?v=TTM_b7EJg5E) by Joe Armstrong (GOTO,
  2018);
* [The Zen of Erlang](https://ferd.ca/the-zen-of-erlang.html) by Fred Hebert
  (2016);
* [The Hitchhiker's Guide to the
  Unexpected](https://ferd.ca/the-hitchhiker-s-guide-to-the-unexpected.html) by
  Fred Hebert (2018);
* [Why Do Computers Stop and What Can Be Done About
  It?](https://www.hpl.hp.com/techreports/tandem/TR-85.7.pdf) by Jim Gray
  (1985);
* The supervision trees chapter of [*Adopting
  Erlang*](https://adoptingerlang.org/docs/development/supervision_trees/)
  (2019);
* "If there's one thing I'd say to the Erlang folks, it's you got the stuff
  right from a high-level, but you need to invest in your messaging
  infrastructure so it's super fast, super efficient and obeys all the right
  properties to let this stuff work really well."
  [quote](https://youtu.be/OqsAGFExFgQ?t=2532) by Martin Thompson (Functional
  Conf, 2017).


[^0]: From Joe Armstrong's thesis (p. 6):

    > In February 1998 Erlang was banned for new product development within
    > Ericsson—the main reason for the ban was that Ericsson wanted to be a consumer
    > of sodware technologies rather than a producer.

    From Bjarne Däcker's thesis (2000, p. 37):

    > In February 1998, Erlang was banned within Ericsson Radio AB (ERA) for new
    > product projects aimed for external customers because:
    >
    > “The selection of an implementation language implies a more long-term
    > commitment than selection of processors and OS, due to the longer life cycle
    > of implemented products. Use of a proprietary language, implies a continued
    > effort to maintain and further develop the support and the development
    > environment. It further implies that we cannot easily benefit from, and find
    > synergy with, the evolution following the large scale deployment of globally
    > used languages.”

[^1]: It's a common misconception is that Erlang is about actors.

    The actor model first presented in [*A Universal Modular Actor Formalism for
    Artificial
    Intelligence*](https://www.ijcai.org/Proceedings/73/Papers/027B.pdf) by Carl
    Hewitt, Peter Bishop, Richard Steiger (1973) and refined by others over time,
    e.g. see Irene Greif's [thesis](https://dspace.mit.edu/handle/1721.1/57710)
    (1975) or Gul Agha's [thesis](https://dspace.mit.edu/handle/1721.1/6952)
    (1985).

    Erlang first appeard later in 1986, but the Erlang developers were [not
    aware](https://erlang.org/pipermail/erlang-questions/2014-June/079794.html) of
    the actor model. In fact Robert Virding, one of the original Erlang designers,
    [claims](https://erlang.org/pipermail/erlang-questions/2014-June/079865.html)
    that it knowing about the actor model might even have slowed them down.

    Carl Hewitt has written a paper called [*Actor Model of Computation: Scalable
    Robust Information Systems*](https://arxiv.org/abs/1008.1459) (2015) which
    documents the differences between Erlang's processes and the actor model.

[^2]: Scala's Akka seems to be of this opinion. They got something they call
    "actors", not to be confused with the actor model as per footnote 1, and
    obligatory supervisors trees. They don't appear to have any analogues of the
    other Erlang behaviours though.

    Confusingly Akka has a concept called
    ["behavior"](https://doc.akka.io/docs/akka/current/general/actors.html#behavior),
    but it has nothing to do with Erlang behaviours.

[^3]: The intuition being that since every program using the state monad can be
    rewritten to a normal form where a single `read`/`get` followed by a single
    `write`/`put`, it seems reasonable to assume that something similar would
    work for `recv` and `send` over the network. I forget the reference for the
    state monad normal form, either Plotkin and Power or Uustalu?
