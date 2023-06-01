---
status: Still in research (please don't share)
---

# On the role of practice in programming

## Motivation

Practice makes perfect, the saying goes. Yet, very dedicated, people appear to
spend a lifetime programming without producing a masterpiece. Why is that? The
perhaps most common explaination is that the field is still young and we haven't
figured out how to engineer things with the same degree of accurency and
predicability as other more established fields. A less common explaination,
which I'd like to explore in this post, is that just because we go to work and
program the whole day long it doesn't mean that we are in fact practicing.

## Defining practice

Mike Acton gave an [interview](https://youtu.be/qWJpI2adCcs?t=3506) where he
said that practice starts *from scratch every time*, unlike a project at work or
as a hobby which builds upon previous work.

I suppose the key thing is that you redo some *specific* thing many times until
you become really good at it. Most projects involve many parts, so it's unlikely
that you are repeatedly doing some specific thing over and over again. This
could explain why working on projects isn't practice.

One exception could be if your project is relatively small and you are doing it
over and over again, then that might qualify as practice.

Mike gives the example of setting aside half an hour per day to practice to try
to implement an
[Asteroids](https://en.wikipedia.org/wiki/Asteroids_(video_game)) clone. In the
beginning you'll probably not get very far, throw it away, start from scratch,
by day 300 you might be able to finish implementing the whole game in the
allocated time. Perhaps Asteroids isn't the best thing to practice, but you get
the idea.

While on the topic of games and getting good at programming, it's interesting to
note that John Carmack and the rest of id Software [developing 13 games in a
year](https://youtu.be/IzqdZAYcwfY?t=540). Early id Software is an extreme
example, most of us probably need to practice on something much smaller.

## Putting practice into a practice

Having established what we mean by practice, let's turn our attention to how to
put practice into a practice.

Joe Armstrong is a good example. He
[explains](https://vimeo.com/1344065#t=8m30s) that he often wrote a piece of
code and the next day he threw it away and rewrote it from scratch. In the early
days of Erlang it was possible to do a total rewrite of the whole language in
less than a week. New language features were added in one work session, if you
couldn't get the idea out of your brain and code it up in that time then you
didn't do it, Joe
[explained](https://dl.acm.org/action/downloadSupplement?doi=10.1145%2F1238844.1238850&file=m6-armstrong-h.mov)
(17:10). In a later talk he elaborated
[saying](https://youtu.be/rQIE22e0cW8?t=3492):

> "We need to break systems down into small understandable components with
> message passing between them and with contracts describing whats going on
> between them so we can understand them, otherwise we just won't be able to
> make software that works. I think the limit of human understandability is
> something like 128KB of code in any language. So we really need to box things
> down into small units of computation and formally verify them and the
> protocols in particular."

Chuck Moore
[said](https://www.red-gate.com/simple-talk/opinion/geek-of-the-week/chuck-moore-geek-of-the-week/)
something in similar:

> "Instead of being rewritten, software has features added. And becomes more
> complex. So complex that no one dares change it, or improve it, for fear of
> unintended consequences. But adding to it seems relatively safe. We need
> dedicated programmers who commit their careers to single applications.
> Rewriting them over and over until they're perfect." (2009)

> "... Such people will never exist. The world is too full of more interesting
> things to do. The only hope is to abandon complex software. Embrace simple.
> Forget backward compatibility."

* Both Joe and Chuck ask for simple systems, so that they can easily be
  rewritten (i.e. practiced on)

- Chuck Moore reimplemented the same Forth many times, in fact Forth was
  designed to be easily reimplementable on new hardware (this was back when new
  CPUs had new instruction sets), he also iterated on the Forth itself (...,
  colorForth, what where the earlier iterations?)

* OKAD, [VLSI](https://en.wikipedia.org/wiki/Very_Large_Scale_Integration)
  design tools, "I’ve spent more time with it that any other; have re-written it
  multiple times; and carried it to a satisfying level of maturity."

* John McCarthy's Lisp with its meta-circular implementation?

## Practice and software engineering

* We've defined pratice, we've seen examples of people who appear to use it on
  personal level, what about scaling it up to teams?

- Can whole projects be simple? Here's two Turing award winners who think so:

- "At last, there breezed into my office the most senior manager of all, a
  general manager of our parent company, Andrew St. Johnston. I was surprised
  that he had even heard of me. "You know what went wrong?" he shouted -- he
  always shouted -- "You let your programmers do things which you yourself do
  not understand." I stared in astonishment. He was obviously out of touch with
  present day realities. How could one person ever understand the whole of a
  modem software product like the Elliott 503 Mark II software system? I
  realized later that he was absolutely right; he had diagnosed the true cause
  of the problem and he had planted the seed of its later solution." --

  The emperor's old clothes, Tony Hoare (1980)
  https://dl.acm.org/doi/10.1145/1283920.1283936

- "The belief that complex systems require armies of designers and programmers
  is wrong. A system that is not understood in its entirety, or at least to a
  significant degree of detail by a single individual, should probably not be
  built." -- [A Plea for Lean
  Software](https://people.inf.ethz.ch/wirth/Articles/LeanSoftware.pdf) by
  Niklaus Wirth (1995)

- Joe story about new manager asking for somebody who can explain the whole
  system to him. "Does anybody understand the entire system? If so, please come
  and talk to me. Nobody put their hand up. In the Erlang group, if somebody
  would have asked that question, several hands would have gone up."
  https://youtu.be/-I_jE0l7sYQ?t=1389

## Processes and tools that encourage practice?

- Let's assume that Mike, Joe, Chuck, Tony and Niklaus are on to something and
  that it's in fact possible to *designing for practice* even in team-sized
  projects (aka software development as opposed to programming)

- What would processes and tools that help encourge such development look like?

### The status quo

- TDD: write test that fails, simplest possilbe implementation that makes test
  pass, then refactor *incrementally* until satisfactory while keeping tests
  green.
  + start from scratch rather than refactor?
  + time limit (e.g. Joe's one working session)
  + size limit (e.g. Joe's 128KB?)
  + test-driven design: easily testable system is well-designed system
  + practice-driven design: easily rewritable system is a well-designed system?
    * the fact that a system is easily and fully testable surely helps when
      rewriting from scratch, but it feels like there's more to it?
    * good spec / documentation / literate programming? Joe's "the research"
    * ability to "zoom" in and out on spec / docs? Refinement.

- Refactor vs rewrite from scratch debate?

- diy vs 3rd party: http://ithare.com/overused-code-reuse/
  + https://lobste.rs/s/yubtob/build_vs_buy
  + https://www.joelonsoftware.com/2001/10/14/in-defense-of-not-invented-here-syndrome/
  + https://eli.thegreenplace.net/2017/benefits-of-dependencies-in-software-projects-as-a-function-of-effort/
  + See chapter 4 of *Development and Deployment of Multiplayer Online Games,
    Vol. II* by Sergey Ignatchenko (2020).

### Possible tricks to steal

- Parallel and independent development, c.f. Dave Snowden and [wisdom of the
  crowd](https://en.wikipedia.org/wiki/Wisdom_of_the_crowd)

- Encourage new team members to rewrite?

- What would programming languages look like if we applied these principles?
  Forth is a good example, are there others?

- Forth, bounded by blocks: "Disk memory is divided into units called “blocks.”
  Each block holds 1,024 characters of source text or binary data, traditionally
  organized as 16 lines of 64 characters."
  https://www.forth.com/starting-forth/3-forth-editor-blocks-buffer/

> There is a great similarity between colorForth and classic Forth: 1024-byte
> blocks. Factoring source code into blocks is equivalent to creating paragraphs
> in English. It breaks a wall of text into pieces that highlight related ideas.
> Many Forth implementations abandoned this advantage and organized source into
> files of arbitrary length. Bad idea. Hugely debated. In addition, the text in
> a block can be displayed on a monitor without scrolling. A quantized unit of
> source code all visible at once.

- What about libraries? Can we design building blocks that allow us to build
  reliable, scalable and maintainable systems, in a way such that the building
  blocks can be understood and implemented by a single programmer in a day?
  Similar to how features were added to early Erlang, as mentioned above?

- One of the goals of this repository is to try to identify those building
  blocks and try to understand them well enough, document what Joe
  [calls](https://youtu.be/h8nmzPh5Npg?t=1302) "the research", perhaps through
  several reimplementations from scratch, so that they can be implemented by
  others in a day.

- More important to provide what Joe called "the research" than to provide a
  library?

- "To gain experience, there's no sustitute for one's own programming effort.
  Organizing a team into managers, designers, programers, analysts and users is
  detrimental. All should participate (with differing degrees of emphasis) in
  all aspects of development. In particular, everybody -- including managers --
  should also be product users for a time. This last measure is the best
  guarantee to correct mistakes and perhaps also eliminate redundancies." --
  Niklaus Wirth

* "What I cannot create, I do not understand" -- Richard Feynman

## Contributing

* Other examples of processes or tools that encourage practice?

* Any references in the same general (or completely opposite) direction would be
  appreciated!

- About Rich Hickey (creator of Clojure), some interview probably with somebody
  else from Cognitect (half?) jokingly: "Rick doesn't write programs longer than
  1000 lines" (I cannot find the reference, I think it was a clojure meetup in
  london with a discussion after the talk? perhaps on simulant or repl driven
  development?)

## See also

* [Coding dojos](https://codingdojo.org/practices/WhatIsCodingDojo/) are spaces
  specifically designed for *practicing*;
* [Are We Really
  Engineers?](https://www.hillelwayne.com/post/are-we-really-engineers/);
* Mike Acton's talk on [Data-Oriented Design](https://youtube.com/watch?v=rX0ItVEVjHc).
