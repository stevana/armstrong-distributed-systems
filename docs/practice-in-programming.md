---
status: Still in research (please don't share)
---

# On the role of practice in programming

- We all know it takes practice to become good at something.

- Yet most programmers don't practice at all.

- If you look up the word *practice* in the dictionary it says: "the act of
  doing something regularly or repeatedly to improve your skill at doing it".

- Mike Acton (of [Data-Oriented
  Design](https://youtube.com/watch?v=rX0ItVEVjHc) fame) gave an
  [interview](https://youtu.be/qWJpI2adCcs?t=3506) a HandmadeCon (2015) where
  he was asked "how to become a good software engineer?" His answer in short:
  practice, and practice starts from scratch every time (unlike a project or
  hobby which builds upon previous work)

- Why doesn't working on a project every day qualify as practice? A project
  involves any parts, so it's unlikely that you are repeatedly doing some
  *specific* thing.

- If your project is relatively small and you are doing it over and over again
  that might qualify as practice.

- Mike gives the example of setting aside 30 mins per day to practice to try to
  implement an [Asteroids](https://en.wikipedia.org/wiki/Asteroids_(video_game))
  clone, in the beginning you'll probably not get very far, throw it away, start
  from scratch, by day 300 you might be able to finish implementing the whole
  game in 30 mins. Perhaps Asteroids isn't the best thing to practice, but you
  get the idea.

- John Carmack, "developing 13 games in a year", [id Software's early
  days](https://youtu.be/IzqdZAYcwfY?t=540)

- Early id Software is an extreme example, most of us probably need to practice
  on something much smaller

- When asked why he wrote on of his books, Joe said: "I clearly remember just
  throwing away... I wrote chapter, and the next day I threw it away, and I
  rewrote it from scratch. That's what I've *always* done with code, but never
  done with English and at that stage it went like bingo -- I can write!"
  https://vimeo.com/1344065 (8:30)

- "Throw away any code that isn't finished after a day. It's garbage anyway." --
  Joe Armstrong (https://twitter.com/sadisticsystems/status/1367399917573070851)

- About Rich Hickey (creator of Clojure), some interview probably with somebody
  else from Cognitect (half?) jokingly: "Rick doesn't write programs longer than
  1000 lines" (I cannot find the reference, I think it was a clojure meetup in
  london with a discussion after the talk? perhaps on simulant or repl driven
  development?)

- Can whole projects be "small"? Here's two Turing award winners who think so:

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
  built." -- [A Plea for Lean Software](https://cr.yp.to/bib/1995/wirth.pdf) by
  Niklaus Wirth (1995)

- Forth, bounded by blocks: "Disk memory is divided into units called “blocks.”
  Each block holds 1,024 characters of source text or binary data, traditionally
  organized as 16 lines of 64 characters."
  https://www.forth.com/starting-forth/3-forth-editor-blocks-buffer/

- Chuck Moore reimplemented the same Forth many times, in fact Forth was
  designed to be easily reimplementable on new hardware (this was back when new
  CPUs had new instruction sets), he also iterated on the Forth itself (...,
  colorForth, what where the earlier iterations?)

- Role of practice in software development (as opposed to programming)?
  Processes and ways of working based on practice? Refactor vs rewrite from
  scratch debate?

- What would programming languages look like if we applied these principles?
  Forth is a good example, are there others?

- What about libraries? Can we design building blocks that allow us to build
  reliable, scalable and maintainable systems, in a way such that the building
  blocks can be understood and implemented by a single programmer in a day?

- One of the goals of this repository is to try to identify those building
  blocks and try to understand them well enough, document what Joe
  [calls](https://youtu.be/h8nmzPh5Npg?t=1302) "the research", perhaps through
  several reimplementations from scratch, so that they can be implemented by
  others in a day.

- More important to provide what Joe called "the research" than to provide a library?

- diy vs 3rd party: http://ithare.com/overused-code-reuse/
  + https://lobste.rs/s/yubtob/build_vs_buy
  + https://www.joelonsoftware.com/2001/10/14/in-defense-of-not-invented-here-syndrome/
  + https://eli.thegreenplace.net/2017/benefits-of-dependencies-in-software-projects-as-a-function-of-effort/

  See chapter 4 of *Development and Deployment of Multiplayer Online Games, Vol.
  II* by Sergey Ignatchenko (2020).


## Contributing

* Any references in the same general (or completely opposite) direction would be
  appreciated!
* [Coding dojos](https://codingdojo.org/practices/WhatIsCodingDojo/) are spaces specifically designed for _practicing_ 
