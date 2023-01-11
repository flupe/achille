---
title: The gist of achille
---

Writing down some thoughts behind the design of **achille**, ungoing work,
ramblings. Some of it *may* get turned into a paper if deemed interesting
enough.

## Build sytems

At its heart, **achille** is nothing more than a *build system*.
Readers may already be familiar with existing build systems, like Make, Shake,
Ninja.

The *role* of a build system is to execute some *tasks* in order to produce
*artefacts* on disk. These taks may depend on other tasks, and the build system
has to ensure that they are run *in the appropriate order*.

Additionally, build systems can satisfy the following properties:

Correctness
: Of course, *the* most important property of a build system is to
  be correct. That is, to always produce the intended artefacts, according to
  the user specification.

Minimal
: A build system is said to be *minimal* when it only rebuilds what is
  strictly-necessary, considering the changes made *since the last run*.

Self-tracking
: A build system is *self-tracking* when it is able to account for changes made to
  user rules since the last run, and rebuild accordingly.
