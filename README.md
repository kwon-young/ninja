# Ninja build system generator

This module contains helper dcg predicates to generate ninja build files akin to the [`ninja_syntax.py`](https://github.com/ninja-build/ninja/blob/master/misc/ninja_syntax.py) python module distributed by ninja.
You can use these predicates if you want to generate your own `build.ninja` build file.

Example usage:

```prolog
build_graph -->
  rule(cp, "cp $in $out"),
  build(["input.txt"], cp, ["output.txt"]).

main -->
  phrase(build_graph, L),
  open("build.ninja", write, Stream),
  string_codes(S, L),
  write(Stream, S),
  close(Stream).
```

Then `build.ninja` contains the following build specification:

```
rule cp
  command = cp $in $out

build input.txt: cp output.txt
```

See the [ninja build format documentation](https://ninja-build.org/manual.html) for generating more complex build files.

## Installation

You can install this pack in swi-prolog as follows:

```prolog
?- pack_install(ninja).
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
Install ninja@0.2 from https://github.com/kwon-young/ninja/archive/v0.2.zip Y/n? 
% Contacting server at https://www.swi-prolog.org/pack/query ... ok
% "v0.2.zip" was downloaded 2 times
Package:                ninja
Title:                  Ninja build system generator
Installed version:      0.2
Author:                 Kwon-Young Choi <kwon-young.choi@hotmail.fr>
Maintainer:             Kwon-Young Choi <kwon-young.choi@hotmail.fr>
Packager:               Kwon-Young Choi <kwon-young.choi@hotmail.fr>
Home page:              https://github.com/kwon-young/ninja
Download URL:           https://github.com/kwon-young/ninja/releases/*.zip
Provides:               ninja
Install "ninja-0.2.zip" (15,854 bytes) Y/n? 
true.
```

## Documentation

You can find a documented list of predicate [here](https://kwon-young.github.io/ninja/).

You can generate the documentation locally, either statically in the subdirectory `docs`:

```
$ swipl prolog/ninja.pl
?- doc_save('.', [doc_root('docs'), recursive(true)]).
```

Or as a server:

```prolog
?- doc_server(4000).
```

## Requirements

This library only depends on prolog librairies distributed by default and autoloaded by swi-prolog:

* [`dcg/basics`](https://www.swi-prolog.org/pldoc/man?section=basics)
* [`dcg/high_order`](https://www.swi-prolog.org/pldoc/man?section=highorder)
* [`option`](https://www.swi-prolog.org/pldoc/man?section=option)
* [`error`](https://www.swi-prolog.org/pldoc/man?section=error)

In order to use this library, you will need to have a good understanding of

* the [ninja build system file format](https://ninja-build.org/manual.html)
* Prolog Definite Clause Grammar (DCG). Here is a [very good primer](https://www.metalevel.at/prolog/dcg) on the subject.

## Motivation

I have always been in search of a good Domain Specific Language (DSL) for build system to do general data processing.
This project is my attempt at one.

But first, we need to define what is a build system or a build system generator.
Keep in mind that these definitions are highly subjective.

### What is a build system ?

A build system is a program which job is to execute other programs by following a build graph.
A build graph is often a file that specify the commands to run.
Each commands form a node in the graph, for which every input files an output files are clearly identified.
By using the same file as output of a node to the input of another node, we can connect the nodes as a graph.

The properties of a good build system are:

* Parallelism: build steps that do not depend on each other should be ran in parallel, so that the whole build finishes as fast as possible
* Caching: build steps should be only rebuilt only if outputs are out of dates

Examples of build systems are:

* make
* ninja
* tup
* redo
* [biomake](https://github.com/evoldoers/biomake)
* and many, many, many, many ... others

Although build systems are very useful, writing the build graph manually can be very tedious.
Often, build graph will consist of hundred or thousands (or more) build steps connected in a very complex graph.

This is where build system generator or meta build system come in.
Build system generator can translate a build graph specified in a custom DSL or programming language into a build graph that can be executed by `make` or `ninja` or another build system.
This is what `CMake` do for instance where its DSL is optimized for program compilation.

Most of the time, build system generators also are build systems.
For example, we could consider that `make` is its own build system generator when using its templating mechanism such as static patterns.
In my view, any syntax or DSL that allows you to avoid specifying the full build graph is a build system generator.

Here, `ninja` is the exception by requiring the full explicit build graph as its input.

### General data processing

The traditional use of build system is program compilation.
The rules to compile a program are *mostly* well known and are embodied in well known meta build system such as `autotools`, `CMake`, `Bazel`, etc.
Almost every programming language has its own meta build system.

However, it turns out that by following our dichotomy between a build system and a build system generator, we notice that build systems can be used for much more things than just program compilation.
Build system can do general data processing but you need to pick your poison on which DSL you will use to specify your build graph.

A very common choice is to use GNU Make syntax with automatic variables, static patterns, etc.
However, you will be very quickly limited in how expressive you can be.
Here is a use-case I have often encountered that cannot be cleanly expressed with Make.

Let's say, we want to process PDFs. We want to split out each page of each PDF, convert each page as an image, OCR the images and finally concatenate each page text into a single text file.
Each PDF can have a different number of pages.

Notice that we have one independent graph per PDF.
This can be cleanly expressed using static patterns in Make.
However, the split and the join at the pages level have to be specified manually for each PDF, which defeat the use of static patterns anyway.

### Using prolog DCG to generate a build graph

Here is how you could use a prolog DCG to specify the above graph.

First, let's specify the knowledge we have of the PDF files we want to process.
Namely, a unique stem for each PDF file and the number of pages for each stem.

```prolog
stem(foo).
stem(bar).

page(foo, 1).
page(foo, 2).
page(bar, 1).
page(bar, 2).
page(bar, 3).
```

Next, we will write DCG rules that builds the name of all the different files we need for our build graph:

```prolog
pdf(Stem) -->
  atom(Stem), ".pdf".

pdf_page(Stem, Page) -->
  atom(Stem), "-", number(Page), ".pdf".

all_pages(Goal, Stem) -->
  foreach(page(Stem, Page), call(Goal, Stem, Page), " ").

image(Stem, Page) -->
  atom(Stem), "-", number(Page), ".jpg".

ocr_page(Stem, Page) -->
  atom(Stem), "-", number(Page), ".txt".

ocr(Stem) -->
  atom(Stem), ".txt".
```

Next, we will describe the various commands to run as ninja rules:

```prolog
rules -->
  rule(split, "split $in $out"),
  rule(convert, "convert $in $out"),
  rule(ocr, "ocr $in $out"),
  rule(cat, "cat $in $out").
```

Now, let's write the build graph for a single PDF:

```prolog
graph(Stem) -->
  build([all_pages(pdf_page, Stem)], split, [pdf(Stem)]),
  foreach(page(Stem, Page), (
    build([image(Stem, Page)], convert, [pdf_page(Stem, Page)]),
    build([ocr_page(Stem, Page)], ocr, [image(Stem, Page)])
  )),
  build([ocr(Stem)], cat, [all_pages(ocr_page, Stem)]).
```

And finally, the full build graph with rules:

```prolog
graph -->
  rules,
  foreach(stem(Stem), graph(Stem)).
```

This DCG will generate the following `build.ninja` file:

```ninja
rule split
  command = split $in $out
rule convert
  command = convert $in $out
rule ocr
  command = ocr $in $out
rule cat
  command = cat $in $out
build foo-1.pdf foo-2.pdf: split foo.pdf
build foo-1.jpg: convert foo-1.pdf
build foo-1.txt: ocr foo-1.jpg
build foo-2.jpg: convert foo-2.pdf
build foo-2.txt: ocr foo-2.jpg
build foo.txt: cat foo-1.txt foo-2.txt
build bar-1.pdf bar-2.pdf bar-3.pdf: split bar.pdf
build bar-1.jpg: convert bar-1.pdf
build bar-1.txt: ocr bar-1.jpg
build bar-2.jpg: convert bar-2.pdf
build bar-2.txt: ocr bar-2.jpg
build bar-3.jpg: convert bar-3.pdf
build bar-3.txt: ocr bar-3.jpg
build bar.txt: cat bar-1.txt bar-2.txt bar-3.txt
```
