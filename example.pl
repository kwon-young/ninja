:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(prolog/ninja).

:- initialization(write_build(graph, "example.ninja"), main).

stem_(foo).
stem_(bar).
stem(atom(Stem)) :-
  stem_(Stem).

page_(foo, 1).
page_(foo, 2).
page_(bar, 1).
page_(bar, 2).
page_(bar, 3).
page(atom(Stem), number(Page)) :-
  page_(Stem, Page).

pdf(Stem) -->
  Stem, ".pdf".

pdf_page(Stem, Page) -->
  Stem, "-", Page, ".pdf".

all_pages(Goal, Stem) -->
  foreach(page(Stem, Page), call(Goal, Stem, Page), " ").

image(Stem, Page) -->
  Stem, "-", Page, ".jpg".

ocr_page(Stem, Page) -->
  Stem, "-", Page, ".txt".

ocr(Stem) -->
  Stem, ".txt".

graph -->
  rule(split, "split $in $out"),
  rule(convert, "convert $in $out"),
  rule(ocr, "ocr $in $out"),
  rule(cat, "cat $in $out"),
  foreach(stem(Stem), (
    build(all_pages(pdf_page, Stem), split, pdf(Stem)),
    foreach(page(Stem, Page), seq([
      build(image(Stem, Page), convert, pdf_page(Stem, Page)),
      build(ocr_page(Stem, Page), ocr, [implicit_ins("toto")])
      ])),
    build(ocr(Stem), cat, all_pages(ocr_page, Stem)))).
