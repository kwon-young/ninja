:- use_module(library(dcg/basics)).
:- use_module(prolog/ninja).
:- ['pack.pl'].
:- initialization(write_build(graph), main).

zipfile -->
  { version(V) },
  "v", atom(V), ".zip".

graph -->
  rule(doc, "swipl -t halt -s load.pl -g doc && touch docs"),
  rule(test, "swipl -t halt -s load.pl -g test"),
  rule(example, "swipl $in"),
  rule(zip, "rm -rf ninja $out && mkdir ninja && cp -r $in ninja/ && zip -r $out ninja && rm -rf ninja"),
  build("test", test, "prolog/ninja.pl"),
  build(["docs", "docs/index.html"], doc, deps("load.pl"), [validations("test")]),
  build(["example.ninja"], example, ["example.pl"]),
  build(zipfile, zip, [
    "docs",
    "build.pl",
    "LICENSE",
    "load.pl",
    "pack.pl",
    "prolog/ninja.pl",
    "README.md",
    "example.pl",
    "example.ninja",
    ".gitignore"], [validations("test")]).
