doc :-
  consult("prolog/ninja.pl"),
  doc_save(., [recursive(true), doc_root("docs")]).

test :-
  consult("prolog/ninja.pl"),
  run_tests.
