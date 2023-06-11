:- module(ninja, [variable//1, variable//2, rule//2, rule//3, build//3, build//4,
                  deps//1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(option)).
:- use_module(library(error)).

/** <module> Ninja build system generator

This module contains helper dcg predicates to generate ninja build files akin to the [`ninja_syntax.py`](https://github.com/ninja-build/ninja/blob/master/misc/ninja_syntax.py) python module distributed by ninja.
You can use these predicates if you want to generate your own `build.ninja` build file.

Example usage:

    build_graph -->
      rule(cp, "cp $in $out"),
      build(["input.txt"], cp, ["output.txt"]).

    main -->
      phrase(build_graph, L),
      open("build.ninja", write, Stream),
      string_codes(S, L),
      write(Stream, S),
      close(Stream).

Then `build.ninja` contains the following build specification:

    rule cp
      command = cp $in $out

    build input.txt: cp output.txt

See the [ninja build format documentation](https://ninja-build.org/manual.html) for generating more complex build files.

@author Kwon-Young Choi
@license GPL
*/

maybe_atom(NameAtom, Name) :-
  (  atom(NameAtom)
  -> Name = atom(NameAtom)
  ;  Name = NameAtom
  ).

identity(X) -->
  X.

indents(List) -->
  indents(identity, List).

:- meta_predicate indents(3, ?, ?, ?).
indents(Goal, List) -->
  indents_(List, Goal).
indents_([], _) --> [].
indents_([H | T], Goal) -->
  sequence(indent(Goal), [H | T]).
indent(Goal, X) -->
  "  ", call(Goal, X).

ws(List) -->
  sequence(identity, " ", List).
ws(Start, List) -->
  ws_(List, Start).
ws_([], _) -->
  [].
ws_([H | T], Start) -->
  sequence(Start, identity, " ", "", [H | T]).

%!  variable(+Pair:pair)// is semidet.
%
%   Generate a variable declaration from a pair Pair.
%   The key can be an atom or a dcg, value should be a dcg.
%   A variable definition always end with a new line.
%
%       ?- phrase(variable(name-"Value"), L), format("~s", [L]).
%       name = Value
%       L = [110, 97, 109, 101, 32, 61, 32, 86, 97|...].
%
variable(Pair) -->
  { must_be(pair, Pair), Pair = Name-Value },
  variable(Name, Value).

%!  variable(+Name:(atom;dcg), +Value:dcg)// is semidet.
%
%   Generate a variable declaration as a variable Name with value Value.
%   The variable name can be an atom or a dcg, value should be a dcg.
%   A variable definition always end with a new line.
%
%       ?- phrase(variable(name, "Value"), L), format("~s", [L]).
%       name = Value
%       L = [110, 97, 109, 101, 32, 61, 32, 86, 97|...].
%
variable(NameAtom, Value) -->
  { maybe_atom(NameAtom, Name) },
  ws([Name, "=", Value]), eol.

%!  rule(+Name:(atom;dcg), +Command:dcg)// is semidet.
%
%   Generate a rule declaration with no additional variables.
%   The name can be an atom or a dcg, the command should be dcg.
%
%       ?- phrase(rule(cp, "cp $in $out"), L), format("~s", [L]).
%       rule cp
%         command = cp $in $out
%       L = [114, 117, 108, 101, 32, 99, 112, 10, 32|...].
%
rule(Name, Command) -->
  rule(Name, Command, []).

%!  rule(+Name:(atom;dcg), +Command:dcg, +Variables:list(pair))// is semidet.
%
%   Generate a rule declaration with no additional variables.
%   The name can be an atom or a dcg.
%   The command should be dcg and describe the command to run.
%   Variables is a list and will be generate using the variable//1 rule.
%
%       ?- phrase(rule(cp, "cp $in $out"), L), format("~s", [L]).
%       rule cp
%         command = cp $in $out
%       L = [114, 117, 108, 101, 32, 99, 112, 10, 32|...].
%
rule(NameAtom, Command, Variables) -->
  { maybe_atom(NameAtom, Name) },
  "rule ", Name, eol,
  indents(variable, [command-Command | Variables]).

%!  build(+Outs:list(dcg), +Rule:(atom;dcg), +Ins:list(dcg))// is semidet.
%
%   Generate a build statement between input Ins and Output Outs with the rule Rule.
%
%       ?- phrase(build(["input.txt"], cp, ["output.txt"]), L), format("~s", [L]).
%       build input.txt: cp output.txt
%       L = [98, 117, 105, 108, 100, 32, 105, 110, 112|...].
%
build(Outs, Rule, Ins) -->
  build(Outs, Rule, Ins, []).

%!  build(+Outs:list(dcg), +Rule:(atom;dcg), +Ins:list(dcg), +Options:list)// is semidet.
%
%   Generate a build statement between input Ins and Output Outs with the rule Rule.
%   Optional arguments can be specify in the option list Options.
%   Valid options are:
%
%   * implicit_ins(ImplicitIns:list(dcg))
%     List of implicit dependencies, as list of dcgs.
%   * implicit_outs(ImplicitOuts:list(dcg))
%     List of implicit outputs, as list of dcgs.
%   * orderonly_ins(OrderonlyIns:list(dcg))
%     List of order only dependencies, as list of dcgs.
%   * validations(Validations:list(dcg))
%     List of validation targets, as list of dcgs.
%   * variables(Variables:list(pair))
%     List of variables as pairs.
%
%         ?- phrase(build(["input.txt"], cp, ["output.txt"],
%              [implicit_outs(["implicit_out.txt"]),
%               implicit_ins(["implicit_in.txt"]),
%               orderonly_ins(["orderonly_in.txt"]),
%               validations(["validation.txt"]),
%               variables([name-"value"])]), L), format("~s", [L]).
%         build input.txt | implicit_out.txt: cp output.txt | implicit_in.txt || orderonly_in.txt |@ validation.txt
%           name = value
%         L = `build input.txt | implici...alue\n`.
%
build(Outs, RuleAtom, Ins, Options) -->
  {
    must_be(list, Outs),
    (  Outs = []
    -> domain_error(nonempty_list, Outs)
    ;  true
    ),
    maybe_atom(RuleAtom, Rule)
  },
  ws("build ", Outs),
  { option(implicit_outs(ImplicitOuts), Options, []) },
  ws(" | ", ImplicitOuts),
  ws((": ", Rule, " "), Ins),
  { option(implicit_ins(ImplicitIns), Options, []) },
  ws(" | ", ImplicitIns),
  { option(orderonly_ins(OrderonlyIns), Options, []) },
  ws(" || ", OrderonlyIns),
  { option(validations(Validations), Options, []) },
  ws(" |@ ", Validations),
  eol,
  { option(variables(Variables), Options, []) },
  indents(variable, Variables).

%!  deps(++Source:string)// is semidet.
%
%   Generate a whitespace separated list of dependencies from prolog source file Source.
%   It also include Source as a dependency.
%
%       ?- phrase(deps("ninja.pl"), L), format("~s", [L]).
%       ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl
%       L = `ninja.pl /usr/lib64/swipl...or.pl`.
%
deps(Source) -->
  Source, " ",
  { xref_source(Source) },
  foreach(xref_uses_file(Source, _, Path), atom(Path), " ").

:- begin_tests(ninja).

test(maybe_atom, [true(Name == atom(name))]) :-
  maybe_atom(name, Name).

test(maybe_atom, [true(Name == NewName)]) :-
  maybe_atom(Name, NewName).

test(indents, [true(S == "  toto  tata")]) :-
  phrase(indents(["toto", "tata"]), L),
  string_codes(S, L).

test(ws, [true(S == "a b")]) :-
  phrase(ws(["a", "b"]), L),
  string_codes(S, L).

test(ws, [true(S == "start: a b")]) :-
  phrase(ws("start: ", ["a", "b"]), L),
  string_codes(S, L).

test(ws, [true(S == "")]) :-
  phrase(ws("start: ", []), L),
  string_codes(S, L).

test(variable, [true(S == "name = value\n")]) :-
  phrase(variable(name-"value"), L),
  string_codes(S, L).

test(variable, [true(S == "name = value\n")]) :-
  phrase(variable(name, "value"), L),
  string_codes(S, L).

test(variable, [true(S == "name = value\n")]) :-
  phrase(variable("name", "value"), L),
  string_codes(S, L).

test(rule, [true(S == "rule cp\n  command = cp $in $out\n")]) :-
  phrase(rule(cp, "cp $in $out"), L),
  string_codes(S, L).

test(rule, [true(S == "rule cp\n  command = cp $in $out\n")]) :-
  phrase(rule("cp", "cp $in $out"), L),
  string_codes(S, L).

test(rule, [true(S == "rule cp\n  command = cp $in $out\n  a = b\n")]) :-
  phrase(rule("cp", "cp $in $out", [a-"b"]), L),
  string_codes(S, L).

test(build, [true(S == "build input.txt: cp output.txt\n")]) :-
  phrase(build(["input.txt"], cp, ["output.txt"]), L),
  string_codes(S, L).

test(build, [true(S == "build input.txt | implicit_out.txt: cp output.txt | implicit_in.txt || orderonly_in.txt |@ validation.txt\n  name = value\n")]) :-
  phrase(build(["input.txt"], cp, ["output.txt"], [
    implicit_outs(["implicit_out.txt"]),
    implicit_ins(["implicit_in.txt"]),
    orderonly_ins(["orderonly_in.txt"]),
    validations(["validation.txt"]),
    variables([name-"value"])]), L),
  string_codes(S, L).

test(deps, [true(S == "ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl")]) :-
  phrase(deps("ninja.pl"), L),
  string_codes(S, L).

:- end_tests(ninja).
