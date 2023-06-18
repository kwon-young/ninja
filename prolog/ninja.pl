:- module(ninja, [variable//1, variable//2, rule//2, rule//3, build//3, build//4,
                  deps//1, generator//1, generator//2, seq//1,
                  write_build/1, write_build/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(option)).
:- use_module(library(error)).

/** <module> Ninja build system generator

This module contains helper dcg predicates to generate ninja build files akin to the [`ninja_syntax.py`](https://github.com/ninja-build/ninja/blob/master/misc/ninja_syntax.py) python module distributed by ninja.
You can use these predicates if you want to generate your own `build.ninja` build file.

Example usage:

    :- use_module(prolog/ninja).
    :- initialization(write_build(build_graph), main).

    build_graph -->
      rule(cp, "cp $in $out"),
      build("input.txt", cp, "output.txt").

Then `build.ninja` contains the following build specification:

    rule generate
      command = swipl $in
      generator = 1
    build build.ninja: generate /home/kwon-young/prog/ninja/build.pl | /home/kwon-young/prog/ninja/build.pl /home/kwon-young/prog/ninja/prolog/ninja.pl
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
  (  { is_list(List) }
  -> indents_(List, Goal)
  ;  indents_([List], Goal)
  ).
indents_([], _) --> [].
indents_([H | T], Goal) -->
  sequence(indent(Goal), [H | T]).
indent(Goal, X) -->
  "  ", call(Goal, X).

ws(List) -->
  ws("", List).
ws(Start, List) -->
  (  { is_list(List) }
  -> ws_(List, Start)
  ;  ws_([List], Start)
  ).
ws_([], _) -->
  [].
ws_([H | T], Start) -->
  sequence(Start, identity, " ", "", [H | T]).

%!  variable(+Pair:pair)// is det.
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

%!  variable(+Name:(atom;dcg), +Value:dcg)// is det.
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

%!  rule(+Name:(atom;dcg), +Command:dcg)// is det.
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

%!  rule(+Name:(atom;dcg), +Command:dcg, +Variables:list(pair))// is det.
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

%!  build(+Outs:list(dcg), +Rule:(atom;dcg), +Ins:list(dcg))// is det.
%
%   Generate a build statement between input Ins and Output Outs with the rule Rule.
%
%       ?- phrase(build(["input.txt"], cp, ["output.txt"]), L), format("~s", [L]).
%       build input.txt: cp output.txt
%       L = [98, 117, 105, 108, 100, 32, 105, 110, 112|...].
%
build(Outs, Rule, Ins) -->
  build(Outs, Rule, Ins, []).

%!  build(+Outs:(list(dcg);dcg), +Rule:(atom;dcg), +Ins:(list(dcg);dcg), +Options:list)// is det.
%
%   Generate a build statement between input Ins and output Outs with the rule Rule.
%   There should be at least one output.
%   You can omit using a list of single element arguments.
%   Optional arguments can be specified in the option list Options.
%   Valid options are:
%
%   * implicit_ins(ImplicitIns:(list(dcg);dcg))
%     List of implicit dependencies, as list of dcgs.
%   * implicit_outs(ImplicitOuts:(list(dcg);dcg))
%     List of implicit outputs, as list of dcgs.
%   * orderonly_ins(OrderonlyIns:(list(dcg);dcg))
%     List of order only dependencies, as list of dcgs.
%   * validations(Validations:(list(dcg);dcg))
%     List of validation targets, as list of dcgs.
%   * variables(Variables:(list(pair);pair))
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

%!  deps(++Source:string)// is det.
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
  foreach((xref_uses_file(Source, Spec, Dep), (Dep = '<not_found>' -> Spec = Path ; Dep = Path)), atom(Path), " ").

%!  generator(+Input:string)// is det.
%   
%   Generate a generator rule and build edge with Input as the prolog generator script.
%   The build edge generated will also contains all prolog files that depends on Input.
%   See the [ninja documentation](https://ninja-build.org/manual.html#ref_rule) for such rule.
%   The output filename will be named "build.ninja" as traditional.
%
%       ?- phrase(generator("example.pl"), format("~s", [L]).
%       rule generate
%         command = swipl $in
%         generator = 1
%       build build.ninja: generate example.pl | example.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /home/kwon-young/prog/ninja/prolog/ninja.pl
%
generator(Input) -->
  generator("build.ninja", Input).

%!  generator(+Output:string, +Input:string)// is det.
%   
%   Generate a generator rule and build edge with Input as the prolog generator script and Output as the ninja build file.
%   The build edge generated will also contains all prolog files that depends on Input.
%   See the [ninja documentation](https://ninja-build.org/manual.html#ref_rule) for such rule.
%
%       ?- phrase(generator("example.pl"), format("~s", [L]).
%       rule generate
%         command = swipl $in
%         generator = 1
%       build build.ninja: generate example.pl | example.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /home/kwon-young/prog/ninja/prolog/ninja.pl
%
generator(Output, Input) -->
  rule(generate, "swipl $in", [generator-"1"]),
  build([Output], generate, [Input], [implicit_ins([deps(Input)])]).

%!  seq(+Goals:list(dcg))// is det.
%   
%   Experimental attempt to ease the chaining of sequential build edges.
%   This predicate allows to specify a list a build edges without repeating the inputs of a build edge with the output of the previous build edge.
%   However, the first build edge have to have its input specified.
%
%       ?- phrase(seq([
%         build("foo.jpg", convert, "foo.pdf"),
%         build("foo.txt", ocr)]), L), format("~s", [L]).
%       build foo.jpg: convert foo.pdf
%       build foo.txt: ocr foo.jpg
%
%   Notice how "foo.jpg" is specified as input to the ocr build edge although we did not specified as input in the seq rule.
%   You can still specify options after the rule name in the build rule.
%
seq([Goal | Goals]) -->
  { compound_name_arguments(Goal, build, [Outputs | _]) },
  Goal,
  seq(Goals, Outputs).
seq([], _) -->
  [].
seq([Goal | Goals], Inputs) -->
  (  { compound_name_arguments(Goal, build, [Outputs, _Name]) }
  -> call(Goal, Inputs)
  ;  { compound_name_arguments(Goal, build, [Outputs, Name, Options]) }
  -> call(build, Outputs, Name, Inputs, Options)
  ;  { existence_error(procedure, Goal) }
  ),
  seq(Goals, Outputs).

:- meta_predicate write_build(2).

%!  write_build(+Goal:dcg) is det.
%
%   Generate a "build.ninja" file using the dcg Goal.
%   This will also automatically add a generator rule as specified in generator/2 for the script containing Goal.
%   You can use this goal as with initialization/2 directive to automatically generate your ninja build file when calling your script with swipl.
%
%   With the following prolog script "build.pl":
%
%       :- use_module(library(ninja)).
%       :- initialization(write_build(build_graph), main).
%
%       build_graph -->
%         rule(cp, "cp $in $out"),
%         build(["input.txt"], cp, ["output.txt"]).
%
%   When called on the command line like this:
%
%       $ swipl build.pl
%
%   Will generate the following "build.ninja" file:
%
%       rule generate
%         command = swipl $in
%         generator = 1
%       build build.ninja: generate /home/kwon-young/prog/ninja/build.pl | /home/kwon-young/prog/ninja/build.pl /home/kwon-young/prog/ninja/prolog/ninja.pl
%       rule cp
%         command = cp $in $out
%       build input.txt: cp output.txt
%
write_build(Goal) :-
  write_build(Goal, "build.ninja").

:- meta_predicate write_build(2, ?).

%!  write_build(+Goal:dcg, Output) is det.
%
%   See write_build/1.
%   Output specify the name of the ninja build file.
write_build(Mod:Goal, Output) :-
  (  atom(Goal)
  -> Head =.. [Goal, _, _]
  ;  compound_name_arguments(Goal, Name, Args),
     append(Args, [_, _], NewArgs),
     Head =.. [Name | NewArgs]
  ),
  predicate_property(Mod:Head, file(AtomFile)),
  atom_string(AtomFile, File),
  phrase((generator(File), Mod:Goal), L),
  open(Output, write, Stream),
  string_codes(S, L),
  write(Stream, S),
  close(Stream).

:- begin_tests(ninja).

test(maybe_atom, [true(Name == atom(name))]) :-
  maybe_atom(name, Name).

test(maybe_atom, [true(Name == NewName)]) :-
  maybe_atom(Name, NewName).

test(indents, [true(S == "  toto  tata")]) :-
  phrase(indents(["toto", "tata"]), L),
  string_codes(S, L).

test(indents, [true(S == "  toto")]) :-
  phrase(indents("toto"), L),
  string_codes(S, L).

test(ws, [true(S == "a b")]) :-
  phrase(ws(["a", "b"]), L),
  string_codes(S, L).

test(ws, [true(S == "a")]) :-
  phrase(ws("a"), L),
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
  phrase(build("input.txt", cp, "output.txt"), L),
  string_codes(S, L).

test(build, [true(S == "build input.txt | implicit_out.txt: cp output.txt | implicit_in.txt || orderonly_in.txt |@ validation.txt\n  name = value\n")]) :-
  phrase(build(["input.txt"], cp, ["output.txt"], [
    implicit_outs(["implicit_out.txt"]),
    implicit_ins(["implicit_in.txt"]),
    orderonly_ins(["orderonly_in.txt"]),
    validations(["validation.txt"]),
    variables([name-"value"])]), L),
  string_codes(S, L).

test(deps, [true(S == "prolog/ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl")]) :-
  phrase(deps("prolog/ninja.pl"), L),
  string_codes(S, L).

test(generator, [true(S == "rule generate\n  command = swipl $in\n  generator = 1\nbuild build.ninja: generate prolog/ninja.pl | prolog/ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl\n")]) :-
  phrase(generator("prolog/ninja.pl"), L),
  string_codes(S, L).

test(generator, [true(S == "rule generate\n  command = swipl $in\n  generator = 1\nbuild toto.ninja: generate prolog/ninja.pl | prolog/ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl\n")]) :-
  phrase(generator("toto.ninja", "prolog/ninja.pl"), L),
  string_codes(S, L).

test(seq, [true(S == "build foo.jpg: convert foo.pdf\nbuild foo.txt: ocr foo.jpg\n")]) :-
  phrase(seq([
    build("foo.jpg", convert, "foo.pdf"),
    build("foo.txt", ocr)]), L),
  string_codes(S, L).

test(seq, [true(S == "build foo.jpg: convert foo.pdf\nbuild foo.txt: ocr foo.jpg\n  a = b\n")]) :-
  phrase(seq([
    build("foo.jpg", convert, "foo.pdf"),
    build("foo.txt", ocr, [variables([a-"b"])])]), L),
  string_codes(S, L).

test(write_build, [true(S == "rule generate\n  command = swipl $in\n  generator = 1\nbuild build.ninja: generate /home/kwon-young/prog/ninja/prolog/ninja.pl | /home/kwon-young/prog/ninja/prolog/ninja.pl /usr/lib64/swipl-9.0.4/library/dcg/basics.pl /usr/lib64/swipl-9.0.4/library/dcg/high_order.pl /usr/lib64/swipl-9.0.4/library/option.pl /usr/lib64/swipl-9.0.4/library/error.pl\n")]) :-
  tmp_file_stream(text, File, Stream),
  close(Stream),
  write_build(ninja:identity([]), File),
  read_file_to_string(File, S, []),
  delete_file(File).

:- end_tests(ninja).
