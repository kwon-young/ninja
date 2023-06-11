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
