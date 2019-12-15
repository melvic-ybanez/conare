# conare
A utility for simulating the functionality of Dotty's context queries

A simple example:

```scala
type Foo = (String, Int)

@contextual[Foo]
def showFoo = s"String param: ${Env[String]}. Integer param: ${Env[Int]}"
```

If you apply `showFoo` to `"Hello"` and `5` (e.g. `showFoo("hello", 5)`), you'll get `"String param: hello. Integer param: 5"` as the result.

More information and enhancements will be added soon.
