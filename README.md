# Conare
A utility for simulating the functionality of Dotty's context queries.

A simple example:

```scala
type Dependencies = (String, Int)

@contextual[Dependencies]
def withStringAndIntEnvs = s"String param: ${Env[String]}. Integer param: ${Env[Int]}"
```

The `contextual` macro will generate implicit parameters of types `String` and `Int`. So the following will work:

```scala
implicit val string: String = "Hello"
implicit val int: Int = 10

println(withStringAndIntEnvs)   // should print "String param: Hello. Integer param: 10"
```

The types of the generated implicit parameters are based on the types specified in the right-hand-side of the environment's definition. 

More examples:
```scala
  type FuncEnv = (Long, String) => String
  type TypeConsEnv[A] = (A, Int)

  // Function environments. The return type is specified in the environment.
  @contextual[FuncEnv]
  def showFuncEnv = s"Long Param: ${Env[Long]}. String Param: ${Env[String]}"

  // Type constructor
  @contextual[TypeConsEnv[String]]
  def showTCEnv: String = s"String Param: ${Env[String]}. Integer is ${Env[Int]}."

  @contextual[String]
  def showString = s"Hello ${Env[String]}"

  @contextual[(String, Int)]
  def showTuple = s"String Param: ${Env[String]}. Integer is ${Env[Int]}"

  @contextual[String => String]
  def showFunc = s"Function type: ${Env[String]}"

  // Curried type (subject for modification or removal)
  @contextual[FuncEnv]
  def showCurried: String = str =>
    s"First string: ${Env[String]}. Second string: ${Env[String]}"
```
