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
    type SkipEnv[A, B, C] = (A, B, C)
    
    // Function environments. The return type is specified in the environment.
    @contextual[FuncEnv]
    def showFuncEnv = s"Long Param: ${Env[Long]}. String Param: ${Env[String]}"
  
    // Type constructor environment
    @contextual[TypeConsEnv[String]]
    def showTCEnv: String = s"String Param: ${Env[String]}. Integer is ${Env[Int]}"
  
    // Skipped substitution for the second type param of the environment.
    // Note: Make sure the skipped type params are resolvable within the current
    // scope after the macro expansion.
    @contextual[SkipEnv[String, ~>, Int]]
    def showSkipped[B] = s"Skipped ${Env[B]}"
  
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
