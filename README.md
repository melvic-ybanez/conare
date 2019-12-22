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
type FuncEnv = (Long, String) => Int
type TypeConsEnv[A] = (A, Int, List[A])
type SkipEnv[A, B, C] = (A, B, C)

case class Person(name: String, age: Int)
case class MyCons[A](head: A, tail: List[A])

// Function environments. The return type is specified in the environment.
@contextual[FuncEnv]
def showFuncEnv = {
  s"Long Param: ${Env[Long]}. String Param: ${Env[String]}"
  1000  // Return type is required to be int
}

// Type constructor Environments
@contextual[TypeConsEnv[String]]
def showTCEnv: String =
s"String Param: ${Env[String]}. Integer is ${Env[Int]}. List: ${Env[List[String]]}"

// Skipped substitution for the second type param of the environment.
// Note: Make sure the skipped type params are resolvable within the current
// scope after the macro expansion.
@contextual[SkipEnv[String, ~>, Int]]
def showSkipped[B] = s"Skipped ${Env[B]}"

@contextual[Person]
def showPerson = s"Name: ${Env[String]}. Age: ${Env[Int]}"

// Case class parameters can be accessed directly in the environment.
@contextual[Person]
def showPersonParams = s"Name (String): $name. Age (Int): $age"

@contextual[MyCons[String]]
def showCons = s"Head: ${Env[String]}. Tail: ${Env[List[String]]}"

@contextual[String]
def showString = s"Hello ${Env[String]}"

@contextual[(String, Int)]
def showTuple = s"String Param: ${Env[String]}. Integer is ${Env[Int]}"

@contextual[String => String]
def showFunc = s"Function type: ${Env[String]}"

// Curried type (subject for modification or removal)
@contextual[FuncEnv]
def showCurried: String = x => s"First Param: ${Env[Long]}. Curried int: $x"
```
