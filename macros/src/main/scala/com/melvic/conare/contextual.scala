package com.melvic.conare

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Generates implicit parameters for the annottee based on a given type.
 * If the macro can locate the definition of the input type, it will deconstruct
 * the right-hand-side of the definition and provide implicits for the parts.
 * Otherwise, an implicit parameter is generated for the type itself.
 *
 * Tuples are deconstructed into multiple parts (but not recursively) and implicits
 * are provided for each part.
 *
 * If the RHS of a definition is a function, or the type parameter itself is, its return
 * type might override the return type of the annottee, unless the annottee explicitly
 * provides a return type, in which case a currying will occur.
 *
 * @tparam A The type on which the generated environment is based.
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class contextual[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ContextualMacro.impl[A]
}

class ContextualMacro(val c: whitebox.Context) {
  import c.universe._

  def impl[A](annottees: c.Expr[Any]*): c.Expr[Any] = {
    val tree = annottees.map(_.tree) match {   /*_*/
      case q"$mod def $func[..$tparams](...$params): $ret = $body" :: _ =>
        val (paramsDecl, envRet) = environment
        val retTree = constructReturnType(envRet, ret)
        q"$mod def $func[..$tparams](...$params)(implicit ..$paramsDecl): $retTree = $body"
      case expr :: _ => c.abort(c.enclosingPosition,
        s"Expected: Function or class declaration. Got $expr")
    }   /*_*/

    c.Expr(tree)
  }

  /**
   * Constructs the environment based on the type parameter of the annotation.
   * @return The parameter declarations together with the return type.
   */
  def environment = c.prefix.tree match {
    case q"new contextual[$tparam[..$a]]" =>
      def correctTypeName: TypeName => Boolean = _.decodedName.toString == tparam.toString

      c.enclosingClass.children match {   /*_*/
        case Template(_, _, body) :: _ => body.flatMap {
          case q"type $typeName[..$tps] = (..$params) => $ret" if correctTypeName(typeName) =>
            Some((constructTermParams(params, tps, a), ret))
          case q"type $typeName[..$tps] = (..$params)" if correctTypeName(typeName) =>
            Some((constructTermParams(params, tps, a), EmptyTree))
          case q"case class $className[..$tps](..$paramDecls)" if correctTypeName(className) =>
            Some((constructTermParams(paramDecls, tps, a), EmptyTree))
          case _ => None
        }.headOption getOrElse {
          // Could not find declaration. Deconstruct the type directly
          // and construct a new env param from it.
          tq"$tparam[..$a]" match {
            case tq"(..$tparams) => $ret" =>
              (constructTermParams(tparams, Nil, Nil), ret)
            case tq"(..$tparams)" =>
              (constructTermParams(tparams, Nil, Nil), EmptyTree)
            case q"$tparam" =>
              (constructTermParams(List(tparam), Nil, Nil), EmptyTree)
          }
        }
      }   /*_*/
    case expr => c.abort(c.enclosingPosition,
      s"Expected Type Param: type declaration (e.g. type Foo = (Bar, Baz)). Got $expr")
  }

  /**
   * Constructs the declarations of the parameters.
   */
  def constructTermParams(params: List[Tree], tparams: List[Tree], targs: List[Tree]) = {
    implicit lazy val paramsAndArgs: List[(Tree, Tree)] = tparams zip targs

    params map {
      case Ident(typeName: TypeName) =>
        val (paramName, resultType) = substituteTArgs(typeName)
        q"$paramName: $resultType"

      // If the context is a named declaration (i.e. from a case class),
      // ignore the generated parameter name and reused the declared one.
      case q"$mod val $param: ${Ident(name: TypeName)}" =>
        val (_, resultType) = substituteTArgs(name)
        q"$param: $resultType"

      // If the parameter type is itself a type constructor, perform
      // substitutions on its own type parameters.
      case tq"$tc[..$tcParams]" =>
        val Ident(tcTypeName: TypeName) = tc
        val newTCParams = tcParams.map {
          case Ident(name: TypeName) => substituteTArgs(name)._2
        }
        val tcName = lowerCamelFormat(tcTypeName.decodedName.toString)
        val tcParamNames = newTCParams.map { _.decodedName.toString }
        val tcTerm = TermName(s"${tcName}Of${tcParamNames.mkString}")
        q"$tcTerm: $tc[..$newTCParams]"

      case q"$mod val $tcDecl: $tc[..$tcTParams]" =>
        val newTCParams = tcTParams.map {
          case Ident(name: TypeName) => substituteTArgs(name)._2
        }
        q"$tcDecl: $tc[..$newTCParams]"
      case expr => error(expr, "Invalid param format")
    }
  }

  def substituteTArgs(typeName: TypeName)(implicit
      paramsAndArgs: List[(Tree, Tree)]) = {
    val typeNameString = typeName.decodedName.toString
    def noSubstitute = (lowerCamelFormat(typeNameString), typeName)

    // Check if the type name is one of the environment's type params. If so,
    // use the name of the corresponding argument, unless the argument is a "skip"
    // command (denoted by `~>`). Otherwise, use the type's name.
    val (paramName, resultType) = paramsAndArgs.find { case (tparam: TypeDef, _) =>
      tparam.name.decodedName.toString == typeNameString
    }.map {
      case (_, Ident(targ: TypeName)) =>
        val targName = targ.decodedName.toString
        val skipName = weakTypeOf[~>].typeSymbol.name.decodedName.toString
        if (targName == skipName) noSubstitute
        else (lowerCamelFormat(targName), targ)
    } getOrElse noSubstitute

    (TermName(paramName), resultType)
  }

  def constructReturnType: (Tree, Tree) => Tree = {
    case (EmptyTree, ret) => ret
    case (envRet, ret) if ret.isEmpty => envRet

    // If both the environment and the annottee's return types
    // are provide, construct a new function where the two of
    // them are the edges, turning the annottee into a curried
    // function.
    case (envRet, funcRet) => tq"$envRet => $funcRet"
  }

  def lowerCamelFormat(paramName: String) =
    paramName.head.toLower + paramName.tail

  def error(expr: Tree, message: String) =
    c.abort(c.enclosingPosition, s"$message: ${showRaw(expr)}")
}
