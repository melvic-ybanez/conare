package com.melvic.conare

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import language.experimental.macros
import scala.reflect.macros.whitebox

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
      case expr => c.abort(c.enclosingPosition, s"Expected: function declaration. Got $expr")
    }   /*_*/

    c.Expr(tree)
  }

  def environment = c.prefix.tree match {
    case q"new contextual[$tparam]" =>
      def correctTypeName: TypeName => Boolean = _.toString == tparam.toString

      c.enclosingClass.children match {   /*_*/
        case Template(_, _, body) :: _ => body.flatMap {
          case q"type $typeName = (..$params) => $ret" if correctTypeName(typeName) =>
            Some(constructTermParams(params), ret)
          case q"type $typeName = (..$params)" if correctTypeName(typeName) =>
            Some((constructTermParams(params), EmptyTree))
          case _ => None
        }.headOption getOrElse {
          // Could not find declaration. Deconstruct the type directly
          // and construct a new env param from it.
          tparam match {
            case tq"(..$tparams) => $ret" => (constructTermParams(tparams), ret)
            case tq"(..$tparams)" => (constructTermParams(tparams), EmptyTree)
            case q"$tparam" => (constructTermParams(List(tparam)), EmptyTree)
          }
        }
      }   /*_*/

    case expr => c.abort(c.enclosingPosition,
      s"Expected Type Param: type declaration (e.g. type Foo = (Bar, Baz)). Got $expr")
  }

  /**
   * Constructs the declarations of the parameters.
   */
  def constructTermParams(params: List[Tree]) = params.map {
    case Ident(typeName: TypeName) =>
      val paramName = typeName.decodedName.toString
      val termParam = TermName(paramName.head.toLower + paramName.tail)
      q"$termParam: $typeName"
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
}
