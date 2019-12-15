package com.melvic.contextual

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
    val tree = annottees.map(_.tree) match {
      case q"$mod def $func[..$tparams](...$params): $ret = $body" :: _ =>
        q"$mod def $func[..$tparams](...$params)(implicit ..$environment): $ret = $body"
      case expr => c.abort(c.enclosingPosition, s"Expected: function declaration. Got $expr")
    }
    c.Expr(tree)
  }

  def environment = c.prefix.tree match {
    case q"new contextual[$tparam]" => c.enclosingClass.children match {
      case Template(_, _, body) :: _ => body.find {
        case q"type $typeName = (..$params)" if typeName.toString == tparam.toString => true
        case _ => false
      } map {
        case q"type $typeName = (..$params)" => params.map {
          case Ident(typ: TypeName) => typeToParam(typ)
        }
      } getOrElse {
        c.abort(c.enclosingPosition, s"Could not find declaration: ${tparam.toString}")
      }
    }
    case expr => c.abort(c.enclosingPosition,
      s"Expected Type Param: type declaration (e.g. type Foo = (Bar, Baz)). Got $expr")
  }

  def typeToParam(typeName: TypeName) = {
    val paramName = typeName.decodedName.toString
    val termParam = TermName(paramName.head.toLower + paramName.tail)
    q"$termParam: $typeName"
  }
}
