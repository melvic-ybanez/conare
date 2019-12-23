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
      def correctTypeName: TypeName => Boolean = _.toString == tparam.toString

      c.enclosingClass.children match {   /*_*/
        case Template(_, _, body) :: _ => body.flatMap {
          case q"type $typeName[..$tps] = (..$params) => $ret" if correctTypeName(typeName) =>
            Some((constructTermParams(params, tps, a), ret))
          case q"type $typeName[..$tps] = (..$params)" if correctTypeName(typeName) =>
            Some((constructTermParams(params, tps, a), EmptyTree))
          case q"case class $className[..$tps](..$paramDecls)" if (correctTypeName(className)) =>
            Some((caseClassTermParams(paramDecls, tps, a), EmptyTree))
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
  def constructTermParams(params: List[Tree],
      tparams: List[Tree],
      targs: List[Tree],
      formatName: String => String = lowerCamelFormat) = {
    lazy val paramsAndArgs = tparams zip targs

    def construct(typeName: TypeName) = {
      val typeNameString = typeName.decodedName.toString
      def noSubstitute = (formatName(typeNameString), typeName)

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
          else (formatName(targName), targ)
      } getOrElse noSubstitute

      val termParam = TermName(paramName)
      (termParam, resultType)
    }

    params map {
      case Ident(typeName: TypeName) =>
        val (termParam, resultType) = construct(typeName)
        q"$termParam: $resultType"
      case tq"$tc[$tparam]" => tparam match {
        case Ident(name: TypeName) =>
          val (term @ TermName(paramName), resultType) = construct(name)
          val tcName = lowerCamelFormat(name.decodedName.toString)
          val tcTerm = TermName(s"${tcName}Of${paramName.capitalize}")
          q"$tcTerm: $tc[$resultType]"
      }
      case expr => error(expr, "Invalid param format")
    }
  }

  def caseClassTermParams(paramDecls: List[Tree], tparams: List[Tree], targs: List[Tree]) = {
    val params = paramDecls.map { case decl: ValDef => decl.tpt }
    constructTermParams(params, tparams, targs, { targName =>
      paramDecls.find { case decl: ValDef => decl.tpt match {
        case Ident(tparam: TypeName) => tparam.decodedName.toString == targName
        case tq"$typeCons[$tparam]" => tparam match { case ident: Ident =>
          ident.name.decodedName.toString == targName
        }
        case expr => error(expr, "Invalid param format")
      }} map {
        case decl: ValDef => decl.name.decodedName.toString
      } getOrElse(lowerCamelFormat(targName))
    })
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
