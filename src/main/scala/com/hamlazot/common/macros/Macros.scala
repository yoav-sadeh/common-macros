package com.hamlazot.common.macros

import com.hamlazot.common.serialization.JsonSerializer
import org.json4s.Formats
import scala.language.experimental.macros


object Macros {

  import scala.reflect.macros.whitebox.Context

  object Mapper {
    implicit def materializeMappable[T]: Mapper[T] =
    macro materializeMappableImpl[T]

    def materializeMappableImpl[T: c.WeakTypeTag](c: Context): c.Expr[Mapper[T]] = {
      import c.universe._
      val tpe = weakTypeOf[T]
      val pred = tpe.asInstanceOf[scala.reflect.internal.Types#ClassTypeRef]
      val infoList = pred.pre.typeSymbol.info.members.toList
      val declarations = tpe.decls
      val ctor = declarations.collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }.get
      val fields = ctor.paramLists.head

      val toMapParams = fields.map { field =>
        val name = field.name.toTermName
        val mapKey: String = name.decodedName.toString

        val realTypeName = if (field.info.resultType.typeSymbol.isAbstract) {

          val info = infoList.find(m => m.nameString == field.info.resultType.typeSymbol.name.toString).get.info
          val fieldTypeSymbol = info.typeSymbol
          fieldTypeSymbol.name
        } else {
          TypeName(tpe.decl(name).typeSignature.typeSymbol.name.toString)
        }

        q"$mapKey -> t.$name"
      }

      val result = c.Expr[Mapper[T]] { q"""
      new Mapper[$tpe] {
        def toMap(t: $tpe) = Map(..$toMapParams)
      }
    """
      }
      result
    }


  }

  object Serializer {

    implicit def getSpecializedSerializer[T]: Serializer[T] =
    macro getSpecializedSerializerImpl[T]

    def getSpecializedSerializerImpl[T: c.WeakTypeTag](c: Context): c.Expr[Serializer[T]] = {
      import c.universe._

      val tpe = weakTypeOf[T]
      val surrogate = tpe.asInstanceOf[scala.reflect.internal.Types#ClassTypeRef]
      val infoList = surrogate.pre.typeSymbol.info.members.toList

      def getTypeName[T: c.WeakTypeTag](field: c.universe.Symbol): TypeName = {
        val typeName = field.info.typeSymbol.asType.name
        val resultType = if (field.info.resultType.typeSymbol.isAbstract) {
          val info = infoList.find(m => m.nameString == field.info.resultType.typeSymbol.name.toString).get.info
          TypeName(info.typeSymbol.asType.name.toString)
        } else {
          typeName
        }
        resultType
      }

      def getTypeArgs[T: c.WeakTypeTag](field: c.universe.Symbol): List[TypeName] = {
        if (field.info.resultType.typeSymbol.isAbstract) {
          val info = infoList.find(m => m.nameString == field.info.resultType.typeSymbol.name.toString).get
          info.typeSignature.typeArgs.map(t => TypeName(t.typeSymbol.name.toString))
        } else {
          field.info.typeArgs.map(t => TypeName(t.typeSymbol.name.toString))
        }
      }

      def isGeneric[T: c.WeakTypeTag](field: c.universe.Symbol): Boolean = {
        if (field.info.resultType.typeSymbol.isAbstract) {
          val info = infoList.find(m => m.nameString == field.info.resultType.typeSymbol.name.toString).get.info
          info.typeConstructor.takesTypeArgs
        }else {
          field.info.resultType.takesTypeArgs
        }
      }

      def nonGenericTemplate(i: Int, realTypeName: c.universe.TypeName): c.universe.TypeApply = {
        TypeApply(
          Select(
            Select(
              Apply(Ident(TermName("json")),
                List(Literal(Constant(i)))),
              TermName("camelizeKeys")),
            TermName("extract")),
          List(Ident(TypeName(realTypeName.toString))))
      }

      def genericTemplate(i: Int, typeName: c.universe.TypeName, typeArgs: List[TypeName]): c.universe.TypeApply = {
        TypeApply(
          Select(
            Select(
              Apply(Ident(TermName("json")),
                List(Literal(Constant(i)))),
              TermName("camelizeKeys")),
            TermName("extract")),
          List(
            AppliedTypeTree(Ident(TypeName("Map")), typeArgs.map(arg => Ident(arg)))
          )
        )
      }

      try {

        val ctor = tpe.decls.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m
        }.get

        val fields = ctor.paramLists.flatten

        val extractors = fields.view.zipWithIndex map { case (field: Symbol, i: Int) =>
          val name = field.asTerm.name
          val typeName = field.info.typeSymbol.asType.name
          val mapKey: String = name.decodedName.toString
          val realTypeName = getTypeName(field)

          if(isGeneric(field)){
            genericTemplate(i, realTypeName, getTypeArgs(field))
          }else{
            nonGenericTemplate(i, realTypeName)
          }

        } toList

        val deserializeExpr = Apply(Select(Ident(TermName(surrogate.pre.typeSymbol.name.toString)), TermName(tpe.typeSymbol.asClass.name.toString)),
          extractors)

        val result = c.Expr[Serializer[T]] { q"""
      new Serializer[$tpe] {
        def deserializ(jsonStr: String): $tpe = {
        val json = parse(jsonStr).asInstanceOf[JObject].children

        val result = $deserializeExpr
        result
      }
      }
    """
        }

        //println(showRaw(deserializeExpr))
        result

      } catch {
        case any: Throwable =>
          println(s"excepzione: $any")
          null
      }


    }

  }

//  object TypePath {
//    implicit def isInnerClass[T]: Boolean = macro isInnerClassImpl[T]
//
//    def isInnerClassImpl[T: c.WeakTypeTag](c: Context): c.Expr[Boolean] = {
//      import c.universe._
//      val tpe = weakTypeOf[T]
//
//      val inner = tpe.typeSymbol.info.termSymbol.isModule
//      //println(s"info: ${tpe.typeSymbol}")
//      //println(s"inner: $inner")
//      c.Expr[Boolean] { q"""$inner"""}
//    }
//  }

  trait Mapper[T] {
    def toMap(t: T): Map[String, Any]

    //def fromMap(map: Map[String, Any]): T
  }


  trait Serializer[T] extends JsonSerializer {


    var formats: Option[Formats] = None


    override implicit def jsonFormat: Formats = formats.getOrElse(super.jsonFormat)

    def deserializ(jsonStr: String): T
  }

}

