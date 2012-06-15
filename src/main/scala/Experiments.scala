import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def implement[T] = macro ExperimentsImpl.implement[T]
}

object ExperimentsImpl {
  
  def implement[T: c.TypeTag](c: Context): c.Expr[T] = {
    import c.mirror._
    import c.universe._
    import Flag._

    // Convert a methodType into its ultimate result type
    // For nullary and normal methods, this is just the result type
    // For curried methods, this is the final result type of the result type
    def finalResultType(methodType: Type): Type = methodType match {
      case NullaryMethodType(result) => result 
      case MethodType(_, result) => finalResultType(result)
      case PolyType(_, result) => finalResultType(result)
      case _ => methodType
    }
    
    // Convert a methodType into a list of lists of params:
    // UnaryMethodType => Nil
    // Normal method => List(List(p1, p2, ...))
    // Curried method => List(List(p1, p2, ...), List(q1, q2, ...), ...)
    def paramss(methodType: Type): List[List[Symbol]] = methodType match {
      case MethodType(params, result) => params :: paramss(result)
      case PolyType(_, result) => paramss(result)
      case _ => Nil
    }
    
    def buildParams(methodType: Type) =
      paramss(methodType) map { params =>
        params map { p =>
          val paramType = p.typeSignatureIn(methodType).typeSymbol
          val paramTypeTree = 
            if (paramType hasFlag PARAM)
              Ident(newTypeName(paramType.name.toString))
            else
              Ident(paramType)
              
          ValDef(
            Modifiers(PARAM),
            newTermName(p.name.toString),
            paramTypeTree,
            EmptyTree)
        }
      }
    
    def buildTypeParams(methodType: Type) =
      methodType.typeParams map { t => 
        TypeDef(
          Modifiers(PARAM),
          newTypeName(t.name.toString), 
          List(), 
          TypeBoundsTree(Ident(staticClass("scala.Nothing")), Ident(staticClass("scala.Any"))))
      }
    
    // def <|name|>(p1: T1, p2: T2, ...): T = null.asInstanceOf[T]
    def methodDef(name: Name, methodType: Type): DefDef = {
      val params = buildParams(methodType)
      val body = TypeApply(
                   Select(Literal(Constant(null)), newTermName("asInstanceOf")), 
                   List(TypeTree().setType(finalResultType(methodType)))) 
      DefDef(
        Modifiers(),
        name, 
        buildTypeParams(methodType), 
        params,
        TypeTree(),
        body)
    }
    
    def methodImpl(m: Symbol, t: Type): DefDef = {
      val mt = m.typeSignatureIn(t) 
      mt match {
        case NullaryMethodType(_) => methodDef(m.name, mt)
        case MethodType(_, _) => methodDef(m.name, mt)
        case PolyType(_, _) => methodDef(m.name, mt)
        case _ => sys.error("Don't know how to handle "+ mt.getClass)
      }
    }
    
    // def <init>() = { super.<init>(); () }
    def initDef = 
      DefDef(
        Modifiers(), 
        newTermName("<init>"), 
        List(), 
        List(List()), 
        TypeTree(),
        Block(
          List(
            Apply(
              Select(Super(This(newTypeName("")), newTypeName("")), newTermName("<init>")), 
              List())), 
          Literal(Constant(()))))
      
    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol

    // { final class $anon extends T { ... }; new $anon() }.asInstanceOf[T])
    def anonClass(t: Type) = {
      val methodsToImplement = t.members filterNot (m => isMemberOfObject(m))
      val ttree = TypeTree().setType(t)
      TypeApply(
        Select(
          Block(
            List(
              ClassDef(
                Modifiers(FINAL), 
                newTypeName("$anon"),
                List(),
                Template(
                  List(ttree), 
                  emptyValDef,
                  initDef +: (methodsToImplement map (m => methodImpl(m, t))).toList))),
            Apply(
              Select(
                New(Ident(newTypeName("$anon"))), 
                newTermName("<init>")), 
              List())),
          newTermName("asInstanceOf")),
        List(ttree))
    }

    c.Expr(anonClass(typeTag[T].tpe))
  }
}