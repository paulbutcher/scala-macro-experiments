import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def implement[T](target: T) = macro ExperimentsImpl.implement[T]
}

object ExperimentsImpl {
  
  def implement[T: c.TypeTag](c: Context)(target: c.Expr[T]): c.Expr[T] = {
    import c.mirror._
    import reflect.api.Modifier._
    
    def finalResultType(methodType: Type): Type = methodType match {
      case NullaryMethodType(result) => result 
      case MethodType(_, result) => finalResultType(result)
      case _ => methodType
    }
    
    def paramss(methodType: Type): List[List[Symbol]] = methodType match {
      case MethodType(params, result) => params :: paramss(result)
      case _ => Nil
    }
    
    def buildParams(methodType: Type) = paramss(methodType) map { params =>
        params map { p =>
            ValDef(
              Modifiers(Set(parameter)),
              newTermName(p.name.toString),
              Ident(p.asTypeIn(methodType).typeSymbol),
              EmptyTree)
          }
      }
    
    def methodDef(name: Name, params: List[List[ValDef]], result: Type): DefDef = 
      DefDef(
        Modifiers(),
        name, 
        List(), 
        params,
        TypeTree(),
        TypeApply(
          Select(Literal(Constant(null)), newTermName("asInstanceOf")), 
          List(TypeTree().setType(result))))
    
    def methodImpl(m: Symbol, t: Type): DefDef = {
      val mt = m.asTypeIn(t) 
      mt match {
        case NullaryMethodType(_) => methodDef(m.name, buildParams(mt), finalResultType(mt))
        case MethodType(_, _) => methodDef(m.name, buildParams(mt), finalResultType(mt))
        case _ => sys.error("Don't know how to handle "+ m)
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
          
    def targetVal(t: Type) =
      ValDef(Modifiers(), 
        newTermName("target"), 
        Ident(t.typeSymbol), 
        target.tree)
      
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
                Modifiers(Set(`final`)), 
                newTypeName("$anon"),
                List(),
                Template(
                  List(ttree), 
                  emptyValDef,
                  initDef +: targetVal(t) +: 
                    (methodsToImplement map (m => methodImpl(m, t))).toList))),
            Apply(
              Select(
                New(Ident(newTypeName("$anon"))), 
                newTermName("<init>")), 
              List())),
          newTermName("asInstanceOf")),
        List(ttree))
    }
        
    anonClass(c.tag[T].tpe)
  }
}