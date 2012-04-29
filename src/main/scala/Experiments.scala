import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def implement[T] = macro ExperimentsImpl.implement[T]
}

object ExperimentsImpl {
  
  def implement[T: c.TypeTag](c: Context): c.Expr[T] = {
    import c.mirror._
    import reflect.api.Modifier._
    
    def buildParams(params: List[Symbol], mt: Type) = {
      List(
        params map { p =>
          val pt = p.asTypeIn(mt)
          ValDef(
            Modifiers(Set(parameter)),
            newTermName(p.name.toString),
            Ident(pt.typeSymbol),
            EmptyTree
          )
        }
      )
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
        case NullaryMethodType(result) => methodDef(m.name, List(), result)
        case MethodType(params, result) => methodDef(m.name, buildParams(params, mt), result)
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
      
    def isMemberOfObject(m: Symbol) = TypeTag.Object.tpe.member(m.name) != NoSymbol

    // { final class $anon extends T { ... }; new $anon() }.asInstanceOf[T])
    def anonClass(t: Type, methodsToImplement: Iterable[Symbol]) = {
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
                  initDef +: (methodsToImplement map (m => methodImpl(m, t))).toList))),
            Apply(
              Select(
                New(Ident(newTypeName("$anon"))), 
                newTermName("<init>")), 
              List())),
          newTermName("asInstanceOf")),
        List(ttree))
    }
        
    val t = c.tag[T].tpe
    val methodsToImplement = t.members filterNot (m => isMemberOfObject(m))
    anonClass(t, methodsToImplement)
  }
}