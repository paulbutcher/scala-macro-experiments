import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def implement[T] = macro ExperimentsImpl.implement[T]
}

object ExperimentsImpl {
  
  def implement[T: c.TypeTag](c: Context): c.Expr[T] = {
    import c.mirror._
    import reflect.api.Modifier._
    
    def methodDef(m: Symbol) =
      DefDef(
        Modifiers(),
        m.name, 
        List(), 
        List(),
        Ident(newTypeName("Unit")),
        Literal(Constant(()))
      )
    
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
                  initDef +: (methodsToImplement map (m => methodDef(m))).toList))),
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
