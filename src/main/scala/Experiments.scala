import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def myNew[T]: T = macro ExperimentsImpl.myNew[T]
  
  def members[T]: List[String] = macro ExperimentsImpl.members[T]
  
  def firstMember[T] = macro ExperimentsImpl.firstMember[T]
  
  def isCurried[T] = macro ExperimentsImpl.isCurried[T]
  
  def implement[T] = macro ExperimentsImpl.implement[T]
}

object ExperimentsImpl {
  
  def myNew[T: c.TypeTag](c: Context): c.Expr[T] = {
    import c.mirror._
    Apply(
      Select(
        New(TypeTree().setType(c.tag[T].tpe)),
        newTermName("<init>")),
      List())
  }
  
  def members[T: c.TypeTag](c: Context): c.Expr[List[String]] = {
    import c.mirror._
    val ms = c.tag[T].tpe.members map { m => Literal(Constant(m.toString)) }
    Apply(
      Select(
        Select(
          This(staticModule("scala.collection.immutable").moduleClass),
          newTermName("List")),
        newTermName("apply")),
      ms.toList)
  }
  
  def firstMember[T: c.TypeTag](c: Context): c.Expr[String] = {
    import c.mirror._
    val t = c.tag[T].tpe
    val fm = t.members.head
    val fmt = c.TypeTag(fm.typeSignatureIn(t))
    Literal(Constant(fmt.tpe.toString))
  }
  
  def isCurried[T: c.TypeTag](c: Context) = {
    import c.mirror._
    val t = c.tag[T].tpe
    t.members foreach { m =>
      m.asTypeIn(t) match {
        case MethodType(_, result) => result match {
          case MethodType(_, _) => println(m.toString + " is curried")
          case _ =>
        }
        case _ =>
      }
    }
    reify(())
  }
  
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
