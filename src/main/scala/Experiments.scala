import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def myNew[T]: T = macro ExperimentsImpl.myNew[T]
  
  def members[T]: List[String] = macro ExperimentsImpl.members[T]
  
  def firstMember[T] = macro ExperimentsImpl.firstMember[T]
  
  def isCurried[T] = macro ExperimentsImpl.isCurried[T]
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
}
