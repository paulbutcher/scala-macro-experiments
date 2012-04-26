import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def myNew[T]: T = macro ExperimentsImpl.myNew[T]
  
  def members[T]: List[String] = macro ExperimentsImpl.members[T]
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
  
  def members[T: c.TypeTag](c: Context): c.Expr[List[_]] = {
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
}
