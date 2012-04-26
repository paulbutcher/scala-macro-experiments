import reflect.makro.Context

object Experiments {
  import language.experimental.macros
  
  def myNew[T]: T = macro ExperimentsImpl.myNew[T]
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
}