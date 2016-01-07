package parsers

case class Tree[+A](value: A, branches: List[Tree[A]]) {

  override def toString: String = {
    "\n" + traverse((a, i) => "\t" * i + a.toString + "\n")()
  }

  // this could be less ad-hoc (traversable?)
  def traverse(f: (A, Int) => String)(level: Int = 0): String = {
    def op(acc: String, tree: Tree[A]) = acc + tree.traverse(f)(level + 1)
    f(value, level) + branches.foldLeft("")(op)
  }

}
