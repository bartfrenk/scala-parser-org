package parser

case class Tree[+A](value: A, children: List[Tree[A]])
