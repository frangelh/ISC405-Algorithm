import scala.annotation.tailrec

case class Nodo[T](elem: T, rango: Int, padre: T)


case class UnionFind[T] private (nodos: Map[T, Nodo[T]], conta: Int) {


  def union(x: T, y: T): UnionFind[T] = {
    require(nodos.contains(x) && nodos.contains(y), "Solo Agregados")

    val (elem1, uf1) = this.buscar(x)
    val (elem2, uf2) = uf1.buscar(y)
    val (raiz1: Nodo[T], raiz2: Nodo[T]) = (uf2.nodos(elem1.get), uf2.nodos(elem2.get))
    (raiz1, raiz2) match {

      case (n1, n2) if n1 == n2 => this

      case (n1 @ Nodo(_, rank1, _), n2 @ Nodo(_, rank2, _)) if rank1 > rank2 =>
        new UnionFind(uf2.nodos + (n2.elem -> n2.copy(padre = n1.elem)),
          conta - 1)

      case (n1 @ Nodo(_, rank1, _), n2 @ Nodo(_, rank2, _)) if rank1 < rank2 =>
        new UnionFind(uf2.nodos + (n1.elem -> n1.copy(padre = n2.elem)),
          conta - 1)

      case (n1 @ Nodo(_, rank1, _), n2 @ Nodo(_, rank2, _)) /*if rank1 == rank2*/ =>
        new UnionFind(uf2.nodos + (n1.elem -> n1.copy(rango = rank1 + 1))
          + (n2.elem -> n2.copy(padre = n1.elem)),
          conta - 1)
    }
  }


  def buscar(elem: T): (Option[T], UnionFind[T]) = {
    nodos.get(elem).fold[(Option[T], UnionFind[T])] {
      (None, this)
    } { node =>
      val ruta = rutaARaiz(node)
      val raiz = ruta.head
      val actualizarNodos = ruta.foldLeft (nodos) { (ns, n) =>
        ns + (n.elem -> n.copy(padre = raiz.elem))
      }

      (Some(raiz.elem), new UnionFind[T](actualizarNodos, conta))
    }
  }


  def size: Int = conta

  private def rutaARaiz(node: Nodo[T]): List[Nodo[T]] = {
    @tailrec
    def path(ns: List[Nodo[T]]): List[Nodo[T]] = {
      val n = ns.head
      n.padre match {
        case n.elem => ns
        case p => path(nodos(p) :: ns)
      }
    }
    path(List(node))
  }

}

object UnionFind {
  private def node[T](elem: T): Nodo[T] = Nodo(elem, 0, elem)


  def apply[T](elements: T*): UnionFind[T] = {
    val nodes = Map(elements.map{e => e -> node(e) } : _*)
    new UnionFind[T](nodes, elements.size)
  }
}