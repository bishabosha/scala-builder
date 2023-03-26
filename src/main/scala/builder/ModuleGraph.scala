package builder

import errors.*
import collection.mutable

object ModuleGraph:

  def stages(graph: Map[String, Module]): List[List[Module]] =
    val stages = mutable.ListBuffer.empty[List[Module]]
    val dependencies =
      graph.map((k, v) => k -> v.dependsOn.to(mutable.Set))

    val reverseDeps: mutable.Map[String, mutable.Set[String]] =
      val buf = mutable.Map.empty[String, mutable.Set[String]]
      for (k, v) <- graph do
        buf.getOrElseUpdate(k, mutable.Set.empty[String])
        for dep <- v.dependsOn do
          buf.getOrElseUpdate(dep, mutable.Set.empty[String]).add(k)
      buf

    def iterate(s1: List[Module], acc: List[List[Module]]): List[List[Module]] =
      var sNext = mutable.ListBuffer.empty[Module]
      for module <- s1 do
        val ndeps = dependencies(module.name)
        for d <- ndeps.toList do
          val incoming = reverseDeps(d)
          ndeps -= d
          incoming -= module.name
          if incoming.isEmpty then
            sNext += graph(d)
          end if
      if sNext.isEmpty then
        acc
      else
        val s2 = sNext.toList
        iterate(s2, s2 :: acc)

    val s0 = reverseDeps.collect({ case (node, incoming) if incoming.isEmpty => graph(node) }).toList
    iterate(s0, s0 :: Nil)
  end stages


  def checkValid[T](graph: Map[String, Module])(using CanError[T]): Unit =
    // prove not cyclic, if cyclic return error, else return None

    val lookup =
      for (k, v) <- graph if k != v.name do
        abortWithError(s"module graph key '$k' mismatches its value '${v.name}'.")
      graph

    val seen = mutable.Set.empty[String]
    val visiting = mutable.Set.empty[String]
    def visit(node: Module, from: Module | Null): Unit =
      if visiting.contains(node.name) then
        val fromName = Option(from).map(_.name).getOrElse("<unknown>")
        val onMessage = if fromName == node.name then "itself." else s"module '${node.name}'."
        abortWithError(s"module graph is invalid: module '$fromName' has a cyclic dependency on $onMessage")
      else if !seen.contains(node.name) then
        visiting.add(node.name)
        node.dependsOn.foreach(dep => lookup.get(dep) match
          case Some(module) => visit(module, node)
          case _ => abortWithError(s"module '${node.name}' depends on '$dep' which does not exist.")
        )
        visiting.remove(node.name)
        seen.add(node.name)
      else
        ()
    end visit
    lookup.values.foreach(visit(_, null))
