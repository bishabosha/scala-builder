package builder

import errors.*
import collection.mutable

object ModuleGraph:

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
