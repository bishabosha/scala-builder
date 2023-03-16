package builder

trait SharedConfig:
  def name: Option[String]

case class Config(
  name: Option[String] = None,
  modules: List[Module] = Nil
) extends SharedConfig


case class Module(
  name: Option[String] = None,
  // version: Option[String] = None,
  // dependencies: List[Dependency] = Nil
) extends SharedConfig
