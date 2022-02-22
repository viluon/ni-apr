package microc.cli

object Main extends App {
  val cli = new CliParser

  val status = try {
    val action = cli.parse(args)
    action.run()
  } catch {
    case CliException(message, _) =>
      System.err.println(message)
      1
    case e: Exception =>
      System.err.println(s"fatal: ${e.getMessage}")
      e.printStackTrace()
      1
  }
  sys.exit(status)
}
