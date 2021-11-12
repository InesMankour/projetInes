
package scala
object Parser
{
    // takes input data (frequently text) and builds a data structure
    // Chemin pour les csv
    val sourcesPath = "./src/main/scala/resources/"

    // Ouvrir les csv
    val bufferedSourceCountries = io.Source.fromFile(sourcesPath + "countries.csv")
    val bufferedSourceAirports = io.Source.fromFile(sourcesPath + "airports.csv")
    val bufferedSourceRunways = io.Source.fromFile(sourcesPath + "runways.csv")

    // Pour avoir nos données en liste
    val linesCountries = bufferedSourceCountries.getLines.toList
    val linesAirports = bufferedSourceAirports.getLines.toList
    val linesRunways = bufferedSourceRunways.getLines.toList
    
    // Fonction qui remplit les listes de chaque type
    def csvToList(types: String): List[Any] =
    {
      def csv_to_list2(lines: List[String], acc: List[Any] = Nil): List[Any] = lines match
      {
        case (Nil)     => acc
        case (head::tail) => val anyObject = Functions.toAny(types, head);
                          csv_to_list2(tail, anyObject::acc)
      }
      types match
      {
        case ("country") => csv_to_list2(Parser.linesCountries)
        case ("airport") => csv_to_list2(Parser.linesAirports)
        case ("runway")  => csv_to_list2(Parser.linesRunways)
        case (_)         => ???
      }
    }

    def buffersClosing(): Unit =
    {
        // Closing our buffer
        bufferedSourceCountries.close
        bufferedSourceAirports.close
        bufferedSourceRunways.close
    }






}
