package scala

import data.{Airport, Country,Runway}

object Utils
{
    // Fonction qui modifie le format en fonction du type d'input (code ou nom)
    def countryNameFormat(name: String): String = name.length match
    {
      case (2) => name.toUpperCase; // si code on met en maj
      case (_) => name.toLowerCase.capitalize // si nom de pays on met france=> France
    }

    // Fonction sensée retourner le code du pays à partir du nom
    def countryToCode(country: String, countriesList: List[Country], nbSplit: Int = 15): String =
    {
      // sous fonction qui parcours countriesList pour trouver le nom associé
        def countryToCode2(countriesList: List[Country], nbSplit: Int): String = countriesList match
        {
            case (Nil) => "ZZ"
            case (head::tail) if head.name.splitAt(nbSplit)._1 == country => head.code
            case (head::tail) => countryToCode2(tail, nbSplit)
        }

        val code = countryToCode2(countriesList, nbSplit);

        (code, nbSplit == 2) match
        {
            case ("ZZ", false) => countryToCode(country, countriesList, nbSplit - 1)
            case (_, _)        => code
        }
    }







    // Fonction qui affiche les aeroports et leurs occurences
    def airportOccPrint(list: List[(Int, String)]): Unit = list match
    {
      case (Nil)     => println()
      case (head::tail) => println("Number of airports in " + head._2 + " is " +  head._1);
                        airportOccPrint(tail)
    }

    // Fonction qui cherche la latitude des runways pour savoir leur occurences dans la fonction qui suit
    def runwayLatitudeToList(runwaysList: List[Runway], acc: List[String] = Nil)
    : List[String] = runwaysList match
    {
      case (Nil)     => acc
      case (e::tail) => runwayLatitudeToList(tail, e.le_ident::acc)
    }

    // Fonction qui cherche les occurences de chaque latitude et affiche les 10 plus communes
    def commonRunwayLatitudePrint(): Unit =
    {
      val occ_list = runwayLatitudeToList(Main.runwaysList).groupBy(identity)
                                                      .mapValues(_.size)
                                                      .toList.map{ case (k, v) => (v, k) }
                                                      .sorted.reverse
                                                      .splitAt(10)._1
      def commonRunwayLatitudePrint2(occ_list: List[(Int, String)]): Unit = occ_list match
      {
        case (Nil)     => println
        case (head::tail) => println( head._2 + " with " +
                                    head._1 + " occurrences");
                          commonRunwayLatitudePrint2(tail)
      }

      commonRunwayLatitudePrint2(occ_list)
    }





 // Toutes les fonctions utiles aux sorties

  // Function that return a country/airport/runway object giving a file.csv string line
  def toAny(types: String, line: String) =
  {
    val cols = (line + " ").split(",")
    types match
    {
      case ("country") => Country(cols(0), cols(1), cols(2), cols(3), cols(4), cols(5))
      case ("airport") => Airport(cols(0), cols(1), cols(2), cols(3), cols(4), cols(5), cols(6), cols(7), cols(8), cols(9),
        cols(10), cols(11), cols(12), cols(13), cols(14), cols(15), cols(16), cols(17))
      case ("runway")  => Runway(cols(0), cols(1), cols(2), cols(3), cols(4), cols(5), cols(6), cols(7), cols(8), cols(9),
        cols(10), cols(11), cols(12), cols(13), cols(14), cols(15), cols(16), cols(17), cols(18),
        cols(19))
      case (_)         => ???
    }
  }

  // Function that print runways associated to a given airport
  def airportToRunwaysPrint(airport_ident: String, lines: List[Runway], acc: Int = 0): Unit = lines match
  {
    case (Nil) if acc == 0                             => println("\t--> No runways associated to that airport.\n")
    case (Nil)                                         => println
    case (head::tail) if head.airport_ident == airport_ident => println("\t--> " + head.id
      + " | " + head.airport_ref
      + " | " + head.airport_ident
      + " | " + head.length_ft
      + " | " + head.width_ft
      + " | " + head.surface);
      airportToRunwaysPrint(airport_ident, tail, acc + 1)
    case (head::tail)                                     => airportToRunwaysPrint(airport_ident, tail, acc)
  }

  // Function that print airports associated to a country
  def countryToAirportsPrint(country: String, airportsList: List[Airport], runwaysList: List[Runway]): Unit =
  {
    def countryToAirportsPrintHat(airport_list: List[Airport], nb_airport: Int): Unit = airport_list match
    {
      case (Nil)                                                 => println(nb_airport + " Aeroports trouvés en " + country + Console.RESET + ".\n")
      case (head::tail) if head.iso_country == ("\"" + country + "\"") => println(head.name);
        print(Console.BLUE);
        airportToRunwaysPrint(head.ident, runwaysList) // print runways
        print(Console.RESET)
        countryToAirportsPrintHat(tail, nb_airport + 1);
      case (e::tail)                                             => countryToAirportsPrintHat(tail, nb_airport)
    }
    if (country == "ZZ")
      println("Erreur" + country + " N'est pas un bon code.\n")
    else
      countryToAirportsPrintHat(airportsList, 0)
  }

  // Function that return the list containing all the airports iso country
  def isoCountryAirportToList(airportsList: List[Airport], acc: List[String] = Nil)
  : List[String] = airportsList match
  {
    case (Nil)     => acc
    case (head::tail) => isoCountryAirportToList(tail, head.iso_country::acc)
  }

  // Function that return the list containing all runway airport associated to its surface
  def runwayAirportSurfaceToList(runwaysList: List[Runway], acc: List[(String, String)] = Nil)
  : List[(String, String)] = runwaysList match
  {
    case (Nil)     => acc
    case (head::tail) => runwayAirportSurfaceToList(tail, acc:+(head.airport_ident, head.surface))
  }

  // Function that return the list containing all airport id associated to its country
  def airportIsocountryToList(airportsList: List[Airport], acc: List[(String, String)] = Nil)
  : List[(String, String)] = airportsList match
  {
    case (Nil)     => acc
    case (head::tail) => airportIsocountryToList(tail, acc:+(head.ident, head.iso_country))
  }

}
