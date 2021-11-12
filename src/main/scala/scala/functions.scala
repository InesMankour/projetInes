package scala

import data.{Airport, Country,Runway}

object Functions
{


  // Fonction qui modifie le format en fonction du type d'input (code ou nom)
  def countryNameFormat(name: String): String = name.length match
  {
    case (2) => name.toUpperCase; // si code on met en maj
    case (_) => name.toLowerCase.capitalize // si nom de pays on met france=> France
  }


  def getCodeFromName(countryName : String, countries: List[Country]): String = {
    val countryResult = countries.find( c => c.name.equals("\""+countryName+"\""))

    countryResult match{
      case None => "ZZ"
      case Some(value) => value.code.substring(1, value.code.length()-1)
    }

  }






  // Fonction qui affiche les aeroports et leurs occurences (query)
  def airportOccPrint(list: List[(Int, String)]): Unit = list match
  {
    case (Nil)     => println()
    case (head::tail) => println("Number of airports in " + head._2 + " is " +  head._1);
      airportOccPrint(tail)
  }

  // Fonction qui cherche la latitude des runways pour savoir leur occurences dans la fonction qui suit (report 4)
  def runwayLatitudeToList(runwaysList: List[Runway], acc: List[String] = Nil)
  : List[String] = runwaysList match
  {
    case (Nil)     => acc
    case (e::tail) => runwayLatitudeToList(tail, e.le_ident::acc)
  }

  // Fonction qui cherche les occurences de chaque latitude et affiche les 10 plus communes (report4)
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



  // Fonction qui retourne la liste des ID ainsi que les pays des aéroports de la liste
  def airportIsocountryToList(airportsList: List[Airport], acc: List[(String, String)] = Nil)
  : List[(String, String)] = airportsList match
  {
    case (Nil)     => acc
    case (head::tail) => airportIsocountryToList(tail, acc:+(head.ident, head.iso_country))
  }

  // Fonction qui affiche les pistes associées a un aeroport donné
  def airportToRunwaysPrint(airport_ident: String, lines: List[Runway], acc: Int = 0): Unit = lines match
  {
    case (Nil) if acc == 0                             => println("\t--> Aucune piste associee a cet aeroport\n")
    case (Nil)                                         => println
    // on affiche les caracteristiques de chaque aéroport
    case (head::tail) if head.airport_ident == airport_ident => println("\t--> " + head.id
      + " | " + head.airport_ref
      + " | " + head.airport_ident
      + " | " + head.length_ft
      + " | " + head.width_ft
      + " | " + head.surface);
      airportToRunwaysPrint(airport_ident, tail, acc + 1)
    // On réitère jusqu'a parcourir toute la liste des pistes
    case (head::tail) => airportToRunwaysPrint(airport_ident, tail, acc)
  }

  // Fonction qui affiche les aeroports associés a un pays
  def countryToAirportsPrint(country: String, airportsList: List[Airport], runwaysList: List[Runway]): Unit =
  {
    def countryToAirportsPrint2(airport_list: List[Airport], nb_airport: Int): Unit = airport_list match
    {

      // case de sortie
      case (Nil) => println(nb_airport + " Aeroports trouvés en " + country + ".\n")
      // cas ou le pays du premier aeroport est effectivement le country associé on affiche alors directement le nom de l'aeroports
      case (head::tail) if head.iso_country == ("\"" + country + "\"") => println(head.name);
        airportToRunwaysPrint(head.ident, runwaysList) // print runways
        countryToAirportsPrint2(tail, nb_airport + 1);
      // sinon on continue de parcourir notre liste d'aeroport
      case (head::tail)=> countryToAirportsPrint2(tail, nb_airport)
    }

    if (country == "ZZ")
      println("Erreur " + country + " N'est pas un bon code.\n")
    else
      countryToAirportsPrint2(airportsList, 0)
  }

  // Fonction qui retourne la listes des codes de pays des aeroports de la liste
  def isoCountryAirportToList(airportsList: List[Airport], acc: List[String] = Nil)
  : List[String] = airportsList match
  {
    case (Nil)     => acc
    case (head::tail) => isoCountryAirportToList(tail, head.iso_country::acc)
  }

  // Fonction qui retourne la liste des surfaces des pistes ainsi que leurs aéroports (report 3)
  def runwayAirportSurfaceToList(runwaysList: List[Runway], acc: List[(String, String)] = Nil)
  : List[(String, String)] = runwaysList match
  {
    // on retourne la valeur de l'acc lorsqu'on a fini de parcourir la fonction
    case (Nil)     => acc
    //on ajoute a chaque fois l'ID et la surface de la piste dans l'accumulateur
    case (head::tail) => runwayAirportSurfaceToList(tail, acc:+(head.airport_ident, head.surface))
  }
  // Fonction qui retourne un unction that return a country/airport/runway object giving a file.csv string line
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


}
