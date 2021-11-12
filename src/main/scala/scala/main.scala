package scala

import data.{Airport, Country,Runway}

object Main
{
  // Ici on crée des listes immutables de nos csv Creating
  val countriesList = Parser.csvToList("country").asInstanceOf[List[Country]]
  val airportsList  = Parser.csvToList("airport").asInstanceOf[List[Airport]]
  val runwaysList   = Parser.csvToList("runway").asInstanceOf[List[Runway]]
  
  def main(args: Array[String]): Unit =
  {

    println("\n---------------------")
    println("Application de recherche d'aeroports par Ines et Thomas")
    println("---------------------\n")

    // Option parser function
    def optionParser(): Unit =
    {
      if (args.length != 1)
        println("Choisissez votre option: query ou report \n")
      else
      {
        val option = args(0)
        option match
        {
          case ("query")  => query
          case ("report") => report
          case (_)               => println("Choisissez votre option: query ou report\n")
        }
      }
    }

    // Launching option parser function
    optionParser

    // Closing our buffer
    Parser.buffersClosing()





    // Fonction pour l'option query
    def query(): Unit =
    {
      println("Vous avez choisi l'option query \n")
      println("Entrez un nom ou code d'un pays \n")
      // On appelle ici la fonction countryNameFormat pour vérifier le type de l'input
      val countryin = Functions.countryNameFormat(scala.io.StdIn.readLine("Pays> " ))
      println

      (countryin, countryin.length) match
      {
        // si l'input est un code alors on peut directement utiliser la fonction countrytoAirports
        case (_, 2) => Functions.countryToAirportsPrint(countryin, airportsList, runwaysList);
                       query
        // sinon il faut d'abord aller chercher le code avec la fonction countrytoCode
        case (_, _) => val country_code = Functions.getCodeFromName(countryin, countriesList); // ex: France => FR
                       Functions.countryToAirportsPrint(country_code, airportsList, runwaysList);
                       query
      }
    }

    // Function for the report option
    def report(): Unit =
    {
      println("Tu as choisis l'option report !\n");
      println("Que veux-tu savoir ? \n")
      println("Appuie sur 1 pour voir les 10 pays avec le plus d'aeroports")
      println("Appuie sur 2 pour voir les 10 pays avec le moins d'aeroports.")
      println("Appuie sur 3 pour voir le type de chaque piste par pays.")
      println("Appuie sur 4 pour voir les latitudes de piste les plus trouvees.\n")
      val reportin = scala.io.StdIn.readLine("ChoixReport> ");
      println

      reportin match
      {
        // pour les cases 1 et 2 on fait
        case ("1"|"2") => val isoCountryAirportList = Functions.isoCountryAirportToList(airportsList)
                                                                        .groupBy(identity)
                                                                        .mapValues(_.size)
                                                                        .toList.map{ case (k, v) => (v, k) }
                                                                        .sorted;
                          reportin match
                          {
                            case ("1") => println("les 10 pays avec le plus d'aeroports sont :\n")
                                          Functions.airportOccPrint(isoCountryAirportList.reverse.splitAt(10)._1)
                            case ("2") => println("les 10 pays avec le moins d'aeroports sont :\n")
                                          Functions.airportOccPrint(isoCountryAirportList.splitAt(10)._1)
                          };
                          report
        case ("3")             => val l1 = Functions.runwayAirportSurfaceToList(runwaysList)
                                  val l2 = Functions.airportIsocountryToList(airportsList)
                                  val l3 = l1.flatMap(x => l2.map(y => (x,y)))
                                                .filter{ case ((_,y),(b,_)) => y == b }
                                                .map {case ((x, _),(_,c)) => (x,c) }
                                  report
        case ("4")             => println("les 10 latitude de pistes les plus retrouvees:\n")
                                  Functions.commonRunwayLatitudePrint;
                                  report
        case (_)               => report
      }
    }


  }
}
