package scala

import org.scalatest._
import data.Country
import data.Runway
import data.Airport
/**
 *
 * Programme de test SCALA pour le projet de d'airport 
 *
 * @author  Thomas Noirclerc
 * @version 2.13
 * @since   2021-12-11
 *
 **/

class test extends FlatSpec with Matchers {

  "countryNameFormat" should "Transformer mes valeurs" in {
    Functions.countryNameFormat("Fr") should be("FR")
    Functions.countryNameFormat("FR") should be("FR")
    Functions.countryNameFormat("france") should be("France")
    Functions.countryNameFormat("FRANCE") should be("France")
  }

  "countryToCode" should "censée retourner le code du pays à partir du nom " in {
  val country = Country("302618","AE","United Arab Emirates","AS","http://en.wikipedia.org/wiki/United_Arab_Emirates","UAE")
  val country2 = Country("302687","FR","France","EU","http://en.wikipedia.org/wiki/France","")
  Functions.countryToCode("United Arab Emirates",List(country),15) should be("AE")
  Functions.countryToCode("France",List(country2),15) should be("FR")


  }
  "toAny" should "censée nous retournée un objet de type runway/country/airport en fonction de l'input" in {
    val line = "245528,6528,00CA,6000,80,ASPH,0,0,04,35.3493,-116.893,,50,,22,35.3603,-116.878,,,"
    val runway = Runway("245528","6528","00CA","6000","80","ASPH","0","0","04","35.3493","-116.893","","50","","22","35.3603","-116.878","",""," ")
    Functions.toAny("runway",line) should be (runway)
  }

  "runwayLatitudeToList" should "cherche la latitude des runways pour savoir leur occurences dans la fonction qui suit (report 4)" in {
    val runwayList = Runway("245528","6528","00CA","6000","80","ASPH","0","0","04","35.3493","-116.893","","50","","22","35.3603","-116.878","",""," ")
    val acc = List.empty[String]
    Functions.runwayLatitudeToList(List(runwayList),acc) should be (List("04"))
  }

  "isoCountryAirportToList" should "retourne la listes des codes de pays des aeroports de la liste" in {
    val airport = Airport("6539","00IL","small_airport","Hammer Airport","41.97840118408203","-89.5604019165039","840","NA","US","US-IL","Polo","no","00IL","","00IL","","","")
    val acc = List.empty[String]
    Functions.isoCountryAirportToList(List(airport),acc) should be (List("US"))
  }
  "runwayAirportSurfaceToList" should "retourne la liste des surfaces des pistes ainsi que leurs aéroports (report 3)" in {
    val runway = Runway("245528","6528","00CA","6000","80","ASPH","0","0","04","35.3493","-116.893","","50","","22","35.3603","-116.878","","","")
    val acc = List.empty[(String, String)]
    Functions.runwayAirportSurfaceToList(List(runway),acc) should be (List(("00CA","ASPH")))
  }

}


