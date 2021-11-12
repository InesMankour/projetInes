# projetInes
à faire pour le 11 novembre au soir :

CORE SUBJECT (for every one to reach 10)
C.1) For this project you'll write a program parsing 3 CSV files. The files contain data for countries, airports and runway information.
  You will create an sbt scala project with unit test. Feel free to use any scala library/framework as to write your test.
  You must not use «var», «for», «return», «throw», «try/catch», keywords (unless you're writing a optional part, in that case it may be allowed if you ask first).
If you don't chose the database optional task, you're allowed to use mutables collection instead. The sole purpose of the mutable collections must be to replaces a database.

 

C.2.1) You're program will be a command line program that will ask the user for two options - Query or Reports.
C.2.2 Query Option will ask the user for the country name or code and display the airports & runways at each airport. The input can be country code or country name.
C.2.3 Choosing Reports will display the following (possibly through a menu):
• 10 countries with highest number of airports (with count) and countries  with lowest number of airports.
• Type of runways (as indicated in "surface" column) per country
• The top 10 most common runway latitude (indicated in "le_ident" column)

 

If you get the job done (6pt) with a clean code (4pt), correct test coverage (4pt) you'll have up to 14.

 

OPTIONAL PART (for those who want more)
To reach 20 or more (if its allowed) chose among those options.
O.0) Use coursier (0.5pt)
O.1) In 2.2 make the name matching partial/fuzzy. e.g. entering zimb will result in Zimbabwe (2pt)
O.2) Use database (in memory for the exercice like h2,sqlite) (4pt)
O.3) Do a GUI (6pt)
O.4) provide a rest API (4pt)
O.5) O.4 with a scalajs (possibly scalajs-react) frontend, in that case you only have to write Query (no report) from the CORE question. (4pt + 4pt of O.4)

 


For optional part 2 to 4 you're allowed to use scala libraries. But every group should use different libraries.
Possible libraries for
O.2) Anorm, slick, squeryl, reactive-mongo, Casbah, elastic4s, Quil, doobie, Scalikejdbc, sdbc, sorm, mongo-scala-driver
O.4) Finch, http4s, Akka Http, Spray, Play (finatra and scalatra are forbidden)
