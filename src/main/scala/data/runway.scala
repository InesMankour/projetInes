package data

case class Runway(id: String, airport_ref: String, airport_ident: String, length_ft: String, width_ft: String,
                  surface: String, lighted: String, closed: String, le_ident: String, le_latitude_deg: String,
                  le_longitude_deg: String, le_elevation_ft: String, le_heading_degT: String,
                  le_displaced_threshold_ft: String, he_ident: String, he_latitude_deg: String, he_longitude_deg: String,
                  he_elevation_ft: String, he_heading_degT: String, he_displaced_threshold_ft: String)
