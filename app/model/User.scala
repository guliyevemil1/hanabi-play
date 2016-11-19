package model

@SerialVersionUID(6822161743322278253L)
class User(
          val idToken : String,
          val name : String,
          val fullName : String,
          val picture : String
          ) extends Serializable