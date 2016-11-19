package controllers.admin

import model.User

case class ValidSignin(details : User) extends SignInStatus
