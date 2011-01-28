package models

import play.db.sql._
import SqlRowsParser._
import Row._

import play.data.Validators._

import java.util._

// ~~~ Contact

case class Contact(
    @Required firstname: String,
    @Required name: String,
    @Required birthdate: Date,
    @Required @Email email: String
)

object Contact extends MEntity[Long, Contact]