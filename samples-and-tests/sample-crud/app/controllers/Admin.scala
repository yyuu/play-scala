package controllers

import play._
import play.mvc._

import models._

object Contacts extends Controller with CRUD[Contact] with Secure
object Companies extends Controller with CRUD[Company] with Secure