package com.scalametrics.controllers


  import com.scalametrics.models._
  import play.api.mvc._
  import play.api.libs.json._


  class Api extends Controller {

    def fitRegression = Action(parse.json) { request =>
      request.body match {
        case f: JsSuccess => {

          Ok("Looks good pal!")
        }
        case e: JsError => {
          Ok("Uh looks like we have a problem")
        }
      }
    }
  }

