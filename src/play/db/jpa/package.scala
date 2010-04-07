/**
* overriding Model with Scala version
*/
package play { 
  package db { 
    package object jpa {
      type Model = ScalaModel
    } 
  } 
}
