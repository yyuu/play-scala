
class FreshDatabase {
    
    import play._
    import play.test._
    
    import models._
    
    info("Apply fixture: FreshDatabase")
    
    Fixtures.deleteDatabase()
    
    Yaml[List[Contact]]("data.yml").foreach {
        Contact.create(_)
    }
    
}