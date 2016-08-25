import javax.inject._

import play.api._
import play.api.http.DefaultHttpFilters
import play.api.mvc._
import play.filters.gzip.GzipFilter

@Singleton
class Filters @Inject() (gzipFilter: GzipFilter) extends DefaultHttpFilters(gzipFilter)