package  org.aphreet.c3.snippet
import xml.NodeSeq
 import net.liftweb.http.js.JsCmds._
 import net.liftweb.http.js.JsCmds
 import net.liftweb.util.Helpers._
 import net.liftweb.http.{S, SHtml}
 import org.aphreet.c3.lib.DependencyFactory._
 import com.ifunsoftware.c3.access.{C3System, SearchResultEntry}
 import org.apache.commons.httpclient.util.URIUtil
 import java.text.SimpleDateFormat
import org.aphreet.c3.model.{Tag, Category}
import net.liftweb.mapper.By

class SearchSnippet {

   private val c3 = inject[C3System].open_!
   var results: List[SearchResultEntry] = List()
   def search = {
    "* *" #> ((x: NodeSeq) => x ++ Script(OnLoad(JsCmds.JsShowId("categories_s"))))
  }

  val tagSeparator = ","

  def render = {
    var tags: Array[String] = Array()
    var query = ""
    def process(){
      def query_param(tags:Array[String],query:String) = {
        //TODO:query params =  tags + query text

        query
      }
      if (!tags.isEmpty || !query.isEmpty){
        results = c3.search(query_param(tags,query))
      }
      else{
        S.notice("Entry param search")
      }
    }

    "name=tags" #> SHtml.onSubmit(v => tags = v.split(tagSeparator))&
    "name=query" #> SHtml.onSubmit(query = _) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
  def result = {
    if (results.isEmpty){
      ".conteyner_result" #> NodeSeq.Empty
    }else{
      ".conteyner_result *" #> results.map {
        entry => toCss(entry)
      }
    }
  }

private lazy val dateFormat: SimpleDateFormat = new SimpleDateFormat("dd/MM/yyyy")

 private def toCss(entry: SearchResultEntry) = {
    val resource = c3.getResource(entry.address, List("c3.ext.fs.path"))
    val metadata = resource.metadata
    val name = metadata.getOrElse("c3.fs.nodename", "")
    val path = resource.systemMetadata.getOrElse("c3.ext.fs.path", "")
    val resourceType = metadata.getOrElse("c3.fs.nodetype", "")
    //val c3Path = C3Path(path)
    ".address *" #> entry.address &
    ".header_search *" #> URIUtil.decode(name,"UTF-8")&
    ".type_search *" #> resourceType &
    ".date_create *" #> dateFormat.format(resource.date)
 }
  def categories = {
    val categories = Category.findAll()
    var flag2 = 0
    def categoryContents(cat: Category) = {
      flag2+=1
      val id = "tab"+flag2
      var id_span = 0
      val tagNames = Tag.findAll(By(Tag.category, cat))

      "span" #> tagNames.map{tg:Tag =>{
        id_span+=1
        "span *" #> tg.name &
        "span [id]" #> (id+"_"+id_span)
      }}&
        "a *" #> cat.name &
        "ul [id]" #> id
    }
    ".category_cnt *" #> categories.map{ cat:Category =>  categoryContents(cat) }
    }

}


//class SearchSnippet extends StatefulSnippet{
//
//  val c3 = inject[C3System].open_!
//  val logger = Logger(classOf[SearchSnippet])
//
//  var dispatch : DispatchIt = if(stringToSearch.isEmpty){
//
//    case "search" => searchForm _
//    case "miniSearch" => miniSearchForm _
//
//  } else {
//
//    case "search" => resultPage _
//    case "miniSearch" => miniSearchForm _
//
//  }
//
//  var searchString = ""
//
////  def resultPage (html: NodeSeq) = {
////    if (! stringToSearch.isEmpty )
////      searchString = stringToSearch.open_!
////
////    val resultEntries = c3.search(searchString)
////
////    val format: SimpleDateFormat = new SimpleDateFormat("dd/MM/yyyy")
////
////    bind("search", html,
////      "query" -> SHtml.text(searchString, processQuery _ ,"placeholder" -> "Search","size" -> "60" ),
////
////      "resultSet" -> { (ns : NodeSeq) =>
////        resultEntries.flatMap( entry => {
////
////          try {
////            val resource = c3.getResource(entry.address, List("c3.ext.fs.path"))
////            val metadata = resource.metadata
////
////            val name = metadata.getOrElse("c3.fs.nodename", "")
////            val path = resource.systemMetadata.getOrElse("c3.ext.fs.path", "")
////            val resourceType = metadata.getOrElse("c3.fs.nodetype", "")
////
////            val c3Path = C3Path(path)
////
////            if(name != "")
////              bind("entry", ns,
////                "address" ->  entry.address ,
////                "name" -> URIUtil.decode(name, "UTF-8"),
////                "created" -> format.format(resource.date),
////                "resource_path" -> Text(""),
////                "full_path" -> SHtml.link(c3Path.resourceUri, () => {}, Text(name)),
////                "type" -> { resourceType match {
////                  case "directory" => <img src="/images/icons/folder.gif"/>
////                  case _ => <img src="/images/icons/document.gif" />
////                }}
////              )
////            else NodeSeq.Empty
////          }
////          catch {
////            case e: C3AccessException => {
////              logger error e
////              NodeSeq.Empty
////            }
////          }
////
////        }):NodeSeq
////      },
////      "submit" -> SHtml.submit("Go", () => {}  )
////    )
////  }
//
//  def searchForm (html: NodeSeq) = {
//    bind("search", html,
//      "query" -> SHtml.text(searchString, processQuery _ , "placeholder" -> "Search","size" -> "60"),
//      "resultSet" -> "",
//      "submit" -> SHtml.submit("Go", () => {
//        dispatch = {
//          case "search" => resultPage _
//          case "miniSearch" => miniSearchForm _
//        }
//      })
//    )
//  }
//
//  // we store a string entered in an input box of mini search form
//  object stringToSearch extends SessionVar[Box[String]](Empty)
//
//  def miniSearchForm (html : NodeSeq) = {
//    var searchParam = ""
//    bind("miniSearch", html,
//      "search_string" -> SHtml.text("",searchParam = _ , "placeholder" -> "Search"),
//      "submit" -> SHtml.submit("Go", () => S.redirectTo("/search",() => {
//
//        stringToSearch.set(Full(searchParam))
//        User.addSearchRequest( searchParam )
//      }))
//    )
//  }
//
//  def processQuery(query : String){
//    searchString = query
//    stringToSearch(Full(query))
//
//    if(searchString != ""){
//      User.addSearchRequest(searchString)
//
//      dispatch = {
//        case "search" => resultPage _
//        case "miniSearch" => miniSearchForm _
//
//      }
//    }
//    else {
//      S.error("Please, enter a text to search for")
//      dispatch = {
//        case "search" => searchForm _
//        case "miniSearch" => miniSearchForm _
//      }
//    }
//  }
//
//}