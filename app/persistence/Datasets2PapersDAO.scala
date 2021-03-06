package persistence

import anorm.SqlParser._
import anorm._
import models.{Dataset, Datasets2Papers, Paper}
import play.api.Play.current
import play.api.db.DB

import scala.collection.mutable

/**
  * Created by Mattia on 22.01.2015.
  */
object Datasets2PapersDAO {
   private val datasets2papersParser: RowParser[Datasets2Papers] =
     get[Pk[Long]]("id") ~
       get[Long]("datasets_id") ~
       get[Long]("papers_id") map {
       case id ~datasets_id ~papers_id => Datasets2Papers(id, datasets_id, papers_id)
     }

   def findById(id: Long): Option[Datasets2Papers] =
     DB.withConnection { implicit c =>
       SQL("SELECT * FROM datasets2papers WHERE id = {id}").on(
         'id -> id
       ).as(datasets2papersParser.singleOpt)
     }

  def findDatasetsByPaperId(paperId: Long): List[Dataset] = {
    DB.withConnection { implicit c =>
      val datasets2papers = SQL("SELECT * FROM datasets2papers WHERE papers_id = {id}").on(
        'id -> paperId
      ).as(datasets2papersParser *)
      val result : mutable.MutableList[Dataset] = new mutable.MutableList[Dataset]
      datasets2papers.foreach( d2p => {
        result.+=:(DatasetDAO.findById(d2p.datasets_id).get)
      })
      result.toList
    }
  }

  def findPapersByDatasetId(datasetId: Long): List[Paper] = {
    DB.withConnection { implicit c =>
      val datasets2papers = SQL("SELECT * FROM datasets2papers WHERE datasets_id = {id}").on(
        'id -> datasetId
      ).as(datasets2papersParser *)
      var result : mutable.MutableList[Paper] = new mutable.MutableList[Paper]
      for(paper <- datasets2papers){
        result += PaperDAO.findById(paper.papers_id).get
      }
      result.toList
    }
  }

   def add(datasets_id: Long, papers_id: Long): Long = {
     var id: Option[Long] = None
     getAll().foreach(dataset => {
       if(dataset.datasets_id == datasets_id && dataset.papers_id == papers_id){
         id = Some(dataset.id.get)
       }
     })
     if(id == None) {
       id = DB.withConnection { implicit c =>
         SQL("INSERT INTO datasets2papers(datasets_id, papers_id) VALUES ({datasets_id}, {papers_id})").on(
           'datasets_id -> datasets_id,
           'papers_id -> papers_id
         ).executeInsert()
       }
     }
     id.get
   }

   def getAll(): List[Datasets2Papers] =
     DB.withConnection { implicit c =>
       SQL("SELECT * FROM datasets2papers").as(datasets2papersParser*).toList
     }
 }
