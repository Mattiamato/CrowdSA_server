package controllers

import java.util.Date

import anorm.NotAssigned
import com.typesafe.config.ConfigFactory
import models.{Assignment, Turker}
import org.apache.commons.codec.binary.Base64
import pdf.HighlightPdf
import persistence._
import play.api.mvc._

/**
 * Created by Mattia on 22.01.2015.
 */
object Waiting extends Controller {
	val config = ConfigFactory.load("application.conf")
	val layoutMode = config.getInt("layoutMode")

	/**
	 * Get - Get the right waiting layout (layout is defined in application.conf)
	 * @param turkerId the id of the turker
	 * @param flash the message to flash in the waiting overview
	 * @return
	 */
	def getWaitingLayout(turkerId: String, flash: Flash): Result = {
		try {
			val mode = layoutMode match {
				case 1 => Ok(views.html.waiting_1(turkerById(turkerId), flash))
				case 2 => Ok(views.html.waiting_2(turkerById(turkerId), flash))
				case 3 => Ok(views.html.waiting_3(turkerById(turkerId), flash))
				case 4 => Ok(views.html.waiting_4(turkerById(turkerId), flash))
				case 5 => Ok(views.html.waiting_5(turkerById(turkerId), flash))
				case _ => Ok("LayoutMode is not yet defined")
			}
			return mode
		} catch {
			case e: Exception => e.printStackTrace()
		}
		return InternalServerError("Something went wrong!")
	}

	private def turkerById(turkerId: String): Turker = {
		TurkerDAO.findByTurkerId(turkerId).getOrElse(null)
	}

	/** GET - waiting page
	  *
	  */
	def waiting = Action { implicit request =>
		val session = request.session

		request.session.get("turkerId").map {
			turkerId => {
				if (turkerById(turkerId) == null) {
					Redirect(routes.Application.index()).withNewSession
				} else {
					//Logger.debug("Get Waiting Layout with args: " + request.session.get("success").get)
					getWaitingLayout(turkerId, request.flash)
				}
			}
		}.getOrElse {
			Redirect(routes.Application.index())
		}
	}

	/**
	 * Get - Get question based on the layout type currently loaded
	 * @return
	 */
	def getQuestion() = Action { implicit request =>
		val session = request.session

		request.session.get("turkerId").map {
			turkerId =>
				val m = layoutMode match {
					case 1 => getRandomQuestionRandomPaper(turkerId) //RANDOM_QUESTION_RANDOM_PAPER
					case 2 => getRandomQuestionSamePaper(turkerId, request.session) //RANDOM_QUESTION_SAME_PAPER
					case 3 => getSameQuestionTypeRandomPaper(turkerId, request.session) //SAME_QUESTION_TYPE_RANDOM_PAPER
					case 4 => getSameQuestionTypeSamePaper(turkerId, request.session) //SAME_QUESTION_TYPE_SAME_PAPER
					case _ => InternalServerError("LayoutMode is not yet defined")
				}
				m
		}.getOrElse {
			Redirect(routes.Application.index())
		}
	}

	/**
	 * Get - Ony used for layout 5, gets the question that the turker chose
	 * @param questionId id of the question to load
	 * @return Redirect to the PDF Viewer page
	 */
	def getDefinedQuestion(questionId: Long) = Action { implicit request =>
		val session = request.session

		request.session.get("turkerId").map {
			turkerId =>
				val assignmentId = assignQuestion(questionId, turkerId)
				if (assignmentId != -1) {
					Redirect("/viewer/" + questionId.toString + "/" + assignmentId.toString)
				} else {
					val paper_id = QuestionDAO.findById(questionId).get.papers_id
					Redirect(routes.Waiting.secondStep(paper_id)).flashing(
						"error" -> "You are already solving another question. Submit or cancel the task in order to solve other questions."
					)
				}
		}.getOrElse {
			Redirect(routes.Application.index())
		}
	}

	/**
	 * Get- gets only one question type from a single paper
	 * @param turkerId id of the turker
	 * @param session session of the turker
	 * @return redirects to the PDF Viewer page
	 */
	def getSameQuestionTypeSamePaper(turkerId: String, session: Session): Result = {
		var questionType: String = ""
		var paperId: Long = 0
		if (session.get("paperId").getOrElse("") == "") {
			// If no paper is yet selected, select a random one
			paperId = PaperDAO.getRandomPaperId()
		} else {
			paperId = session.get("paperId").get.toLong
		}
		if (session.get("questionType").getOrElse("") == "") {
			// If no question type is yet selected, select a random one
			questionType = QuestionDAO.getRandomQuestionType(turkerId)
		} else {
			questionType = session.get("questionType").get
		}
		try {
			val questionId = QuestionDAO.getSameQuestionTypeRandomPaper(turkerId, questionType)
			val assignmentId = assignQuestion(questionId, turkerId)

			Ok("/viewer/" + questionId.toString + "/" + assignmentId.toString).withSession(
				session +
					("paperId" -> paperId.toString) +
					("questionType" -> questionType)
			)
		} catch {
			case e: Exception => {
				Thread.sleep(5000)
				getSameQuestionTypeSamePaper(turkerId, session)
			}
		}
	}


	/**
	 * Get only one question type from random papers
	 * @param turkerId
	 * @param session
	 * @return
	 */
	def getSameQuestionTypeRandomPaper(turkerId: String, session: Session): Result = {
		var questionType: String = ""
		if (session.get("questionType").getOrElse("") == "") {
			// If no question type is yet selected, select a random one
			questionType = QuestionDAO.getRandomQuestionType(turkerId)
		} else {
			questionType = session.get("questionType").get
		}
		try {
			val questionId = QuestionDAO.getSameQuestionTypeRandomPaper(turkerId, questionType)
			val assignmentId = assignQuestion(questionId, turkerId)
			Ok(questionId.toString + "/" + assignmentId.toString).withSession(
				session + ("questionType" -> questionType))
		} catch {
			case e: Exception => {
				Thread.sleep(5000)
				getSameQuestionTypeRandomPaper(turkerId, session)
			}
		}
	}

	/**
	 * Get a random questionId from a single paper
	 * @param turkerId
	 * @return
	 */
	def getRandomQuestionSamePaper(turkerId: String, session: Session): Result = {
		var paperId: Long = 0
		if (session.get("paperId").getOrElse(0) == 0) {
			// If no paper is yet selected, select a random paper
			paperId = PaperDAO.getRandomPaperId()
		} else {
			paperId = session.get("paperId").get.toLong
		}
		try {
			val questionId = QuestionDAO.getRandomQuestionSamePaper(turkerId, paperId)
			val assignmentId = assignQuestion(questionId, turkerId)
			Ok(questionId.toString + "/" + assignmentId.toString).withSession(
				session + ("paperId" -> paperId.toString))
		} catch {
			case e: Exception => {
				Thread.sleep(5000)
				getRandomQuestionSamePaper(turkerId, session)
			}
		}
	}

	/**
	 * Get a random questionId from the available questions stored in the database
	 * @return
	 */
	def getRandomQuestionRandomPaper(turkerId: String): Result = {
		try {
			val questionId = QuestionDAO.getRandomQuestionRandomPaper(turkerId)
			val assignmentId = assignQuestion(questionId, turkerId)
			Ok(questionId.toString + "/" + assignmentId.toString)
		} catch {
			case e: Exception => {
				Thread.sleep(5000)
				getRandomQuestionRandomPaper(turkerId)
			}
		}
	}

	/**
	 * Assign a question to a team composed only by 1 turker (the requester)
	 * @param questionId id of the question
	 * @return Assignment id that is created
	 */
	def assignQuestion(questionId: Long, turkerId: String): Long = {
		// if a question is already assigned and it is not cancelled and there are
		// no answers and the question is still available, return -1
		if (AssignmentDAO.isAnAssignmentAlreadyOpen(turkerId)) {
			-1
		} else {
			val date = new Date().getTime / 1000
			val timeout = config.getInt("reservationTimeForQuestion_sec")
			val assignment = new Assignment(NotAssigned, date, date + timeout, false, questionId, Turkers2TeamsDAO.findSingleTeamByTurkerId(turkerId).id.get)
			val assignmentId = AssignmentDAO.add(assignment)
			assignmentId
		}
	}

	/**
	 * Post - Reject/Remove completely from the database the assignment
	 * @return
	 */
	def rejectAssignment = Action { implicit request =>
		val session = request.session

		request.session.get("turkerId").map {
			turkerId =>
				val assignmentId = request.body.asJson.get \ "assignmentId"
				AssignmentDAO.remove(assignmentId.toString().replace("\"", "").toLong)
				Ok("")
		}.getOrElse {
			Redirect(routes.Application.index())
		}
	}

	/**
	 * Get - Open second step a.k.a. Question overview page
	 * @param paper_id id of the paper chosen on the first step
	 * @return
	 */
	def secondStep(paper_id: Long) = Action { implicit request =>
		val session = request.session

		request.session.get("turkerId").map {
			turkerId =>
				val paper = PaperDAO.findById(paper_id).get
				val config = ConfigFactory.load("application.conf")
				val showReward = config.getBoolean("showReward")
				// If there are available questions for the user redirect to these. Otherwise force redirect to paper overview
				if (QuestionDAO.getQuestionsByPaperId(turkerId, paper.id.get).length > 0) {
					Ok(views.html.waiting_5_2nd_step(turkerById(turkerId), request.flash, paper, Base64.encodeBase64String(HighlightPdf.getPdfAsArrayByte(paper.pdf_path)), showReward))
				} else {
					Redirect(routes.Waiting.waiting())
				}
		}.getOrElse {
			Redirect(routes.Application.index())
		}
	}

}
