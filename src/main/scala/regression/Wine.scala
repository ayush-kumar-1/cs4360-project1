package regression 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression, 
                            CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.Plot

//Scala imports 
import scala.Console
import java.io.{File, FileOutputStream}

//other imports 
import helper.Helper

/**
* Regression Analysis for winequality dataset. 
* @author Ayush Kumar
*/
object Wine extends App {

    val red_wine = Relation.apply("data/WineQuality/winequality-red.csv", "red_wine", 
                                domain=null, key = 0, eSep = ";", cPos = null)
    red_wine.show(5)

    val (x,y) = red_wine.toMatriDD(0 to 10, 11)
    val ox = new MatrixD(x.dim1, 1, 1.0) ++^ x // x augmented with vector of ones 
    //Creating all the models
    //val models: Array[PredictorMat] = new Array[PredictorMat]{
        val MVR = new Regression(ox,y) 
        val Quad = new QuadRegression(x,y)
        val QuadX = new QuadXRegression(x, y) 
        val Cubic = new CubicRegression(x, y) 
        val CubicX = new CubicXRegression(x, y) 
    //} //models
    val Ridge = new RidgeRegression(ox, y) 
    val Lasso = new LassoRegression(ox, y)
    
    val (backReg, _) = Quad.backwardElimAll(cross = false, index_q = Fit.index_rSqBar)
    val (forReg, _) = Quad.forwardSelAll(cross = false, index_q = Fit.index_rSqBar)
    val (stepReg, _) = Helper.stepRegressionAll(QuadX, index_q = Fit.index_rSqBar)

    println(s"back = $backReg")
    println(s"forward = $forReg")
    println(s"step = $stepReg")


} // Wine