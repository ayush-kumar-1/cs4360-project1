package regression 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression, 
                            CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.{Plot, PlotM} 
import scalation.util.banner

//Scala imports 
import scala.Console
import java.io.{File, FileOutputStream}

//other imports 
import helper.Helper

/**
* Regression Analysis for SeoulBike dataset. 
* @author Brandon Amirouche
*/
object SeoulBike extends App {

    val bike = Relation.apply("data/SeoulBikeDataCleaned.csv", "bike", 
                                domain=null, key = 0, eSep = ",", cPos = null)
    bike.show(5)










    //Here, we attach the last 5 columns away from the squaring of the Matrix in order to avoid total Colinearity, which will
    //corrupt the results and produce NaN or infinite answers

    val (x,y) = bike.toMatriDD(1 to 14, 0)

    val xQuad = new QuadRegression(Helper.reduce_coll(new Regression(bike.toMatriD(1 to 9), y), threshold = 10), y).getX ++^ bike.toMatriD(10 to 14)

    val xQuadX = new QuadXRegression(Helper.reduce_coll(new Regression(bike.toMatriD(1 to 9), y), threshold = 10), y).getX ++^ bike.toMatriD(10 to 14)

    val xCubic = new CubicRegression(Helper.reduce_coll(new Regression(bike.toMatriD(1 to 9), y), threshold = 10), y).getX ++^ bike.toMatriD(10 to 14)

    val xCubicX = new CubicXRegression(Helper.reduce_coll(new Regression(bike.toMatriD(1 to 9), y), threshold = 10), y).getX ++^ bike.toMatriD(10 to 14)

    //val xDQuad = new QuadRegression(xD,y)
    //println("hello world")
    val cx  = Helper.reduce_coll(new Regression(x, y), threshold = 2)
    val ox = new MatrixD(x.dim1, 1, 1.0) ++^ x // x augmented with vector of ones 

    val mu_x = x.mean
    val mu_y = y.mean

    val x_c = x - mu_x
    val y_c = y - mu_y
    //Creating all the models
    val MVR = new Regression(ox,y) 
    //val Quad = new QuadRegression(cx,y)
    val Quad = new Regression(xQuad, y)

    val QuadX = new Regression(xQuadX, y) 
    
    //val CubicX = new Regression(Helper.reduce_coll(new CubicXRegression(x, y)), y)
    //val cx  = Helper.reduce_coll(new Regression(x, y), threshold = 10)
    val Cubic = new Regression(xCubic, y)
    val CubicX = new Regression(xCubicX, y)
    
    //val Cubic = new CubicRegression(Helper.reduce_coll(new Regression(x, y)), y) 
    //val CubicX = new CubicXRegression(Helper.reduce_coll(new Regression(x, y)), y) 
    
    val Ridge = new RidgeRegression(x_c, y_c) 
    val Lasso = new LassoRegression(x_c, y_c)

    banner("Correlation Matrix")
    println(MVR.corrMatrix())

    
    var (forward, backward, stepwise) = run_model(MVR, "MultipleLinearRegression")
    var (forwardQ, backwardQ, stepwiseQ) = run_model(Quad, "QuadraticRegression")
    var (forwardQX, backwardQX, stepwiseQX) = run_model(QuadX, "QuadraticCrossRegression")
    var (forwardC, backwardC, stepwiseC) = run_model(Cubic, "CubicRegression")
    var (forwardCX, backwardCX, stepwiseCX) = run_model(CubicX, "CubicCrossRegression")


    banner("Multiple Linear Regression")
    println(s"Forward: ${forward.fitMap} \n\nBackward: ${backward.fitMap} \n\nStepwise: ${stepwise.analyze().fitMap}")
    banner("Quadratic Regresssion")
    println(s"Forward: ${forwardQ.fitMap} \n\nBackward: ${backwardQ.fitMap} \n\nStepwise: ${stepwiseQ.analyze().fitMap}")
    banner("Quadratic Regression with Cross Terms")
    println(s"Forward: ${forwardQX.fitMap} \n\nBackward: ${backwardQX.fitMap} \n\nStepwise: ${stepwiseQX.analyze().fitMap}")
    banner("Cubic Regression")
    println(s"Forward: ${forwardC.fitMap} \n\nBackward: ${backwardC.fitMap} \n\nStepwise: ${stepwiseC.analyze().fitMap}")
    banner("Cubic Regression with Cross Terms")
    println(s"Forward: ${forwardCX.fitMap} \n\nBackward: ${backwardCX.fitMap} \n\nStepwise: ${stepwiseCX.analyze().fitMap}")

    banner("Ridge Regression") 

    Ridge.findLambda
    println(Ridge.analyze().summary)

    banner("Lasso Regression")
    println(Lasso.analyze().summary)

    /**
    * Does forward selection, backward elimination and stepwise regression 
    * for the given model, then plots and saves the tree models and then 
    * returns the 3 models generated from the process. 
    * 
    * @param model the regression model to be used for all steps 
    * @param modelName the name of the regression model
    * @return array of regressions in order of forward, backward, stepwise
    */
    def run_model (model: PredictorMat, modelName: String): (PredictorMat, PredictorMat, PredictorMat) = { 
        var (forReg, forRegMat) = model.forwardSelAll() 
        var (backReg, backRegMat) = model.backwardElimAll() 
        var (stepReg, stepRegMat) = model.stepRegressionAll()

        plot_and_save(backRegMat, modelName + "BackElim.png")
        plot_and_save(forRegMat, modelName + "ForwardSel.png")
        plot_and_save(stepRegMat, modelName + "StepReg.png")

        val resultArray = new Array[PredictorMat](3)
        resultArray(0)  = Helper.forwardSelection(model)
        resultArray(1) = Helper.backwardElimination(model) 
        resultArray(2) = model.buildModel(model.getX.selectCols(stepReg.toArray))

        return (resultArray(0), resultArray(1), resultArray(2))    
    } //run_model

    /**
    * Plots and saves the resutls of the backwards/forwards 
    * variable selection process. 
    *
    * @param regMat the matrix that results from the process 
    * @param path the path to be saved at 
    */
    def plot_and_save (regMat: MatriD, path: String, basePath: String = "plots/Bike/")  = { 
        val plot = new PlotM(VectorD.range(0, regMat.dim1 - 2), regMat.t, 
            label = Array[String]("R^2", "adj-R^2", "cvR^2"), 
            _title = "Quality of Fit vs. Model Complexity", 
            lines = true)
        
        plot.saveImage(basePath + path) 

    } //plot_and_save

} // Wine