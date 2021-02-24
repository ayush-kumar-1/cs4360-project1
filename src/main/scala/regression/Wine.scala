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
* Regression Analysis for winequality dataset. 
* @author Ayush Kumar
*/
object Wine extends App {

    val red_wine = Relation.apply("data/WineQuality/winequality-red.csv", "red_wine", 
                                domain=null, key = 0, eSep = ";", cPos = null)
    red_wine.show(5)

    val (x,y) = red_wine.toMatriDD(0 to 10, 11)
    val ox = new MatrixD(x.dim1, 1, 1.0) ++^ x // x augmented with vector of ones 

    val mu_x = x.mean
    val mu_y = y.mean

    val x_c = x - mu_x
    val y_c = y - mu_y
    //Creating all the models
    val MVR = new Regression(ox,y) 
    val Quad = new QuadRegression(x,y)
    val QuadX = new QuadXRegression(x, y) 
    val Cubic = new CubicRegression(x, y) 
    val CubicX = new CubicXRegression(x, y) 
    
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
    def plot_and_save (regMat: MatriD, path: String, basePath: String = "plots/Wine/Scala/")  = { 
        val plot = new PlotM(VectorD.range(0, regMat.dim1 - 2), regMat.t, 
            label = Array[String]("R^2", "adj-R^2", "cvR^2"), 
            _title = "Quality of Fit vs. Model Complexity", 
            lines = true)
        
        plot.saveImage(basePath + path) 

    } //plot_and_save

} // Wine