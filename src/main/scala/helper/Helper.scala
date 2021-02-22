package helper 

//ScalaTion imports
import scalation.analytics.{ANCOVA, Regression, QuadRegression, QuadXRegression}
import scalation.analytics.{CubicRegression, CubicXRegression, LassoRegression, RidgeRegression}
import scalation.analytics.PredictorMat
import scalation.analytics.Fit
import scalation.columnar_db.Relation 
import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectoD, VectorI}
import scalation.plot.{Plot, PlotM}
import scalation.scala2d.VizFrame
import scala.collection.mutable.ArrayBuffer

//scala imports 
import scala.Console
import java.io.{File, FileOutputStream}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/**
* Specifies many useful regression utilities using scala that are related to 
* project one. 
* @author Ayush Kumar
*/
object Helper { 

    /**
    * Takes a given regression, does backwards elmination all the way through, 
    * and then optionaly plots R^2, adj-R^2, cv-R^2, AIC for all possible models. 
    * best model is determined by AIC. 
    *
    * @param rg regression for backwardElmination 
    * @param plot true if plots should be generated 
    * @param path where the plots should be saved
    * @param modelName the name of the model 
    * @return optimal regression based on AIC
    */
    def backwardElimination(rg: PredictorMat, modelName: String, 
                            plot: Boolean = false, path: String): PredictorMat =  { 
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set((0 to k).toArray :_*)
        var rsq = new Array[Double](k)
        var adjRsq = new Array[Double](k)
        var cvRsq = new Array[Double](k)
        var aic = new Array[Double](k)
        var bestModel: PredictorMat = null
        
        
        for (i <- 0 to x.dim2 - 3) { 
            val (toBeRemoved, reg) = rg.backwardElim(selectedVars, Fit.index_aic)
            var qoF= reg.analyze().fit
            //updating arrays for plotting purposes
            val fos = new FileOutputStream(new File("./data/Garbage.txt"))
            Console.withOut(fos) {
            cvRsq(i) = reg.analyze().crossValidate()(0).mean
            } //suppressing output
            adjRsq(i) = qoF(1)
            rsq(i) = qoF(0)
            aic(i) = qoF(11)
            
            if (toBeRemoved >= 0) { 
                selectedVars -= toBeRemoved
            } //if

            if (i == 0) { //first time bestmodel will be none
                bestModel = reg; 
            } else if (qoF(11) < bestModel.analyze().fit(11)) { 
                bestModel = reg; 
            } //elif

        } //for i 

        if (plot) { 
            plotQoF(rsq.reverse, adjRsq.reverse, cvRsq.reverse, aic.reverse, modelName, path)
        } //plot
        
        return bestModel

    } //backwardElmination

    /**
    * Takes a given regression, does forward selection all the way through, and 
    * then optionally plots R^2, adj-R^2, cv-R^2, AIC for all possible models. Best
    * model is determined by lowest AIC. 
    * 
    * @param rg regression to forward select 
    * @param plot true if plot should be generated
    * @param path where the plots should be saved
    * @return optimal regression based on AIC 
    */
    def forwardSelection(rg: PredictorMat, modelName: String, plot: Boolean = false, path: String = null): PredictorMat = {
        val x: MatriD = rg.getX
        val k: Int = x.dim2 - 1

        var selectedVars = scala.collection.mutable.Set(0)
        var rsq = new Array[Double](k)
        var adjRsq = new Array[Double](k)
        var cvRsq = new Array[Double](k)
        var aic = new Array[Double](k)
        var bestModel: PredictorMat = null
        
        
        for (i <- 0 to x.dim2 - 2) { 
            val (toBeAdded, reg) = rg.forwardSel(selectedVars, Fit.index_aic)
            var qoF= reg.analyze().fit
            //updating arrays for plotting purposes
            val fos = new FileOutputStream(new File("./data/Garbage.txt"))
            Console.withOut(fos) {
            cvRsq(i) = reg.analyze().crossValidate()(0).mean
            } //suppressing output
            adjRsq(i) = qoF(1)
            rsq(i) = qoF(0)
            aic(i) = qoF(11)
            
            if (toBeAdded >= 0) { 
                selectedVars += toBeAdded
            } //if

            if (i == 0) { //first time bestmodel will be none
                bestModel = reg; 
            } else if (qoF(11) < bestModel.analyze().fit(11)) { 
                bestModel = reg; 
            } //elif

        } //for i 

        if (plot) { 
            plotQoF(rsq, adjRsq, cvRsq, aic, modelName, path)
        } //plot

        return bestModel

    } //forwardSelection

    /**
    * Plots the 4 Quality of Fit measures used for the backwards elmination, 
    * forward selection, and stepwise regression methods. 
    * 
    * @param rsq R^2 values 
    * @param adjRsq adj-R^2 values 
    * @param cvRsq k-fold cross validated R^2 values 
    * @param aic aic values 
    */
    def plotQoF(rsq: Array[Double], adjRsq: Array[Double], cvRsq: Array[Double], aic: Array[Double], modelName: String, 
                path: String) = { 
        val k = rsq.length
        
        //converting arrays for vectors for plotting purposes 
        var rsqV = new VectorD(k, rsq)
        var adjRsqV = new VectorD(k, adjRsq)
        var cvRsqV = new VectorD(k, cvRsq)
        var aicV = new VectorD(k ,aic)
            
        val domain = (0 to k).toArray.map(x => x.toDouble)
        val range = new VectorD(k, domain)
        
        val rsq_plot = new Plot(range, rsqV, _title = "R^2 vs. Model Complexity")
        val adjRsq_plot = new Plot(range, adjRsqV, _title = "adj-R^2 vs. Model Complexity")
        val cvRsq_plot = new Plot(range, cvRsqV, _title = "cv-R^2 vs. Model Complexity")
        val aic_plot = new Plot(range, aicV, _title = "AIC vs. Model Complexity")

        val titlename = path + "/" + modelName
        save_plot(rsq_plot, titlename + "rsq.png")
        save_plot(adjRsq_plot, titlename + "adjRsq.png")
        save_plot(cvRsq_plot, titlename + "cvRsq.png")
        save_plot(aic_plot, titlename + "aic.png")


    } //plotQoF

    /**
    * Saves a given plot to the designated path. 
    * 
    * @param plot the plot to be saved 
    * @param fname the file path name
    */
    def save_plot(plot: VizFrame, fname: String)  = { 
        val bimg: BufferedImage = new BufferedImage(plot.getSize().width, plot.getSize().height, BufferedImage.TYPE_INT_ARGB)
        val gr = bimg.createGraphics()
        plot.paint(gr)
        gr.dispose()
        ImageIO.write(bimg, "png", new File(fname))
    } //save_plot

    /**
    * Stepwise regression is an improvement over backward elmination and forward selection as it 
    * considers adding and removing variables at every step. Default QoF measure is aic. 
    * 
    * @param cols the columns already included in the model at this point 
    * @param index_q the index of the quality of fit measure to be used 
    * @param first the first variable to consider for dropping or adding (default includes intercept)
    * @param rg the regression to use the stepRegression on 
    * @return index of column to be added (negtive integer means should be removed) and the 
    * regression after a single step 
    */
    def stepRegression (cols: scala.collection.mutable.Set[Int], rg: PredictorMat, index_q: Int = Fit.index_aic, 
    first: Int = 1): (Int, PredictorMat) = { 
        //consider forward and backward step 
        if (cols.size <= 2) { 
            return rg.forwardSel(cols, index_q)
        } //if

        val (i_for, forward) = rg.forwardSel(cols, index_q) 
        val (i_back, backward) = rg.backwardElim(cols, index_q, first) 
        //determine which step is best based on index_q
        val aic_for = forward.fit(index_q)
        val aic_back = backward.fit(index_q)
        

        if (aic_for > aic_back || i_back < 0) { 
            return (i_for, forward)
        } //if

        return (-i_back, backward)
    } //stepRegression

    /**
    * Does stepwise regression until the model can no longer be improved upon. This is behavior is different from 
    * the forwardSelection and backwardElimantion methods which continue until the quality of fit meausure chosen
    * no longer improves when variables are added or removed.  
    * 
    * @param rg the regression to find the optimal variable set for 
    * @param index_q the QoF measure to be used (defualt aic) 
    * @param first the first variable to considered for adding or dropping 
    * @param cross true if using 10-fold cross validation for QoF measures 
    * @return the optimal set of paramters, and a matrix with all quality of fit measures for every step
    */
    def stepRegressionAll(rg: PredictorMat, index_q: Int = Fit.index_aic, 
    first: Int = 1, cross: Boolean = true): (scala.collection.mutable.Set[Int], MatriD) = { 

        var cols = scala.collection.mutable.Set(0)
        
        //always take the first step 
        var (i, currentStep): (Int, PredictorMat) = stepRegression(cols, rg, index_q) 
        var fit = currentStep.fit(index_q)
        var step_stats: ArrayBuffer[VectoD] = new ArrayBuffer[VectoD]()
        step_stats += currentStep.fit
        var counter: Int = 1
        var output: String = null

        if (i < 0) { 
            cols -= -i
            output = s"<== StepRegression: remove (#${counter}) variable ${-i}, qof = $fit"
        } else { 
            cols += i
            output = s"==> StepRegression: add (#${counter}) variable $i, qof = $fit"
        } //if-else
        println(output)

        var prev = i 

        
        for (j <- 0 to rg.getX.dim2 - 3) {
            counter+= 1
            var (i, currentStep) = stepRegression(cols, rg, index_q) 
            var newFit = currentStep.fit(index_q) 

            step_stats += currentStep.fit
            fit = currentStep.fit(index_q)
            if (i < 0) { 
                cols -= -i
                output = s"<== StepRegression: remove (#${counter}) variable ${-i}, qof = $fit"
            } else { 
                cols += i
                output = s"==> StepRegression: add (#${counter}) variable $i, qof = $fit"
            } //if-else
            println(output)

            //check for convergence (removing and adding the same variable) 
            if (-i == prev) { 
                return (cols, MatrixD.apply(step_stats.toArray))
            } //if

            prev = i 
        } //while
        
        return (cols, MatrixD.apply(step_stats.toArray))

    } //stepRegressionAll

} //Helper