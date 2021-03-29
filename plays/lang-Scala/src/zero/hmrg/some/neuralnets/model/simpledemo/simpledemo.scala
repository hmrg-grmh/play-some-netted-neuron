package zero.hmrg.some.neuralnets.model ;

/*
seq net(same as layers):
    1 layer
    2 layer
    3 layer
    ...

list layer(same as lines):
    1 line
    2 line
    3 line
    ...

list,num line:
    weights,num ;

list weights:
    1 weight
    2 weight
    ...
*/

/******************************************************/

package object simpledemo
{
    object Type
    {
        trait NeuralNet ;
        trait InOutLine ;
    } ; /* : object Type : */
    
    /**
     * make a simple neural-net from zero
     * use only one file, may the code can be compressed by simple way
     * use only basic feature in Scala-lang
     * I want the code as easy as possible for read, means should be less ambiguity
     * and much readable by format even without IDE
     * I do not like the conceptions be made for only one domain
     * language and words only can be alive when it be used frequently
     * and only the simple less-rule one can deserved to be used frequently
     * so I will only use the visualized words to make names
     * ---------------------------------
     * /* :xxx XXX: */ :
     *    means The xxx XXX is OVER HERE , it is OVER .
     *    means The xxx XXX " YiJing JieShu Le, BuYao HuanXiang Le " .
     */
    case class SimpleNeuralNet
    ( weightLineBiasedListSeq: Seq[(Int,List[(Int,(List[(Int,Double)],Double))])] )
    ( sizeIn:Int, sizeOut:Int )
        extends Type.NeuralNet
    {
        lazy val getNumberOfHiddenLayer
        : Int = weightLineBiasedListSeq.size - 1
    } ; /* :case class SimpleNeuralNet: */
    object SimpleNeuralNet
        extends Type.NeuralNet
    {
        
        object Maker
        {
            /**
             * one line is : many weights -- 1 bia
             * one layer is : many lines
             * one net is : many layers
             */
            
            object Mode
            {
                trait SomeMakingMode ;
                object RANDOM extends SomeMakingMode ;
                object ZERO extends SomeMakingMode ;
            } ; /* :object Mode: */
            
            def makeNet
            ( modeChoose: Mode.SomeMakingMode = Mode.RANDOM )
            ( sizeLineIn:Int, sizeLineOut:Int, sizesForHiddenLayer:Seq[Int] )
            : SimpleNeuralNet =
            {
                val sizesForLayer
                : List[Int] = sizesForHiddenLayer.toList ::: List(sizeLineOut) ;
                
                val sizeForNet
                : Int = sizesForLayer.size ;
                
                val sizeOfLines
                : Seq[(Int,Int)] =
                    (1 to sizeForNet) zip
                    (List(sizeLineIn) ::: sizesForHiddenLayer.toList) ;
                
                def getLineSizeForLayer (indexOfLayer:Int)
                : Int =
                {
                    return sizeOfLines.toMap.get(indexOfLayer)getOrElse(-1) ;
                } ; /* :def getLineSizeForLayer: */
                
                def makeRandomLineBia (sizeOfLine:Int)
                : (List[(Int, Double)], Double) =
                {
                    val indexedLine
                    : List[(Int, Double)] =
                        (1 to sizeOfLine)
                            .toList
                            .map(
                                indexInLine =>
                                    indexInLine ->
                                    scala.util.Random.nextDouble
                                ) ;
                    return indexedLine ->
                           ( scala.util.Random.nextDouble +
                             scala.util.Random.nextInt(32) - 16 -
                             scala.util.Random.nextDouble ) ;
                } ; /* :def makeRandomLineBia: */
                
                def makeZeroLineBia (sizeOfLine:Int)
                : (List[(Int, Double)], Double) =
                {
                    val indexedLine
                    : List[(Int, Double)] =
                        (1 to sizeOfLine)
                            .toList
                            .map( indexInLine => indexInLine -> 0.0 ) ;
                    return indexedLine -> 0.0 ;
                } ; /* :def makeZeroLineBia: */
                
                val makeLineBia
                : (Int) => (List[(Int, Double)], Double) =
                    modeChoose match
                    {
                        case Mode.RANDOM => makeRandomLineBia _
                        case _ | Mode.ZERO => makeZeroLineBia _
                    } ; /* :val makeLineBia: */
                
                val netDataMade
                : Seq[(Int,List[(Int,(List[(Int,Double)],Double))])] =
                    ((1 to sizeForNet) zip sizesForLayer)
                        .toSeq
                        .map
                        {
                            case (indexOfLayer,sizeLayer) =>
                                indexOfLayer ->
                                (1 to sizeLayer)
                                    .toList
                                    .map(
                                        /* here need to be: one index -> one line-bia */
                                        indexOfLine =>
                                            indexOfLine ->
                                            makeLineBia(getLineSizeForLayer(indexOfLayer))
                                        )
                            /* here need to be: one index -> one layer */
                        } ; /* :val netDataMade: */
                return SimpleNeuralNet
                    .apply(
                        netDataMade)(
                        sizeIn = sizeLineIn,
                        sizeOut = sizeLineOut) ;
            } ; /* :def makeNet: */
        } ; /* :object SimpleNeuralNet: */
        
        object SomeGift
        {
            /**
             * use to make the neuron-num always be some positive number
             */
            def relu (n:Double)
            : Double =
            {
                return if (n < 0) 0 else n ;
            } ; /* :def relu: */
        } ; /* :object SomeGift: */
        
        object Operator
        {
            def multiplyListElems
            ( a:List[Double], b:List[Double] )
            : List[Double] =
            {
                return (a.map(Some(_)) zip b.map(Some(_)))
                    .map{ case (Some(a),Some(b)) => a * b } ;
            } ; /* : def multiplyListElems : */
            def getNextLayerNeuronList
            ( neuronList:List[Double], neuralJunctionGroupList:List[List[Double]] )
            : List[Double] =
            {
                /**
                 * neuralJunctionGroup.Size should be eq to neuronList.Size
                 * neuralJunctionGroupList.Size should be eq to nextLayerNeuronList.Size
                 */
                return neuralJunctionGroupList
                    .map(
                        neuralJunctionGroup =>
                            multiplyListElems(neuralJunctionGroup, neuronList).sum
                        ) ;
            } ; /* :def getNextLayerNeuronList: */
        } ; /* :object Operator: */
        
        
        object Model
        {
            case class ModelGroup
            ( modelList:List[(Int,Type.NeuralNet)] )
            {  } ; /* :case class ModelGroup: */
            object ModelGroup
            {
                def groupMaker
                ( size:Int , netMakingMode:Maker.Mode.SomeMakingMode = Maker.Mode.RANDOM )
                ( sizeLineIn:Int, sizeLineOut:Int, sizesForHiddenLayer:Seq[Int] )
                : ModelGroup =
                {
                    val modelListGot
                    : List[(Int,Type.NeuralNet)] =
                        (1 to size)
                            .toList
                            .map(
                                modelIndex =>
                                    modelIndex ->
                                    Maker.makeNet(
                                        netMakingMode)(
                                        sizeLineIn,
                                        sizeLineOut,
                                        sizesForHiddenLayer)
                                ) ;
                    return ModelGroup.apply(modelListGot) ;
                } ; /* :def groupMaker: */
            } ; /* :object ModelGroup: */
            
            /* model to do sth - actively */
            object Action
            {
                
                class OutData extends Type.InOutLine ;
                class InData extends Type.InOutLine ;
                case class OutLine
                ( data: Array[Double] )
                    extends OutData
                {  } ;
                case class Cost
                ( value: Double )
                    extends OutData
                {  } ;
                case class InLine
                ( data: Array[Double] )
                    extends InData
                {  } ;
                case object Line
                    extends Type.InOutLine
                {
                    //
                } ; /* :case object Line: */
                
                
                /* only way to use ModelGroup in normal or in training */
                /**
                 * return :
                 *    List of resArray(OutLine) with cost,
                 *    for everyone(have-index) in the group(List)
                 */
                def invokeModel
                ( groupedModels:ModelGroup , environmentEvolveIn:Environment.TabledData )
                : List[(Int,(Cost,OutLine))] =
                {
                    
                    val envTableBody = environmentEvolveIn.body ;
                    
                    def oneNetOneEventCostedResultGetter
                    ( model:SimpleNeuralNet  )
                    : (Cost,OutLine) =
                    {
                        
                        def dataLineOnOneWBLineResultGetter
                        ( wbLine: (List[(Int,Double)],Double) )
                        ( dataIn: InLine )
                        : Double =
                        {
                            val bias : Double = wbLine._2 ;
                            val weightLine : List[(Int,Double)] = wbLine._1 ;
                            
                            val dataLineMultipliedWeightLine
                            : List[((Int, Double), Int)] =
                                (dataIn.data.toList.zipWithIndex zip weightLine)
                                    .map
                                    {
                                        case ((dataValue,dataIndex),(indexOfWeight,weight)) =>
                                            (indexOfWeight -> dataValue.*(weight)) ->
                                            (indexOfWeight - 1 - dataIndex)
                                    } ; /* :val dataLineMultipliedWeightLine: */
                            
                            val preResultGot
                            : Double =
                                dataLineMultipliedWeightLine
                                    .filter(_._2 == 0)
                                    .map
                                    {
                                        case ((indexOfWeight,valueMultipliedWeight),verifiedCode) =>
                                            valueMultipliedWeight
                                    } /* here index is not important , because then will be sum */
                                    .sum
                                    .+(bias) ; /* :val preResultGot: */
                            
                            return SomeGift.relu(preResultGot) ;
                        } ; /* :def dataLineOnOneWBLineResultGetter: */
                        
                        
                        model
                            .weightLineBiasedListSeq
                            .map
                            {
                                case (indexOfLayer,weightLineBiasedList) =>
                                    indexOfLayer ->
                                    weightLineBiasedList
                                        .map
                                        {
                                            case (indexOfLine,weightLineBiased) =>
                                                indexOfLine ->
                                                weightLineBiased
                                        }
                            }
                    } ;
                    groupedModels.modelList
                    return
                } ; /* :def invokeModel: */
                /* way to output ModelGroup */
                def importModel (srcFilePath:String) =
                {
                    //return
                } ;
                /* way to output ModelGroup */
                def exportModel (aimFilePath:String) =
                {
                    //return
                } ;
            } ; /* :object Action: */
            
            /* model be do sth - passively */
            object Evolution
            {
                /* test ModelGroup then know res and costs then evolve if cost too much */
                def practice () =
                {
                    //return
                } ;
                /* way to minus costs then practice */
                def evolve (module: Type.NeuralNet) =
                {
                    //return
                } ;
                /* evolve is change after practice ,
                   practice is test then evolve-or-just rt res:good-enough
                _  */
                
                /* <a Group of Model> update it-self automatically ... if i can do that ... */
                def revolute (module: Type.NeuralNet) =
                {
                    //return
                } ;
            } ;
            
            
        } ; /* :object Model: */
        
        
        object Environment
        {
            trait Data ;
            
            case class TabledData
            ( head:Array[String] , body:List[Array[String]] )
                extends Data
            {  } ;
            
            case class Event
            ( eventFeatureData: Array[Double] , eventLabelData: Array[Double])
                extends Type.InOutLine
            {  } ;
            object Event
            {
                
                def eventListMaker
                ( byTabledDataFeature: TabledData, byOutTabledDataLabel: TabledData )
                : List[(Array[Double], Array[Double])] =
                {
                    
                    def oneLineStringToDouble
                    ( fields: Array[String] )
                    : Array[Double] =
                        fields.map(_.toDouble) ;
                    
                    val eventListGot
                    : List[(Array[Double], Array[Double])] =
                        (byTabledDataFeature.body.zipWithIndex zip byOutTabledDataLabel.body.zipWithIndex )
                            .toList
                            .map
                            {
                                case ((oneLineFeature,featureIndex),(oneLineLabel,labelIndex)) =>
                                    oneLineStringToDouble(oneLineFeature) ->
                                    oneLineStringToDouble(oneLineLabel) ->
                                    (featureIndex - labelIndex)
                            }
                            .filter(_._2 == 0)
                            .map
                            {
                                case (eventFeatureWithLabelVerified,verifyCode) =>
                                    eventFeatureWithLabelVerified
                            } ; /* :val eventList: */
                    
                    return eventListGot ;
                } ; /* :def eventListMaker: */
                
            } ;
            
            def csvReader (path: String)
            : Environment.TabledData =
            {
                val csvFile
                : scala.io.BufferedSource =
                    scala.io.Source.fromFile(path) ;
                
                import sys.process._ ;
                val csvLineList
                : List[Array[String]] =
                    csvFile
                        .getLines()
                        .toList
                        .map(
                            line =>
                                line.split(",")
                                    .map( field => s"echo -e ${field}".!! )
                            ) ;
                
                csvFile.close() ;
                
                return TabledData( csvLineList.head, csvLineList.tail ) ;
            } ;
            
        } ;
        
        
    } ;
} ;
