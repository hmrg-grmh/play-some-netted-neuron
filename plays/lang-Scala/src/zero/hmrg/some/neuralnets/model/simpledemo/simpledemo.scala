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
    ( weightLineBiasedListSeq: Seq[(Int,List[(Int,SimpleNeuralNet.WBLine)])],
      sizeIn:Int, sizeOut:Int, sizesForHiddenLayer:Seq[Int] )
        extends Type.NeuralNet
    {  } ; /* :case class SimpleNeuralNet: */
    
    /**
     * one line is : many weights -- 1 bia
     * one layer is : many lines
     * one net is : many layers
     */
    object SimpleNeuralNet
        extends Type.NeuralNet
    {
        
        case class WBLine
        (data:(List[(Int,Double)],Double))
        {  } ;
        
        object WBLine
        {
            def errorWBLineMaker
            : WBLine =
            {
                return WBLine.apply(List(-1 -> -1.1) -> -1.1) ;
            } ;
            
            def wbLineGetter
            (model:SimpleNeuralNet)
            (indexOfLayer:Int, indexOfWBLine:Int)
            : WBLine =
            {
                return model
                    .weightLineBiasedListSeq
                    .toMap
                    .getOrElse( indexOfLayer , List(-1 -> WBLine.errorWBLineMaker) )
                    .toMap
                    .getOrElse( indexOfWBLine , WBLine.errorWBLineMaker ) ;
            } ;
        } ;
        
        object Layer
        {
            def layerSizeGetter
            ( model: SimpleNeuralNet )
            ( indexOfLayer: Int )
            :Int =
            {
                val sizeOfNet
                : Int =
                    model
                        .weightLineBiasedListSeq
                        .toMap
                        .getOrElse(indexOfLayer,List(-1 -> WBLine.errorWBLineMaker))
                        .toMap
                        .size ;
                return sizeOfNet - 1 ;
            }
        } ;
        
        object Maker
        {
            object Mode
            {
                trait SomeMode ;
                object RANDOM extends SomeMode ;
                object ZERO extends SomeMode ;
            } ; /* :object Mode: */
            
            def netMaker
            ( modeChoose: Mode.SomeMode = Mode.RANDOM )
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
                    return sizeOfLines.toMap.get(indexOfLayer).getOrElse(-1) ;
                } ; /* :def getLineSizeForLayer: */
                
                def makeRandomWBLine (sizeOfLine:Int)
                : WBLine =
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
                    return WBLine
                        .apply(
                            indexedLine ->
                            (
                            scala.util.Random.nextDouble +
                            scala.util.Random.nextInt(32) - 16 -
                            scala.util.Random.nextDouble ) ) ;
                } ; /* :def makeRandomWBLine: */
                
                def makeZeroWBLine (sizeOfLine:Int)
                : WBLine =
                {
                    val indexedLine
                    : List[(Int, Double)] =
                        (1 to sizeOfLine)
                            .toList
                            .map( indexInLine => indexInLine -> 0.0 ) ;
                    return WBLine.apply(indexedLine -> 0.0) ;
                } ; /* :def makeZeroWBLine: */
                
                val makeWBLine
                : (Int) => WBLine =
                    modeChoose match
                    {
                        case Mode.RANDOM => makeRandomWBLine _
                        case _ | Mode.ZERO => makeZeroWBLine _
                    } ; /* :val makeWBLine: */
                
                val netDataMade
                : Seq[(Int,List[(Int,WBLine)])] =
                    ( (1 to sizeForNet) zip sizesForLayer )
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
                                            makeWBLine( getLineSizeForLayer( indexOfLayer ) )
                                        )
                            /* here need to be: one index -> one layer */
                        } ; /* :val netDataMade: */
                
                return SimpleNeuralNet
                    .apply(
                        netDataMade,
                        sizeIn = sizeLineIn,
                        sizeOut = sizeLineOut,
                        sizesForHiddenLayer = sizesForHiddenLayer) ;
            } ; /* :def netMaker: */
        } ; /* :object SimpleNeuralNet: */
        
        object SomeGift
        {
            /**
             * use to make the neuron-num always be some positive number
             */
            def relu (n:Double)
            : Double =
            {
                return if ( n < 0 ) 0 else n ;
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
                            multiplyListElems(neuralJunctionGroup, neuronList)
                                .sum
                        ) ;
            } ; /* :def getNextLayerNeuronList: */
            
            def getCostLine
            (array1:Array[Double], array2:Array[Double])
            : List[Double] =
            {
                return (array1 zip array2)
                    .toList
                    .map
                    {
                        case (num1,num2) =>
                            val x = num1 - num2 ; x * x
                    } ;
            } ;
        } ; /* :object Operator: */
        
        
        object Model
        {
            case class ModelGroup
            ( modelList:List[(Int,SimpleNeuralNet)] )
            {  } ; /* :case class ModelGroup: */
            
            object ModelGroup
            {
                def groupMaker
                ( size:Int , netMakingMode:Maker.Mode.SomeMode = Maker.Mode.RANDOM )
                ( sizeLineIn:Int, sizeLineOut:Int, sizesForHiddenLayer:Seq[Int] )
                : ModelGroup =
                {
                    val modelListGot
                    : List[(Int,SimpleNeuralNet)] =
                        (1 to size)
                            .toList
                            .map(
                                modelIndex =>
                                    modelIndex ->
                                    Maker.netMaker(
                                        netMakingMode )(
                                        sizeLineIn ,
                                        sizeLineOut ,
                                        sizesForHiddenLayer )
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
                case class BeingLine
                ( data: Array[Double] )
                    extends InData
                {  } ;
                
                
                /* only way to use ModelGroup in normal or in training */
                /**
                 * return :
                 *    List of resArray(OutLine) with cost,
                 *    for everyone(have-index) in the group(List)
                 */
                def invokeModel
                ( groupedModels:ModelGroup ,
                  envFeatureTable:Environment.TabledData ,
                  envLabelTable:Environment.TabledData )
                : List[(Int,(Cost,OutLine))] =
                {
                    val eventDatas
                    : (List[Environment.Event], (Array[String], Array[String])) =
                        Environment.Event.eventListMaker( envFeatureTable, envLabelTable ) ;
                    
                    val eventList : List[Environment.Event] = eventDatas._1 ;
                    val headOfEventList : (Array[String], Array[String]) = eventDatas._2 ;
                    
                    def oneNetOneEvent
                    ( model:SimpleNeuralNet, event:Environment.Event )
                    : (Cost,OutLine) =
                    {
                        val feature : Array[Double] = event.eventFeatureData ;
                        val label : Array[Double] = event.eventLabelData ;
                        
                        def eventBeingWBLine
                        ( eventBeing: BeingLine, wbLine: WBLine )
                        : Double =
                        {
                            val bias : Double = wbLine.data._2 ;
                            val weightLine : List[(Int,Double)] = wbLine.data._1 ;
                            
                            val dataLineMultipliedWeightLine
                            : List[((Int, Double), Int)] =
                                ( eventBeing.data.toList.zipWithIndex zip weightLine)
                                    .map
                                    {
                                        case ((dataValue,dataIndex),(indexOfWeight,weight)) =>
                                            (indexOfWeight -> dataValue.*(weight)) ->
                                            (indexOfWeight - 1 - dataIndex)
                                    } ; /* :val dataLineMultipliedWeightLine: */
                            
                            val sumedMultiplieds
                            : Double =
                                dataLineMultipliedWeightLine
                                    .filter(_._2 == 0)
                                    .map
                                    {
                                        case ((indexOfWeight,valueMultipliedWeight),verifiedCode) =>
                                            valueMultipliedWeight
                                    } /* here index is not important , because then will be sum */
                                    .sum ; /* :val sumedMultiplieds: */
                            
                            return SomeGift.relu(sumedMultiplieds + bias) ;
                        } ; /* :def eventBeingWBLine: */
                        
                        /*
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
                                                eventBeingWBLine(
                                                    weightLineBiased)(
                                                    BeingLine( feature ) )
                                        }
                            }
                        */
                        
                        def netLayersCalculator
                        ( model: SimpleNeuralNet , feature: Array[Double] )
                        : OutLine =
                        {
                            val sizesForLayers
                            : Seq[Int] = model.sizesForHiddenLayer ;
                            val sizeIn : Int = model.sizeIn ;
                            val sizeOut : Int = model.sizeOut ;
                            
                            @scala.annotation.tailrec
                            def layerIter
                            ( beingLine: BeingLine, iterPassedCount: Int = 0 )
                            ( numberOfHiddenLayer: Int )
                            : BeingLine =
                            {
                                if
                                ( iterPassedCount > numberOfHiddenLayer )
                                {
                                    return beingLine ;
                                }
                                else
                                {
                                    val atIndexOfLayer
                                    : Int = iterPassedCount + 1 ;
                                    
                                    val newBeingLineData
                                    : Array[Double] =
                                        ( 1 to Layer.layerSizeGetter(model)(atIndexOfLayer) )
                                            .toList
                                            .map(
                                                indexOfWBLine =>
                                                    indexOfWBLine ->
                                                    WBLine.wbLineGetter(
                                                        model )(
                                                        atIndexOfLayer ,
                                                        indexOfWBLine )
                                                )
                                            .map
                                            {
                                                case (indexOfWBLine,wbLine) =>
                                                    indexOfWBLine ->
                                                    eventBeingWBLine(beingLine,wbLine)
                                            }
                                            .sortBy(_._1)
                                            .map(_._2)
                                            .toArray ;
                                    
                                    return layerIter(
                                        BeingLine.apply(newBeingLineData) ,
                                        iterPassedCount + 1 )(
                                        numberOfHiddenLayer ) ;
                                } ;
                            } ;
                            
                            val theLastBeingLine
                            : BeingLine =
                                layerIter(
                                    BeingLine.apply(feature) ,
                                    iterPassedCount = 0 )(
                                    model.sizesForHiddenLayer.size )
                            
                            return OutLine.apply(theLastBeingLine.data) ;
                        } ; /* :def netLayersCalculator: */
                        
                        val outLine
                        : OutLine = netLayersCalculator( model, feature ) ;
                        
                        val costLine
                        : List[Double] = Operator.getCostLine( outLine.data, label ) ;
                        
                        return Cost.apply(costLine.sum) -> outLine ;
                    } ;
                    
                    val modelGroupResults
                    : List[(Int,(Cost,OutLine))] =
                        eventList
                            .map(
                                event =>
                                    groupedModels
                                        .modelList
                                        .map
                                        {
                                            case (indexOfModel,model) =>
                                                indexOfModel ->
                                                oneNetOneEvent(model, event)
                                        }
                                )
                            .flatten ;
                    return modelGroupResults ;
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
                /* or newer replace older ? new group replace old */
                def revolute (module: Type.NeuralNet) =
                {
                    /* two model do this */
                    def procreate =
                    {
                        //burn
                        //swop: <some> w at same index exchange
                        //change: every w change small
                    } ;
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
                /**
                 * need F and L table is one-line with one-line
                 * @param byTabledDataFeature
                 * @param byTabledDataLabel
                 * @return
                 */
                def eventListMaker
                ( byTabledDataFeature: TabledData, byTabledDataLabel: TabledData )
                : (List[Event],(Array[String], Array[String])) =
                {
                    val featureHead = byTabledDataFeature.head ;
                    val labelHead = byTabledDataLabel.head ;
                    
                    def oneLineStringToDouble
                    ( fields : Array[String] )
                    : Array[Double] =
                    {
                        return fields.map( _.toDouble ) ;
                    } ;
                    
                    val eventListGot
                    : List[Event] =
                        ( byTabledDataFeature.body.zipWithIndex zip byTabledDataLabel.body.zipWithIndex )
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
                            }
                            .map
                            {
                                case (featureLine,labelLine) =>
                                    Event.apply(featureLine,labelLine)
                            } ; /* :val eventList: */
                    
                    return eventListGot -> (featureHead,labelHead) ;
                } ; /* :def eventListMaker: */
                
            } ;
            
            def csvReader
            ( path: String )
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
                                line.split( "," )
                                    .map(
                                        field =>
                                            s"echo -e ${ field }".!!
                                        )
                            ) ;
                
                csvFile.close() ;
                
                return TabledData( csvLineList.head, csvLineList.tail ) ;
            } ;
            
        } ;
        
        
    } ;
} ;
