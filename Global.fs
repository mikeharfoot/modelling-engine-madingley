namespace Angara.Artefacts.GEMOutputSample.Global

open Angara.Artefacts.GEMConfiguration
open Angara.Artefacts.GEMEnvironment
open Angara.Artefacts.GEMModelState
open Angara.Artefacts.Table

//
// Define a class that implements the interface IMadingleyModelOutput
//
type MadingleyModelOutput(modelState : GEMModelState option,
                          configuration : GEMConfiguration,
                          environment : GEMEnvironment) = 

    //
    // Mutable lists to track the global GEM data
    // These lists (because it's easy to add a new item to a list)
    // and are built up in reverse order (because it is quicker to prepend and very slow to append)
    // and then reversed to make the table (which needs an array - which has a fixed size)
    //
    // Alternatively, if worried about performance, you could create arrays of size = total number of timesteps and fill in the arrays
    // with each timestep
    //
    let mutable g_timeSteps : float list = []
    let mutable g_totalLivingBiomass : float list = []
    let mutable g_organicPoolOut : float list = []
    let mutable g_respiratoryPoolOut : float list = []
    let mutable g_totalNumberOfCohorts : float list = []
    let mutable g_totalNumberOfStocks : float list = []
    let mutable g_numberOfCohortsExtinct : float list = []
    let mutable g_numberOfCohortsProduced : float list = []
    let mutable g_numberOfCohortsCombined : float list = []

    //
    // Calculate the cohort biomass for all grid cells. This iterates over all cells and all cohorts per cell and
    // sums the cohort biomass
    //
    let gridCohortBiomassTotal (gridCellDatas : GridCellData[]) : float =
        gridCellDatas
        |> Array.fold
            (fun total1 (gcd : GridCellData) ->
                total1 + Array.fold
                    (fun total2 (gc : CohortData[]) ->
                        total2 + Array.fold
                            (fun total3 (gcc : CohortData) ->
                                total3 + ((gcc.IndividualBodyMass + gcc.IndividualReproductivePotentialMass) * gcc.Abundance)
                            )
                            0.0
                            gc
                    )
                    0.0
                    gcd.Cohorts
            )
            0.0

    //
    // Calculate the stock biomass for all grid cells. This iterates over all cells and all stocks per cell
    // and sums the stock biomass
    //
    let gridStockBiomassTotal (gridCellDatas : GridCellData[]) : float =
        gridCellDatas
        |> Array.fold
            (fun total1 (gcd : GridCellData) ->
                total1 + Array.fold
                    (fun total2 (gc : StockData[]) ->
                        total2 + Array.fold
                            (fun total3 (gcc : StockData) ->
                                total3 + gcc.TotalBiomass
                            )
                            0.0
                            gc
                    )
                    0.0
                    gcd.Stocks
            )
            0.0

    //
    // Calculate totals for an environment variable for all grid cells
    //
    let gridEnvironmentTotal (variable : string) (gridCellDatas : GridCellData[]) : float =
        Array.fold
            (fun (total : float) (gcd : GridCellData) ->
                total + gcd.Environment.[variable].[0]
            )
            0.0
            gridCellDatas

    //
    // Calculate global values for this timestep and update the mutable data
    //
    let calculateOutputs (currentTimeStep : int) (modelState : GEMModelState) : unit =
        g_timeSteps <- (float)(currentTimeStep + 1) :: g_timeSteps

        let globalDiagnosticVariables = modelState.GlobalDiagnosticVariables
        let gridCellDatas = modelState.GridCellDatas.Value

        // Add total cohort biomass and total stock biomass to the total biomass tracker
        let totalLivingBiomass = (gridCohortBiomassTotal gridCellDatas) + 
                                 (gridStockBiomassTotal gridCellDatas)
        g_totalLivingBiomass <- totalLivingBiomass :: g_totalLivingBiomass

        // Get total organic pool biomass
        let organicPoolOut = gridEnvironmentTotal "Organic Pool" gridCellDatas
        g_organicPoolOut <- organicPoolOut :: g_organicPoolOut

        // Get total respiratory pool biomass
        let respiratoryPoolOut = gridEnvironmentTotal "Respiratory CO2 Pool" gridCellDatas
        g_respiratoryPoolOut <- respiratoryPoolOut :: g_respiratoryPoolOut

        // Get number of cohorts and stocks
        g_totalNumberOfCohorts <- (globalDiagnosticVariables.["NumberOfCohortsInModel"]) :: g_totalNumberOfCohorts
        g_totalNumberOfStocks <- (globalDiagnosticVariables.["NumberOfStocksInModel"]) :: g_totalNumberOfStocks

        // Get numbers of cohort extinctions and productions
        g_numberOfCohortsExtinct <- (globalDiagnosticVariables.["NumberOfCohortsExtinct"]) :: g_numberOfCohortsExtinct
        g_numberOfCohortsProduced <- (globalDiagnosticVariables.["NumberOfCohortsProduced"]) :: g_numberOfCohortsProduced
        g_numberOfCohortsCombined <- (globalDiagnosticVariables.["NumberOfCohortsCombined"]) :: g_numberOfCohortsCombined

    //
    // Implement the IMadingleyModelOutput interface
    //
    interface IMadingleyModelOutput with

        //
        // Run is beginning - prepare the data
        //
        member x.BeginRun(modelState : GEMModelState) : unit =

            g_timeSteps <- []
            g_totalLivingBiomass <- []
            g_organicPoolOut <- []
            g_respiratoryPoolOut <- []
            g_totalNumberOfCohorts <- []
            g_totalNumberOfStocks <- []
            g_numberOfCohortsExtinct <- []
            g_numberOfCohortsProduced <- []
            g_numberOfCohortsCombined <- []

            calculateOutputs (-1) modelState

        member x.BeginYear(year : int) : unit =
            ()

        //
        // Timestep is beginning
        //
        member x.BeginTimestep(currentTimeStep : int,
                               currentMonth : int) : unit =
            ()

        //
        // This cell has been processed
        //
        member x.EndCell(cellIndex : int) : unit =
            ()

        //
        // Store global process data
        //
        member x.StoreNPPGrid(globalProcessTrackerData : GlobalProcessTrackerData) : unit =
            ()

        //
        // Timestep has completed, update the data
        //
        member x.EndTimestep(currentTimeStep : int,
                             processTrackerData : ProcessTrackerData[],
                             crossCellProcessTrackerData : CrossCellProcessTrackerData,
                             dispersals : uint32,
                             modelState : GEMModelState) : unit =
            calculateOutputs currentTimeStep modelState

        //
        // Year has completed, build a new table with all data so far
        //
        member x.EndYear(year : int, modelState : GEMModelState) : GEMModelState * obj =
#if false
            let reverseListToArray (l : float list) : float array =
                let a = Array.create l.Length 0.0

                List.iteri
                    (fun i f -> Array.set a (l.Length - 1 - i) f)
                    l

                a
#else
            let reverseListToArray (l : float list) : float array = l |> List.rev |> List.toArray
#endif

            let table = 
                Angara.Table.ofColumns
                    [
                        "Time step", RealColumn (reverseListToArray g_timeSteps)
                        "Total living biomass", RealColumn (reverseListToArray g_totalLivingBiomass)
                        "Organic matter pool", RealColumn (reverseListToArray g_organicPoolOut)
                        "Respiratory CO2 pool", RealColumn (reverseListToArray g_respiratoryPoolOut)
                        "Number of cohorts extinct", RealColumn (reverseListToArray g_numberOfCohortsExtinct)
                        "Number of cohorts produced", RealColumn (reverseListToArray g_numberOfCohortsProduced)
                        "Number of cohorts combined", RealColumn (reverseListToArray g_numberOfCohortsCombined)
                        "Number of cohorts in model", RealColumn (reverseListToArray g_totalNumberOfCohorts)
                        "Number of stocks in model", RealColumn (reverseListToArray g_totalNumberOfStocks)
                    ]
            Angara.Trace.tracef "RunGEM produced %d rows" (table.GetSchema() |> snd)
            modelState, table :> obj

        member x.EndRun(modelState : GEMModelState) : unit =
            ()

//
// The Factory exports the Create function to build a new implementation of the IMadingleyModelOutput
// interface. Pass in currrent state (if resuming), configuration and environment
//
module Factory =
    let Create(modelState : GEMModelState option,
               configuration : GEMConfiguration,
               environment : GEMEnvironment) : IMadingleyModelOutput =
        MadingleyModelOutput(modelState, configuration, environment) :> IMadingleyModelOutput
