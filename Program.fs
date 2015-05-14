open System.IO

open Angara

open Angara.Artefacts.Assembly
open Angara.Artefacts.GEMConfiguration
open Angara.Artefacts.GEMEnvironment
open Angara.Artefacts.Table
open Angara.Methods


let noTransform index (properties : Map<string, float>) : Map<string, float> =
    properties

let transformMaximumMassRow11 index (properties : Map<string, float>) : Map<string, float> =
    if index = 11 then
        properties
        |> Map.map
            (fun (key : string) (value : float) ->
                if key = "maximum mass" then
                    100.0
                else value
            )
    else properties

let transformFunctionalGroups (fg : FunctionalGroupDefinitionsData) (transformProperties) : FunctionalGroupDefinitionsData =

    let transformData (fgda : FunctionalGroupDefinitionData[]) : FunctionalGroupDefinitionData[] =
        fgda
        |> Array.mapi
            (fun (index : int) (fgd : FunctionalGroupDefinitionData) ->
                { fgd with Properties = transformProperties index fgd.Properties }
            )   

    { fg with Data = transformData(fg.Data) }


let transformConfig (c : GEMConfiguration) transformProperties : GEMConfiguration =
    let tc = { c with CohortFunctionalGroupDefinitions = transformFunctionalGroups(c.CohortFunctionalGroupDefinitions) transformProperties }

    tc



[<EntryPoint>]
let main argv = 

    // Path to the configuration folder, this should contain at least EcosystemModelInitialisation.csv
    let pathToModelSetup = "model"

    // Path to the environmental data folder:
    let pathToData = "data"

    // Where to store or load the environment data
    let environmentFileName = "environment.zip"

    // Load the configuration data
    let configuration = ImportGEMConfiguration.run(pathToModelSetup)

    // Load the environment data
    let environment =
        if File.Exists environmentFileName then
            GEMEnvironment.Load environmentFileName
        else
            let env = ImportGEMEnvironment.run(pathToData, pathToModelSetup)

            GEMEnvironment.Save env environmentFileName

            env

    // Load an assembly that defines a function to format the output
    let outputFactory = Assembly[FunctionSignature.Make <@ Angara.Artefacts.GEMOutputSample.Global.Factory.Create @>]

    
    let ensemble : (int -> Map<string, float> -> Map<string, float>)[] =
                [|
                    noTransform
                    transformMaximumMassRow11
               |]
    let ensembleStrings : (string)[] = 
        [|
            "noTransform"
            "transformMaximumMassRow11"
        |]

    let tables = 
        ensemble
        |> Array.map
            (fun propertyMap ->
                let configuration = transformConfig configuration propertyMap
                [1..2]
                |> List.map
                    (fun _ ->
                        async {
                            // Run the GEM!
                            let (_, table) = RunGEM.run<ITable>(None, configuration, environment, outputFactory) |> Seq.last

                            return table
                
                        }
                    )
                |> Async.Parallel
                |> Async.RunSynchronously
            )


    // Create output directory using relative path
    Directory.CreateDirectory("testing") |> ignore


    tables
    |> Array.iteri
        (fun j ensembleArray ->
            ensembleArray
            |> Array.iteri 
                    (fun k table ->
                    //
                    // Save the output table
                    //
                    let ensembleString = String.concat "" ["testing/"; ensembleStrings.[j]]
                    Table.writeFile(table.GetTable(), sprintf "%s_global_%d.csv" ensembleString k)
                )
        )

    System.Console.ReadLine() |> ignore

    0 // return an integer exit code
