param($installPath, $toolsPath, $package, $project)

# set 'Copy To Output Directory' to 'Copy if newer'
$file1 = $project.ProjectItems.Item("EcosystemModelInitialisation_Output.csv")
$copyToOutput1 = $file1.Properties.Item("CopyToOutputDirectory")
$copyToOutput1.Value = 2

$file2 = $project.ProjectItems.Item("MassBinDefinitions.csv")
$copyToOutput2 = $file2.Properties.Item("CopyToOutputDirectory")
$copyToOutput2.Value = 2

$file2 = $project.ProjectItems.Item("NetCDF.Interop.dll")
$copyToOutput2 = $file2.Properties.Item("CopyToOutputDirectory")
$copyToOutput2.Value = 2

$file2 = $project.ProjectItems.Item("SDSArrays.dll")
$copyToOutput2 = $file2.Properties.Item("CopyToOutputDirectory")
$copyToOutput2.Value = 2
