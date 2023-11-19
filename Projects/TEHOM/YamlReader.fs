namespace Tehom

open FSharp.Configuration

module YamlReader =

    type WorldData = YamlConfig<"Assets\World\default.yaml">

    let createYamlDescription id name description =
        let yamlDescription = new WorldData.descriptions_Item_Type ()
        yamlDescription.guid <- id
        yamlDescription.name <- name
        yamlDescription.description <- description
        yamlDescription

    let ReadDefaultData () =

        let data = WorldData()
     
        data.Load(__SOURCE_DIRECTORY__ + @"\Assets\World\default.yaml")

        data.descriptions.Add(createYamlDescription
            "key"
            "A shiny key"
            "A key that shines and opens doors."
        )

        data.Save(__SOURCE_DIRECTORY__ + @"\Assets\World\defaultChanged.yaml")