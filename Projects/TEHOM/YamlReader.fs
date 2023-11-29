namespace Tehom

open FSharp.Configuration
open Entity
open Trait

module YamlReader =

    type WorldData = YamlConfig<"Assets\Entities\default.yaml">

    let createYamlDescription id name description =
        let yamlDescription = new WorldData.entities_Item_Type ()
        yamlDescription.guid <- id
        yamlDescription.name <- name
        yamlDescription.description <- description
        yamlDescription

    let FillModelWithYaml model : Model.Model = 

        let data = WorldData()
        data.Load(__SOURCE_DIRECTORY__ + "/" + Assets.Entities.EntitiesDefault)

        let folder oldMap (entity: WorldData.entities_Item_Type) = 
            Map.add (ID entity.guid) {
                ObjectName = String entity.name
                Description = String entity.description;
            } oldMap

        { model with Description = Seq.fold folder model.Description data.entities }

    let ReadDefaultData () =

        let data = WorldData()
     
        data.Load(__SOURCE_DIRECTORY__ + "/" + Assets.Entities.EntitiesDefault)

 //       data.Save(__SOURCE_DIRECTORY__ + @"\Assets\World\defaultChanged.yaml")