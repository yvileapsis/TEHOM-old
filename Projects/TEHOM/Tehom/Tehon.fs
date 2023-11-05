[<AutoOpen>]
module Tehon

open Logic

//
// --------- Model ---------
//

type ObjectID =
    | ID of string

type ObjectVolume = 
    | Volume of int64
    | Limitless
    | SameAsChildren
    | NotApplicable

type Generatable<'a> = 
    | Type of 'a
    | NotGenerated
    | SameAsParent
    | NotApplicable


type Relation = 
    | PartOf of ObjectID // parent
    | LooselyIn of ObjectID // parent
    | ConnectsTo of ObjectID
    | Child of ObjectID

    static member getID = function
        | PartOf id -> id
        | LooselyIn id -> id
        | Child id -> id
        | ConnectsTo id -> id


type ObjectCause = {
    Material:       Generatable<string>
    Formal:         Generatable<string>
    Effecient:      Generatable<string>
    Final:          Generatable<string>
}

type ObjectDescription = {
    Name:           Generatable<string>
    Description:    Generatable<string>
    Details:        Generatable<ObjectCause>
}

type ObjectProperty = {
    VolumeInternal: ObjectVolume
    VolumeExternal: ObjectVolume
}

type World = {
    Descriptions:   Map<ObjectID, ObjectDescription>
    Properties:     Map<ObjectID, ObjectProperty>
    Relations:      Map<Relation, Set<ObjectID>>
}

type Label = Label of string

type Error = Label list * string

module Effects = 

    type Effect<'a, 'b> = { 
        Effect : ('a -> Result<'b, Error>)
        Label: Label
    }

    let run effect inputWorld =
        effect.Effect inputWorld

    let doNothing =
        let label = Label "doNothing"
        let inner world = 
            Success world 
        { Effect = inner; Label = label }
        
    let setLabel effect newLabel =
        let newInnerFn input =
            match effect.Effect input with
            | Success s -> Success s
            | Failure (list, error) -> Failure ([newLabel] @ list, error)

        { Effect = newInnerFn; Label = newLabel }  

    let ( <?> ) = setLabel

    let getLabel effect =
        effect.Label
    let switch processFunc input =
        Success (processFunc input)

    let bindP f p =
        let label = Label "bind"
        let innerFn input =
            match p input with
            | Success s -> run f s
            | Failure f -> Failure f
        { Effect = innerFn; Label = label}

    //let ( >>= ) p f = bindP f p

    let andThen e1 e2 =
        let label = Label <| sprintf "%A andThen %A" (getLabel e1) (getLabel e2)
        let inner world =
            match run e1 world with
            | Failure msg -> Failure msg
            | Success (newWorld) ->
                match run e2 newWorld with
                | Failure msg -> Failure msg
                | Success remaining2 -> Success remaining2
        { Effect = inner; Label = label }
    let ( .>>. ) = andThen

    let foldBack effect set =
        let folder foldEffect on =
            foldEffect .>>. effect on 
            <?> (Label <| sprintf "foldBack %A" (effect on))
        set |> Set.fold folder doNothing
        
    let addToMapOfSets key set =
        let addToSet = function
            | None -> Some set
            | Some oldSet -> Some (Set.union set oldSet)
        Map.change key addToSet

    let removeFromMapOfSets key set =
        let newSet oldSet =
            let setDiff = Set.difference oldSet set
            match Set.isEmpty setDiff with 
            | true -> None
            | false -> Some setDiff

        let removeFromSet = function
            | None -> None
            | Some oldSet -> newSet oldSet

        Map.change key removeFromSet

    let addRelation child parent = 

        let label = Label "add TehomRelation"

        let addRelation = addToMapOfSets parent (Set.empty.Add child)

        let result (world: World) = Success { world with Relations = world.Relations |> addRelation  }
        
        { Effect = result; Label = label } 

    let removeRelation child parent =
        let label = Label "remove TehomRelation"
            
        let removeRelation = removeFromMapOfSets parent (Set.empty.Add child)

        let result (world: World) = 
            match world.Relations |> Map.containsKey parent with
            | true -> Success { world with Relations = world.Relations |> removeRelation }
            | false -> Failure ([label], sprintf "%A not found in %A" child parent)
        
        { Effect = result; Label = label }


    let addRelationBilateral child parent =
        addRelation (Relation.getID parent) child
        .>>. addRelation (Relation.getID child) parent 
        <?> Label "addTwoSideRelation"

    let removeTwoSideRelation child parent = 
        removeRelation (Relation.getID parent) child
        .>>. removeRelation (Relation.getID child) parent
        <?> Label "removeTwoSideRelation"
        
    let moveTwoSideRelation child parentOld parentNew = 
        removeTwoSideRelation child parentOld
        .>>. addRelationBilateral child parentNew
        <?> Label "moveTwoSideRelation"

    let addObjectDescription description object =

        let label = Label "add TehomObject"

        let addDescription = Map.change object (function Some _ -> Some description | None -> Some description)

        let result world = Success { world with Descriptions = world.Descriptions |> addDescription }

        { Effect = result; Label = label }

    let removeObjectDescription objectID =
        let label = Label "remove TehomObject"
        let result world = 
            let newWorld = { world with Descriptions = world.Descriptions |> Map.remove objectID }
            match world.Descriptions |> Map.containsKey objectID with
            | true -> Success newWorld
            | false -> Failure ([label], sprintf "%A not found in world" objectID)
        { Effect = result; Label = label }


    let getStringFromGenerated = function
    | Type t -> t
    | NotGenerated -> "TODO: generate"
    | SameAsParent -> "TODO: get parent"
    | NotApplicable -> ""

    let getParent id = 
        let label = Label "Get Parent"
        let result world =
            match Map.containsKey (Child id) world.Relations with
            | true -> Success (Set.maxElement world.Relations[Child id])
            | false -> Failure ([label], sprintf "parent not found for %A" id)
        { Effect = result; Label = label }

    let describeDetails id =
        let label = Label "Describe"
        let result world =
            let getDetails description =
                let name = getStringFromGenerated description.Name
                let description = getStringFromGenerated description.Description

                sprintf "%s\n\n%s" name description

            match Map.containsKey id world.Descriptions with
                | true -> Success <| getDetails world.Descriptions[id]
                | false -> Failure ([Label "Test"], "no object with that id")

        { Effect = result; Label = label }


type ObjectToRegister = Object of ObjectID * Set<Relation> * ObjectDescription option * ObjectProperty option

module Actions = 
    open Effects

    let gameRegisterObject = function
    | Object (objectID, relations, description, property ) ->
        let description = 
            match description with
            | Some description -> addObjectDescription description objectID 
            | None -> doNothing
    
        let handleRelation = function
            | PartOf id -> addRelationBilateral (Child objectID) (PartOf id)
            | LooselyIn id -> addRelationBilateral (Child objectID) (LooselyIn id)
            | relation -> addRelation objectID relation

        run (description .>>. foldBack handleRelation relations)

    let gameMoveObject objectID parentOld parentNew =
        run (moveTwoSideRelation (Child objectID) (LooselyIn parentOld) (LooselyIn parentNew))

    let describeParent id = 
        let inner1 world =
            let parent = getParent id
            
            match run parent world with 
            | Success id -> id
            | Failure _ -> ID ""

        let inner2 world =
            run (describeDetails (inner1 world)) world

        inner2



let gameWorld = {
    Descriptions = Map<ObjectID, ObjectDescription> []
    Properties = Map<ObjectID, ObjectProperty> []
    Relations = Map<Relation, Set<ObjectID>> []
} 

let objectAbyss: ObjectToRegister =
    Object (ID "abyss",
    Set.empty<Relation>,
    Some {
        Name = Type "Tehon"
        Description = NotGenerated

        Details = Type {
            Material = Type "broken hopes, forgotten memories and secret histories"
            Formal = Type "shapeless and bottomless, titanic in scope"
            Effecient = Type "self-caused"
            Final = Type "ungraspable by even the eldritch mind, much less mortal"
        }
    }, Some {
        VolumeInternal = Limitless
        VolumeExternal = Limitless
    })

let worldSurface = 
    Object (ID "Lvl0Town",
    Set.empty.Add (PartOf (ID "abyss")),
    Some {
        Name = NotGenerated
        Description = Type "Some town on the edge of the abyss, home to weary travellers and abyss divers"
        Details = NotGenerated
    }, None)

let key =
    Object (ID "objectKey",
    Set.ofList [LooselyIn (ID "west1")],
    Some {
        Name = Type "A shiny key"
        Description = Type "This key looks like it could open a nearby door."
        Details = NotGenerated
    }, None)

let worldHouse = 
    Object (ID "testhouse",
    Set.ofList [PartOf (ID "Lvl0Town")],
    Some {
        Name = Type "The test house"
        Description = Type "Test house."
        Details = NotGenerated
    }, None)


let allRooms = [
    
    Object (ID "center",
    Set.ofList [PartOf (ID "testhouse")],
    Some {
        Name = Type "A central room"
        Description = Type "You are standing in a central room with exits in all directions. A single brazier lights the room."
        Details = NotGenerated
    }, None)

    Object (ID "center-north1",
    Set.ofList [PartOf (ID "testhouse"); ConnectsTo (ID "north")],
    Some {
        Name = Type "A darkened passageway"
        Description = Type "You see a darkened passageway to the north."
        Details = NotGenerated
    }, None)

    Object (ID "center-south1",
    Set.ofList [PartOf (ID "testhouse")],
    Some {
        Name = Type "A door"
        Description = Type "You see door to the south. A waft of cold air hits your face."
        Details = NotGenerated
    }, None)

    Object (ID "center-east1",
    Set.ofList [PartOf (ID "testhouse")],
    Some {
        Name = Type "A locked door"
        Description = Type "You see a locked door to the east."
        Details = NotGenerated
    }, None)

    Object (ID "center-west1",
    Set.ofList [PartOf (ID "testhouse")],
    Some {
        Name = Type "A passageway to an interesting room"
        Description = Type "You see an interesting room to the west."
        Details = NotGenerated
    }, None)

    Object (ID "north1",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "A dark room"
        Description = Type "You are standing in a very dark room. You hear the faint sound of rats scurrying along the floor."
        Details = NotGenerated
    }, None)

    Object (ID "south1",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "A cold room"
        Description = Type "You are standing in a room that feels very cold.  Your breath instantly turns into a white puff."
        Details = NotGenerated
    }, None)

    Object (ID "south1-center",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "North exit"
        Description = Type "You see an exit to the north. That room looks much warmer."
        Details = NotGenerated
    }, None)


    Object (ID "west1",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "A cozy room"
        Description = Type "This room seems very cozy, as if someone had made a home here.  Various personal belongings are strewn about."
        Details = NotGenerated
    }, None)

    Object (ID "west1-center",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "A door to the lit room"
        Description = Type "You see a doorway back to the lit room."
        Details = NotGenerated
    }, None)

    Object (ID "east1",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "An open meadow"
        Description = Type "You are in an open meadow. The sun is bright and it takes some time for your eyes to adjust."
        Details = NotGenerated
    }, None)

    Object (ID "east1-center",
    Set.ofList [PartOf (ID "testhouse")],
    Some { 
        Name = Type "A stone doorway"
        Description = Type "You see stone doorway to the west. Why would you want to go back there?"
        Details = NotGenerated
    }, None)
]

let startingLocation = ID "center"
let player =
    Object (ID "player",
    Set.ofList [LooselyIn startingLocation],
    Some {
        Name = Type "Yvile"
        Description = Type "Just your average adventurer."
        Details = NotGenerated
    }, None)
