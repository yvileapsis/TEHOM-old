module Logic

//
// --------- Logic --------- 
//

type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

let (>>=) x f =
    bind f x

let foldBackBind processFunc set =
    let inner world =
        let folder result object =
            result >>= processFunc object

        set |> List.fold folder (Success world)
    inner

let switch processFunc input =
    Success (processFunc input)
