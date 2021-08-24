#load "Common.fsx"
#load "OOTurtleLib.fsx"

open Common

module TurtleApiLayer =
    open OOTurtleLib

    exception TurtleApiException of string

    let log message =
        printfn "%s" message

    // convert the distance parameter to a float, or throw an exception
    let validateDistance distanceStr =
        try
            float distanceStr
        with
        | ex ->
            let msg = sprintf "Invalid distance '%s' [%s]" distanceStr ex.Message
            raise (TurtleApiException msg)

    // convert the angle parameter to float<Degrees>, or throw an exception
    let validateAngle angleStr =
        try
            (float angleStr) * 1.0<Degrees>
        with
        | ex ->
            let msg = sprintf "Invalid angle '%s' [%s]" angleStr ex.Message
            raise (TurtleApiException msg)

    // convert the color parameter to PenColor, or throw an exception
    let validateColor colorStr =
        match colorStr with
        | "Black" -> Black
        | "Blue" -> Blue
        | "Red" -> Red
        | _ ->
            let msg = sprintf "Color '%s' is not recognized" colorStr
            raise (TurtleApiException msg)

    type TurtleApi() =

        let turtle = Turtle(log)

        member this.Exec (commandStr:string) =
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString
            match tokens with
            | [ "Move"; distanceStr ] ->
                let distance = validateDistance distanceStr
                turtle.Move distance
            | [ "Turn"; angleStr ] ->
                let angle = validateAngle angleStr
                turtle.Turn angle
            | [ "Pen"; "Up" ] ->
                turtle.PenUp()
            | [ "Pen"; "Down" ] ->
                turtle.PenDown()
            | [ "SetColor"; colorStr ] ->
                let color = validateColor colorStr
                turtle.SetColor color
            | _ ->
                let msg = sprintf "Instruction '%s' is not recognized" commandStr
                raise (TurtleApiException msg)

module TurtleApiClient =
    open TurtleApiLayer

    let drawTriangle() =
        let api = TurtleApi()
        api.Exec "Move 100"
        api.Exec "Turn 120"
        api.Exec "Move 100"
        api.Exec "Turn 120"
        api.Exec "Move 100"
        api.Exec "Turn 120"

    let drawPolygon n = 
        let angle = 180.0 - (360.0/float n)
        let api = TurtleApi()

        // define a function that draws one side
        let drawOneSide() =
            api.Exec "Move 100.0"
            api.Exec (sprintf "Turn %f" angle)

        // repeat for all sides
        for i in [1..n] do
            drawOneSide()

    let triggerError() =
        let api = TurtleApi()
        api.Exec "Move bad"

//TurtleApiClient.triggerError() |> ignore
TurtleApiClient.drawPolygon 5 |> ignore
TurtleApiClient.drawTriangle() |> ignore
