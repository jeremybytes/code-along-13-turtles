#load "Common.fsx"
#load "OOTurtleLib.fsx"
#load "FPTurtleLib.fsx"
#load "TurtleApiHelpers.fsx"

open Common

type TurtleState = FPTurtleLib.Turtle.TurtleState

type TurtleFunctions = {
    move : Distance -> TurtleState -> TurtleState
    turn : Angle -> TurtleState -> TurtleState
    penUp: TurtleState -> TurtleState
    penDown: TurtleState -> TurtleState
    setColor: PenColor -> TurtleState -> TurtleState
    }

module TurtleApiLayer_FP =
    
    open Result
    open TurtleApiHelpers
    open FPTurtleLib

    type TurtleApi(turtleFunctions: TurtleFunctions) =
        let mutable state = Turtle.initialTurtleState

        let updateState newState =
            state <- newState

        member this.Exec (commandStr:string) =
            let tokens = commandStr.Split(' ') |> List.ofArray |> List.map trimString

            match tokens with
            | [ "Move"; distanceStr ] -> result {
                let! distance = validateDistance distanceStr
                let newState = turtleFunctions.move distance state
                updateState newState
                }
            
            | [ "Turn"; angleStr ] -> result {
                let! angle = validateAngle angleStr
                let newState = turtleFunctions.turn angle state
                updateState newState
                }

            | [ "PenUp" ] -> result {
                let newState = turtleFunctions.penUp state
                updateState newState
                }

            | [ "PenDown" ] -> result {
                let newState = turtleFunctions.penDown state
                updateState newState
                }

            | [ "SetColor"; colorStr ] -> result {
                let! color = validateColor colorStr
                let newState = turtleFunctions.setColor color state
                updateState newState
                }
            | _ ->
                Error (InvalidCommand commandStr)

module TurtleImplementation_FP =
    open FPTurtleLib

    let normalSize() =
        let log = printfn "%s"
        {
            move = Turtle.move log
            turn = Turtle.turn log
            penUp = Turtle.penUp log
            penDown = Turtle.penDown log
            setColor = Turtle.setColor log
        }

    let halfSize() =
        let normalSize = normalSize()

        { normalSize with
            move = fun dist -> normalSize.move (dist/2.0)
        }

module TurtleApiClient_FP =
    open TurtleApiLayer_FP
    open Result

    let drawTriangle(api:TurtleApi) =
        result {
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
            do! api.Exec "Move 100"
            do! api.Exec "Turn 120"
        } |> ignore

do
    let turtleFns = TurtleImplementation_FP.normalSize()
    let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
    TurtleApiClient_FP.drawTriangle(api)

do
    let turtleFns = TurtleImplementation_FP.halfSize()
    let api = TurtleApiLayer_FP.TurtleApi(turtleFns)
    TurtleApiClient_FP.drawTriangle(api)
