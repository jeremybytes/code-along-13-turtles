#load "Common.fsx"
#load "FPTurtleLib.fsx"

open Common
open FPTurtleLib

type EventStore() =
    // private mutable data
    let eventDict = System.Collections.Generic.Dictionary<System.Guid,obj list>()

    let saveEvent = new Event<System.Guid * obj>()

    // Triggered when something is saved
    member this.SaveEvent =
        saveEvent.Publish

    // save an event to storage
    member this.Save(eventId,event) =
        match eventDict.TryGetValue eventId with
        | true,eventList ->
            let newList = event :: eventList // store newest event in front
            eventDict.[eventId] <- newList
        | false,_ ->
            let newList = [event]
            eventDict.[eventId] <- newList
        saveEvent.Trigger(eventId,event)

    // get all events associated with the specified eventId
    member this.Get<'a>(eventId) =
        match eventDict.TryGetValue eventId with
        | true,eventList ->
            eventList
            |> Seq.cast<'a> |> Seq.toList
            |> List.rev // reverse so that oldest events are first
        | false,_ ->
            []

    // clear all events associated with the specified eventId
    member this.Clear(eventId) =
        eventDict.[eventId] <- []

type TurtleId = System.Guid

type TurtleCommandAction =
    | Move of Distance
    | Turn of Angle
    | PenUp
    | PenDown
    | SetColor of PenColor

type TurtleCommand = {
    turtleId : TurtleId
    action : TurtleCommandAction
    }

type StateChangedEvent =
    | Moved of Distance
    | Turned of Angle
    | PenWentUp
    | PenWentDown
    | ColorChanged of PenColor

type MovedEvent = {
    startPos : Position
    endPos : Position
    penColor : PenColor option
    }

type TurtleEvent =
    | StateChangedEvent of StateChangedEvent
    | MovedEvent of MovedEvent

module CommandHandler =

    // apply an event to the current state and return the new state of the turtle
    let applyEvent log oldState event =
        match event with 
        | Moved distance ->
            Turtle.move log distance oldState
        | Turned angle ->
            Turtle.turn log angle oldState
        | PenWentUp ->
            Turtle.penUp log oldState
        | PenWentDown ->
            Turtle.penDown log oldState
        | ColorChanged color ->
            Turtle.setColor log color oldState

    // determine what events to generate, based on the command and the state
    let eventsFromCommand log command stateBeforeCommand =
        // create the StateChangedEvent from the TurtleCommand
        let stateChangedEvent =
            match command.action with
            | Move dist -> Moved dist
            | Turn angle -> Turned angle
            | PenUp -> PenWentUp
            | PenDown -> PenWentDown
            | SetColor color -> ColorChanged color

        // calculate the current state from the new event
        let stateAfterCommand =
            applyEvent log stateBeforeCommand stateChangedEvent

        // create the MovedEvent
        let startPos = stateBeforeCommand.position
        let endPos = stateAfterCommand.position
        let penColor = 
            if stateBeforeCommand.penState = Down then
                Some stateBeforeCommand.color
            else
                None

        let movedEvent = {
            startPos = startPos
            endPos = endPos
            penColor = penColor
            }

        // return the list of events
        if startPos <> endPos then
            [ StateChangedEvent stateChangedEvent; MovedEvent movedEvent]
        else
            [StateChangedEvent stateChangedEvent]

    // The type representing a function that gets the StateChangedEvents for a turtle id
    type GetStateChangeEventsForId =
        TurtleId -> StateChangedEvent list

    // The type representing a function that saves a TurtleEvent
    type SaveTurtleEvent =
        TurtleId -> TurtleEvent -> unit

    // main function : process a command
    let commandHandler
        (log:string -> unit)
        (getEvents:GetStateChangeEventsForId)
        (saveEvent:SaveTurtleEvent)
        (command:TurtleCommand) =

        // first load all the events from the event store
        let eventHistory =
            getEvents command.turtleId

        // then recreate the state before the command
        let stateBeforeCommand =
            let nolog = ignore
            eventHistory
            |> List.fold (applyEvent nolog) Turtle.initialTurtleState

        // construct the events from the command and the stateBeforeCommand
        // use the supplied logger for this bit
        let events = eventsFromCommand log command stateBeforeCommand

        // store the events in the event store
        events |> List.iter (saveEvent command.turtleId)


module CommandHandlerClient =
    open CommandHandler

    // filter to choose only StateChangeEvent from TurtleEvents
    let stateChangedEventFilter = function
        | StateChangedEvent ev -> Some ev
        | _ -> None

    // create a command handler
    let makeCommandHandler() =
        let logger = printfn "%s"
        let eventStore = EventStore()
        let getStateChangedEvents id =
            eventStore.Get<TurtleEvent>(id)
            |> List.choose stateChangedEventFilter
        let saveEvent id ev =
            eventStore.Save(id,ev)
        commandHandler logger getStateChangedEvents saveEvent

    // command versions of standard actions
    let turtleId = System.Guid.NewGuid()
    let move dist = {turtleId=turtleId; action = Move dist}
    let turn angle = {turtleId=turtleId; action = Turn angle}
    let penDown = {turtleId=turtleId; action = PenDown}
    let penUp = {turtleId=turtleId; action = PenUp}
    let setColor color = {turtleId=turtleId; action = SetColor color}

    let drawTriangle() =
        let handler = makeCommandHandler()
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)
        handler (move 100.0)
        handler (turn 120.0<Degrees>)

    let drawPolygon n =
        let angle = 180.0 - (360.0/float n)
        let angleDegrees = angle * 1.0<Degrees>
        let handler = makeCommandHandler()

        let drawOneSide sideNumber =
            handler (move 100.0)
            handler (turn angleDegrees)

        for i in [1..n] do
            drawOneSide i

CommandHandlerClient.drawTriangle()
CommandHandlerClient.drawPolygon 4