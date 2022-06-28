

type TurtleCommand =
    | Move of int
    | Turn of int
    | Sleep of int

type TurtleProgram<'a> =
    | Stop of 'a
    | Move of int * (unit -> TurtleProgram<'a>)
    | Turn of int * (unit -> TurtleProgram<'a>)
    | Sleep of int * (unit -> TurtleProgram<'a>)

let returnT x =
    Stop x

let rec bindT f inst =
    match inst with
    | Stop x -> f x
    | Move (dist, next) -> Move(dist, next >> bindT f)
    | Turn (angle, next) -> Turn(angle, next >> bindT f)
    | Sleep (time, next) -> Sleep(time, next >> bindT f)

type TurtleProgramBuilder() =
    member this.Return(x) = returnT x
    member this.Bind(x, f) = bindT f x
    member this.Zero(x) = returnT ()

let turtleProgram = TurtleProgramBuilder()


