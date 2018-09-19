open Reprocessing;

type raster = list(list(int));
type position = (float, float);
type drawable = (position, raster);

type direction =
  | Left
  | Right
  | None;

type state = {
  time: float,
  timeDelta: float,
  spaceshipsDirection: direction,
  /* actions */
  shooting: bool,
  shooterDirection: direction,
  /* drawables */
  shooter: drawable,
  bullets: list(drawable),
  spaceships: list(drawable),
};

let screenWidth = 600;
let screenHeight = 350;
let pixelSize = 6.0;

let spaceshipA = [
  [0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
  [3, 0, 0, 1, 0, 0, 0, 1, 0, 0, 3],
  [3, 0, 1, 1, 1, 1, 1, 1, 1, 0, 3],
  [3, 1, 1, 0, 1, 1, 1, 0, 1, 1, 3],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [2, 3, 1, 1, 1, 1, 1, 1, 1, 3, 2],
  [2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 2],
  [0, 3, 0, 2, 2, 0, 2, 2, 0, 3, 0],
];

let spaceshipB = [
  [0, 0, 0, 1, 1, 0, 0, 0],
  [0, 0, 1, 1, 1, 1, 0, 0],
  [0, 1, 1, 1, 1, 1, 1, 0],
  [1, 1, 0, 1, 1, 0, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1],
  [0, 3, 2, 3, 3, 2, 3, 0],
  [3, 2, 0, 2, 2, 0, 2, 3],
  [2, 3, 2, 0, 0, 2, 3, 2],
];

let spaceshipC = [
  [0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0],
  [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [0, 0, 3, 1, 1, 0, 0, 1, 1, 3, 0, 0],
  [0, 3, 1, 2, 3, 1, 1, 3, 2, 1, 3, 0],
  [2, 2, 0, 3, 3, 0, 0, 3, 3, 0, 2, 2],
];

let spaceshipsInitial = [
  ((1.0, 1.0), spaceshipC),
  ((15.0, 1.0), spaceshipC),
  ((29.0, 1.0), spaceshipC),
  ((3.0, 13.0), spaceshipB),
  ((17.0, 13.0), spaceshipB),
  ((31.0, 13.0), spaceshipB),
  ((1.0, 25.0), spaceshipA),
  ((16.0, 25.0), spaceshipA),
  ((30.0, 25.0), spaceshipA),
];

let shooterRaster = [
  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
  [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
];

let bulletRaster = [[1], [1], [1]];

let heightOfRaster = r => r |> List.length |> float_of_int;
let widthOfRaster = r => r |> List.hd |> List.length |> float_of_int;

let heightOfShooter = heightOfRaster(shooterRaster);
let widthOfShooter = widthOfRaster(shooterRaster);

let spaceshipSpeed = 10.0;
let bulletSpeed = 60.0;
let shooterSpeed = 40.0;

let initialState = {
  time: 0.0,
  timeDelta: 1.0,
  spaceships: spaceshipsInitial,
  spaceshipsDirection: Right,
  shooter: ((45.0, 50.0), shooterRaster),
  bullets: [],
  shooting: false,
  shooterDirection: None,
};

let setup = env => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  initialState;
};

let stepTime = (state, env) => {
  let delta = Env.deltaTime(env);
  let time = state.time +. delta;
  {...state, time, timeDelta: delta};
};

let recalcPositions = (state, _env) => {
  let spaceshipsAtEdge =
    state.spaceships
    |> List.fold_left(
         (acc, ((x, _), raster)) => {
           let atRightEdge =
             x
             +. widthOfRaster(raster) > float_of_int(screenWidth)
             /. pixelSize;

           let atLeftEdge = x <= 1.0;

           switch (acc, atLeftEdge, atRightEdge) {
           | (Left, _, _) => Left
           | (Right, _, _) => Right
           | (_, true, _) => Left
           | (_, _, true) => Right
           | _ => None
           };
         },
         None,
       );

  let spaceshipsDirection =
    switch (spaceshipsAtEdge) {
    | Left => Right
    | Right => Left
    | _ => state.spaceshipsDirection
    };

  let spaceshipDelta =
    state.timeDelta
    *. spaceshipSpeed
    *. (spaceshipsDirection === Left ? (-1.0) : 1.0);
  let spaceships =
    state.spaceships
    |> List.map((((x, y), raster)) => ((x +. spaceshipDelta, y), raster));

  let bulletsDelta = state.timeDelta *. (-. bulletSpeed);
  let bullets =
    state.bullets
    |> List.map((((x, y), raster)) => ((x, y +. bulletsDelta), raster));

  let ((x, y), raster) = state.shooter;
  let shooterDelta = state.timeDelta *. shooterSpeed;
  let atLeftEdge = x <= 1.0;
  let atRightEdge =
    x +. widthOfShooter >= float_of_int(screenWidth) /. pixelSize;
  let shooterPos =
    switch (state.shooterDirection, atLeftEdge, atRightEdge) {
    | (Left, false, _) => (x -. shooterDelta, y)
    | (Right, _, false) => (x +. shooterDelta, y)
    | _ => (x, y)
    };

  {
    ...state,
    spaceships,
    spaceshipsDirection,
    bullets,
    shooter: (shooterPos, raster),
  };
};

let shootBullet = (state, _env) => {
  let {shooting, shooter, bullets} = state;
  let bullets =
    if (shooting) {
      let ((x, y), _) = shooter;
      let x = x +. widthOfShooter /. 2.0;
      [((x, y), bulletRaster), ...bullets];
    } else {
      bullets;
    };

  {...state, shooting: false, bullets};
};

let drawBoard = (state, env) => {
  let shipColor = Utils.color(~r=200, ~g=200, ~b=0, ~a=255);

  let {time} = state;

  let step = int_of_float(time *. 10.0) mod 10 >= 5;
  let drawables = [
    state.shooter,
    ...List.append(state.bullets, state.spaceships),
  ];

  drawables
  |> List.map((((x, y), raster)) =>
       for (i in 0 to 15) {
         for (j in 0 to 15) {
           let inBounds =
             i < List.length(List.hd(raster)) && j < List.length(raster);

           if (inBounds) {
             let rasterPixel = List.nth(List.nth(raster, j), i);
             let i = float_of_int(i);
             let j = float_of_int(j);

             if (rasterPixel == 1
                 || rasterPixel == 2
                 && step
                 || rasterPixel == 3
                 && ! step) {
               Draw.fill(shipColor, env);
               Draw.ellipsef(
                 ~center=((i +. x) *. pixelSize, (j +. y) *. pixelSize),
                 ~radx=pixelSize /. 3.4,
                 ~rady=pixelSize /. 3.4,
                 env,
               );
             };
           };
         };
       }
     )
  |> ignore;

  state;
};

let draw = (state, env) => {
  let backgroundColor = Utils.color(~r=24, ~g=24, ~b=24, ~a=255);
  Draw.background(backgroundColor, env);

  let (>>>) = (state, fn) => fn(state, env);
  state >>> stepTime >>> recalcPositions >>> shootBullet >>> drawBoard;
};

let keyPressed = (state, env) => {
  let leftIsDown = Env.key(Left, env);
  let rightIsDown = Env.key(Right, env);
  let thisKey = Env.keyCode(env);

  switch (thisKey, leftIsDown, rightIsDown) {
  | (Space, _, _) => {...state, shooting: true}
  | (Left, _, false) => {...state, shooterDirection: Left}
  | (Right, false, _) => {...state, shooterDirection: Right}
  | _ => {...state, shooterDirection: None}
  };
};

let keyReleased = (state, env) => {
  let leftIsDown = Env.key(Left, env);
  let rightIsDown = Env.key(Right, env);

  switch (leftIsDown, rightIsDown) {
  | (true, false) => {...state, shooterDirection: Left}
  | (false, true) => {...state, shooterDirection: Right}
  | _ => {...state, shooterDirection: None}
  };
};

run(~setup, ~draw, ~keyPressed, ~keyReleased, ());