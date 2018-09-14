open Reprocessing;

type raster = list(list(int));
type position = (float, float);
type spaceship = (position, raster);

type state = {
  time: float,
  timeDelta: float,
  speed: float,
  speedV: float,
  spaceships: array(spaceship),
  shooter: spaceship,
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

let spaceshipsInitial = [|
  ((1.0, 1.0), spaceshipC),
  ((15.0, 1.0), spaceshipC),
  ((29.0, 1.0), spaceshipC),
  ((3.0, 13.0), spaceshipB),
  ((17.0, 13.0), spaceshipB),
  ((31.0, 13.0), spaceshipB),
  ((1.0, 25.0), spaceshipA),
  ((16.0, 25.0), spaceshipA),
  ((30.0, 25.0), spaceshipA),
|];

let shooterRaster = [
  [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
  [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
];

let heightOfShooter = shooterRaster |> List.length |> float_of_int;
let widthOfShooter = shooterRaster |> List.hd |> List.length |> float_of_int;

let initialState = {
  time: 0.0,
  speed: 10.0,
  speedV: 0.0,
  timeDelta: 1.0,
  spaceships: spaceshipsInitial,
  shooter: ((45.0, 50.0), shooterRaster),
};

let setup = env => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  initialState;
};

let drawBoard = (state, env) => {
  let shipColor = Utils.color(~r=200, ~g=200, ~b=200, ~a=255);

  let {time} = state;
  let step = int_of_float(time *. 10.0) mod 10 >= 5;
  let len = Array.length(state.spaceships);
  for (s in 0 to len) {
    let ((x, y), ship) = s == len ? state.shooter : state.spaceships[s];
    for (i in 0 to 20) {
      for (j in 0 to 20) {
        let inBounds =
          i < List.length(List.hd(ship)) && j < List.length(ship);

        if (inBounds) {
          let pixelKinda = List.nth(List.nth(ship, j), i);
          let i = float_of_int(i);
          let j = float_of_int(j);

          if (pixelKinda == 1
              || pixelKinda == 2
              && step
              || pixelKinda == 3
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
    };
  };
};

let stepTime = (state, env) => {
  let delta = Env.deltaTime(env);
  let time = state.time +. delta;
  {...state, time, timeDelta: delta};
};

let calculatePositions = (state, _env) => {
  let posXDelta = state.timeDelta *. state.speed;
  let posYDelta = state.timeDelta *. state.speedV;
  let spaceships =
    state.spaceships
    |> Array.map((((x, y), raster)) =>
         ((x +. posXDelta, y +. posYDelta), raster)
       );
  {...state, spaceships};
};

let checkBounds = (state, _env) => {
  let width = raster => raster |> List.hd |> List.length;

  let hitBound =
    state.spaceships
    |> Array.fold_left(
         (acc, ((x, _), raster)) => {
           let isWider =
             int_of_float(x)
             + width(raster) > screenWidth
             / int_of_float(pixelSize);
           acc || isWider || x <= 0.0;
         },
         false,
       );

  let speed = hitBound ? state.speed *. (-1.0) : state.speed;
  {...state, speed};
};

let keyTyped = (state, env) => {
  let {shooter: ((x, y), raster)} = state;

  switch (Env.keyCode(env)) {
  | Left => {...state, shooter: ((x -. 1.0, y), raster)}
  | Right => {...state, shooter: ((x +. 1.0, y), raster)}
  | _ => state
  };
};

let draw = (state, env) => {
  let state = stepTime(state, env);
  let state = calculatePositions(state, env);
  let state = checkBounds(state, env);

  let backgroundColor = Utils.color(~r=24, ~g=24, ~b=24, ~a=255);
  Draw.background(backgroundColor, env);

  drawBoard(state, env);
  state;
};

run(~setup, ~draw, ~keyTyped, ());