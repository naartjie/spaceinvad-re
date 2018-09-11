open Reprocessing;

type state = {
  time: float,
  speed: float,
  position: (int, int),
};

let spaceshipA = [
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0],
  [3, 0, 0, 1, 0, 0, 0, 1, 0, 0, 3],
  [3, 0, 1, 1, 1, 1, 1, 1, 1, 0, 3],
  [3, 1, 1, 0, 1, 1, 1, 0, 1, 1, 3],
  [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  [2, 3, 1, 1, 1, 1, 1, 1, 1, 3, 2],
  [2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 2],
  [0, 3, 0, 2, 2, 0, 2, 2, 0, 3, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

let spaceshipB = [
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0],
  [0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
  [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0],
  [0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0],
  [0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
  [0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0],
  [0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0],
  [0, 0, 2, 0, 3, 0, 3, 0, 2, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

let spaceshipC = [
  [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
  [0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0],
  [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
  [0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0],
  [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
  [0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0],
  [0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0],
  [0, 0, 1, 0, 3, 3, 0, 1, 0, 0, 0],
  [0, 3, 0, 1, 0, 0, 1, 0, 3, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

let spaceships = [|
  ((1, 0), spaceshipA),
  ((2, 11), spaceshipB),
  ((2, 23), spaceshipC),
|];

let setup = env => {
  Env.size(~width=700, ~height=700, env);
  {time: 0.0, speed: 1.0, position: (0, 0)};
};

let drawBoard = (state, env) => {
  let shipColor = Utils.color(~r=200, ~g=200, ~b=200, ~a=255);
  let pixelSize = 20;
  let gap = 1;

  let {time} = state;

  let step = int_of_float(time *. 10.0) mod 10 > 5;

  for (s in 0 to Array.length(spaceships) - 1) {
    let ((x, y), ship) = spaceships[s];
    for (i in 0 to 14) {
      for (j in 0 to 14) {
        let inBounds =
          i < List.length(List.hd(ship)) && j < List.length(ship);

        if (inBounds) {
          let pixelKinda = List.nth(List.nth(ship, j), i);

          if (pixelKinda == 1
              || pixelKinda == 2
              && step
              || pixelKinda == 3
              && ! step) {
            Draw.fill(shipColor, env);
            Draw.rect(
              ~pos=((i + x) * pixelSize, (j + y) * pixelSize),
              ~width=pixelSize - gap,
              ~height=pixelSize - gap,
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
  {...state, time};
};

let draw = (state, env) => {
  let state = stepTime(state, env);
  let backgroundColor = Utils.color(~r=0, ~g=0, ~b=0, ~a=255);
  Draw.background(backgroundColor, env);
  drawBoard(state, env);
  state;
};

run(~setup, ~draw, ());