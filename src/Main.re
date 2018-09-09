open Reprocessing;

type state = {
  time: float,
  speed: float,
  position: (int, int),
};

let setup = env => {
  Env.size(~width=700, ~height=700, env);
  {time: 0.0, speed: 1.0, position: (0, 0)};
};

let drawBoard = (_state, env) => {
  let shipColor = Utils.color(~r=200, ~g=200, ~b=200, ~a=255);
  let pixelSize = 20;
  let gap = 1;

  let spaceShipRaster = [
    /* -1 */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /*  0 */ [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
    /*  1 */ [0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
    /*  2 */ [0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0],
    /*  3 */ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    /*  4 */ [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    /*  5 */ [1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1],
    /*  6 */ [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
    /*  7 */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /*  8 */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    /*  9 */ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  ];

  for (i in 0 to 14) {
    for (j in 0 to 14) {
      let inBounds =
        i < List.length(List.hd(spaceShipRaster))
        && j < List.length(spaceShipRaster);

      if (inBounds && List.nth(List.nth(spaceShipRaster, j), i) == 1) {
        Draw.fill(shipColor, env);
        Draw.rect(
          ~pos=(i * pixelSize, j * pixelSize),
          ~width=pixelSize - gap,
          ~height=pixelSize - gap,
          env,
        );
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