let get_some_exn =
  fun
  | Some x => x
  | None => failwith "can't get optional";

let canvas = get_some_exn (HtmlDom.getElementById HtmlDom.document "canvas");

[%%bs.raw
  {|
function backingScale() {
  if ('devicePixelRatio' in window) {
    if (window.devicePixelRatio > 1) {
      return window.devicePixelRatio;
    }
  }
  return 1;
};

var ratio = backingScale();
var can = document.getElementById("canvas");

if (ratio > 1) {
    can.style.width = can.width + "px";
    can.style.height = can.height + "px";
    can.width = can.width * ratio;
    can.height = can.height * ratio;
  }
|}
];

let ctx = (HtmlDom.canvasElementToJsObj (HtmlDom.elementToCanvasElement canvas))##getContext "2d";

let drawRealPixel (x, y) color => {
  ignore @@ ctx##fillStyle#=color;
  ignore @@ ctx##fillRect x y 1 1
};

let translate (x, y) => (x * 4, y * 4);

let red = "rgb(255, 0, 0)";

let green = "rgb(0, 255, 0)";

let blue = "rgb(0, 0, 255)";

let drawPixel (x, y) => {
  let (x, y) = translate (x, y);
  drawRealPixel (x, y) red;
  drawRealPixel (x, y + 1) red;
  drawRealPixel (x, y + 2) red;
  drawRealPixel (x + 1, y) green;
  drawRealPixel (x + 1, y + 1) green;
  drawRealPixel (x + 1, y + 2) green;
  drawRealPixel (x + 2, y) blue;
  drawRealPixel (x + 2, y + 1) blue;
  drawRealPixel (x + 2, y + 2) blue
};

/* for i in 1 to 100 {
     for j in 1 to 100 {
       drawPixel (i, j)
     }
   }; */
/* type raster = list list int; */
let spaceShipRaster = [
  /* 0 */ [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0],
  /* 1 */ [0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0],
  /* 2 */ [0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0],
  /* 3 */ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
  /* 4 */ [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1],
  /* 5 */ [1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1],
  /* 6 */ [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]
];

let draw () => {
  let raster = spaceShipRaster;
  let x = 50;
  let y = 50;
  for row_num in 0 to (List.length raster - 1)
    {
      let row = List.nth raster row_num;
      for col_num in 0 to ((List.length row) - 1) {
        let pixel = List.nth row col_num;
        if (pixel == 1) {
          drawPixel (x + col_num, y + row_num);
        };
      };
    };
};

draw ();
