type canvasRenderingContext2D;

type canvasElement;

external document : Dom.document = "" [@@bs.val];

external getElementById : Dom.document => string => option Dom.element =
  "" [@@bs.return null_to_opt] [@@bs.send];

external elementToCanvasElement : Dom.element => canvasElement = "%identity";

external canvasElementToJsObj : canvasElement => Js.t {..} = "%identity";

external canvasRenderingContext2DToJsObj : canvasRenderingContext2D => Js.t {..} = "%identity";
