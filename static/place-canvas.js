const placeUrl = '/place.json';

let colors = {};
let board = {};
let inf;
let canvas;
let ctx;

const size = 12;

// Prepare some variables for the dragging gestures logic
let mouseIsDown = false;
let isPanning = false;
let previousMousePosition;

function place_windowKeydown(evt) {
  if (evt.which === 68) {
    isPanning = !isPanning;
  }
}

function place_canvasMousedown(event) {
  if (event.which === 1) {
    mouseIsDown = true;
  } else {
    isPanning = true;
  }
}

function place_windowMouseup(event) {
  if (event.which === 1) {
    mouseIsDown = false;
    inf.updateChunks();
  } else {
    isPanning = false;
  }
}

function place_windowMousemove(event) {
  var newMousePosition = {x: event.offsetX, y: event.offsetY};
  if (mouseIsDown && isPanning) {
    // pan the canvas whenever dragging with the middle or right mouse button
    var dx = previousMousePosition.x - newMousePosition.x;
    var dy = previousMousePosition.y - newMousePosition.y;
    // Canvas gets really messy if you do not clear it up :)
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    inf.moveBy(dx, dy);
  } else if (mouseIsDown) {
    // draw lines when dragging with the left mouse button
    if (previousMousePosition) {
      ctx.beginPath();
      ctx.moveTo(previousMousePosition.x, previousMousePosition.y);
      ctx.lineTo(newMousePosition.x     , newMousePosition.y);
      ctx.stroke();
    }
  }
  previousMousePosition = newMousePosition;
}

function place_addListeners() {
  window.addEventListener('keydown', place_windowKeydown);
  canvas.addEventListener('mousedown', place_canvasMousedown);
  window.addEventListener("mouseup", place_windowMouseup);
  window.addEventListener("mousemove", place_windowMousemove);
}

function place_replaceTable() {
  const hnMain = document.getElementById('hnmain');
  const mainRect = hnMain.getBoundingClientRect();
  const { width } = mainRect;
  const oldPlace = document.getElementById('place');
  const parent = oldPlace.parentElement;
  parent.removeChild(oldPlace);
  const place = document.createElement('div');
  place.id = 'place';
  parent.append(place);
  canvas = document.createElement('canvas');
  canvas.id = 'canvas';
  canvas.width = width.toString();
  canvas.height = window.innerHeight.toString();
  place.appendChild(canvas);
  ctx = canvas.getContext('2d');
  inf = infiniteCanvas.initialize(ctx);
  place_addListeners();
}

function place_drawBoard() {
  board.forEach((row, rowIndex) => {
    row.forEach((pixel, pixIndex) => {
      var color = colors[pixel] || { hex: 'black' };
      ctx.fillStyle = color.hex;
      ctx.fillRect(size * pixIndex, size * rowIndex, size, size);
    });
  });
}

function place_getBoard() {
  place_replaceTable();
  fetch(placeUrl).then(resp => resp.json()).then(json => {
    colors = json.colors;
    board = json.board;
    place_drawBoard();
  });
}

window.addEventListener('load', place_getBoard);
