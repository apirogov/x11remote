//TODO: make faster using many layers?

function getjson(url){
  var x = new XMLHttpRequest();
  x.open("GET",url,false);
  x.send();
  return JSON.parse(x.responseText);
}

//load keymap for keyboard once from server on startup
var keymap = getjson('keymap.json');
//list of keysym -> printable mappings (what to write on the key button)
var labels = getjson('labels.json');
//get keycode layout of virtual keyboard
var layout = getjson('layout.json');

//initialize canvas
var st = new Kinetic.Stage({
  container: 'mycanvas',
  width: 640,
  height: 480,
});
var c = st.container(); //for c.clientWidth/Height

var layer = new Kinetic.Layer(); //to add/remove shapes
st.add(layer);

var mode = 0; //current mode (0=mouse, 1=keyboard)

var currkeys = null; //references to current key objects of keyboard
//state of modifiers affecting key labels
var shiftPressed=false;
var capsLocked=false;
var l3shiftPressed=false;
var l5shiftPressed=false;

//remember coordinates from last event while drag-moving
var ox=null;
var oy=null;
//remember coordinates of mousedown on touchpad
var sx=null;
var sy=null;

function isdef(x){ return (typeof x != 'undefined') }

function xdo(url) {
  var xmlhttp = new XMLHttpRequest();
  xmlhttp.open("GET",url,false); //sync to prevent wrong order (like up->down)
  xmlhttp.send();
}

function newRect(x,y,w,h,borderclr,fillclr) {
  var r = new Kinetic.Rect({
    x: x,
    y: y,
    width: w,
    height: h,
    fill: fillclr,
    stroke: borderclr,
    strokeWidth: 2
  });
  return r;
}

function generateFooter(x,y,w,h) {
  var fscr = newRect(x,y,h,h,"black","yellow");
  var togk = newRect(x+h,y,w-h,h,"black","green");
  fscr.touchstart = fullscreen;
  togk.touchstart = toggleMode;
  fscr.on("mousedown", fullscreen);
  togk.on("mousedown", toggleMode);
  layer.add(fscr);
  layer.add(togk);
}

function generateMouse(x,y,w,h) {
  var th = h/4*3;
  var tpad = newRect(x,y,w,th,"black","#404040");
  var lbtn = newRect(x,th,w/5*2,h-th,"black","#b0b0b0");
  var mbtn = newRect(w/5*2,th,w/5,h-th,"black","#808080");
  var rbtn = newRect(w/5*3,th,w/5*2,h-th,"black","#b0b0b0");

  tpad.touchstart = function(){
    var pos = st.getPointerPosition();
    ox = sx = pos.x;
    oy = sy = pos.y;
  };
  tpad.touchmove = function(){
    var pos = st.getPointerPosition();
    var nx=pos.x;
    var ny=pos.y;
    var sc=2; //scale factor (movement speed)
    if (Math.abs(nx-ox)>1 || Math.abs(ny-oy)>1) {
      var dx = Math.round(nx-ox)*sc;
      var dy = Math.round(ny-oy)*sc;
      xdo("mousemove_relative/"+dx+"/"+dy);
    }
    ox = nx;
    oy = ny;
  };
  tpad.touchend = function(){
    var pos = st.getPointerPosition();
    var x = pos.x;
    var y = pos.y;
    if (Math.abs(sx-x)<3 && Math.abs(sy-y)<3)
      xdo("click/1")
    sx=sy=ox=oy=null;
  };

  lbtn.touchstart = function(){xdo("mousedown/1")};
  lbtn.touchend = function(){xdo("mouseup/1")};
  mbtn.touchstart = function(){xdo("mousedown/2")};
  mbtn.touchend = function(){xdo("mouseup/2")};
  rbtn.touchstart = function(){xdo("mousedown/3")};
  rbtn.touchend = function(){xdo("mouseup/3")};

  layer.add(tpad);
  layer.add(lbtn);
  layer.add(mbtn);
  layer.add(rbtn);
}

function newKey(keycode,x,y,w,h) {
  var rect = newRect(x,y,w,h,"black","gray");

  //keysym which will be sent
  var ksym = 'question';
  if (isdef(keymap[keycode][0]))
    ksym = keymap[keycode][0];

  rect.touchstart = function(){keyDown(ksym);};
  rect.touchend = function(){keyUp(ksym);};
  rect.on("mousedown",function(){keyDown(ksym);});
  rect.on("mouseup",function(){keyUp(ksym);});


  rect.showLevel = function(idx) {
    if (isdef(rect.txt))
      rect.txt.remove();

    //keysym which will really be shown depending on modifiers
    var keysym = 'question';
    if (isdef(keymap[keycode][idx]))
      keysym = keymap[keycode][idx];
    else if (isdef(keymap[keycode][0]))
      keysym = keymap[keycode][0];

    var label = keysym.length==1 ? keysym : '?';
    if (isdef(labels[keysym]))
      label = labels[keysym];

    var fonth = Math.min(Math.abs(h/6*5), w);

    var text = new Kinetic.Text({
      x: x+w/2,
      y: y+h/2,
      text: label,
      fontSize: fonth,
      fontFamily: 'Arial',
      fill: "black",
      });
    text.offsetX(text.width()/2);
    text.offsetY(text.height()/2);

    layer.add(text);
    rect.txt = text;
  }

  layer.add(rect);
  return rect;
}

function generateKeyboard(x,y,w,h) {
  currkeys = [];
  for (var j=0; j<layout.length; j++) {
    currkeys.push([]);
    for (var i=0; i<layout[j].length; i++) {
      var keycode = layout[j][i];

      var kw=w/layout[j].length;
      var kh=h/layout.length;
      key = newKey(keycode,i*kw,j*kh,kw,kh);

      layer.add(key);
      currkeys[(currkeys.length-1)].push(key); //store objects to change text later
    }
  }
  for (var j=0; j<layout.length; j++) {
    for (var i=0; i<layout[j].length; i++) {
      currkeys[j][i].showLevel(0);
    }
  };
}

function keyDown(ksym) {
  xdo("keydown/"+ksym);

  if (ksym=='Shift_L' || ksym=='Shift_R') {
    shiftPressed=true; updateKeyLabels();
  } else if (ksym=='ISO_Level3_Shift') {
    l3shiftPressed=true; updateKeyLabels();
  }

  if (ksym=='Caps_Lock') {
    capsLocked = !capsLocked; updateKeyLabels();
  }
}

function keyUp(ksym){
  xdo("keyup/"+ksym);

  if (ksym=='Shift_L' || ksym=='Shift_R') {
    shiftPressed=false; updateKeyLabels();
  } else if (ksym=='ISO_Level3_Shift') {
    l3shiftPressed=false; updateKeyLabels();
  }
}

function updateKeyLabels() {
  var idx = 0;
  if (shiftPressed && !l3shiftPressed && !l5shiftPressed)
    idx = 1;
  else if (capsLocked && shiftPressed)
    idx = 2;
  else if (capsLocked && !shiftPressed)
    idx = 3;
  else if (!shiftPressed && l3shiftPressed && !l5shiftPressed)
    idx = 4;
  else if (shiftPressed && l3shiftPressed && !l5shiftPressed)
    idx = 5;

  for (var j=0; j<layout.length; j++) {
    for (var i=0; i<layout[j].length; i++) {
      currkeys[j][i].showLevel(idx);
    }
  }
  st.batchDraw();
}

//update sizes of objects relative to canvas size
function updateObjects() {
  var uh = c.clientHeight/3*2+c.clientHeight/4;
  var bh = c.clientHeight-uh;
  currkeys = null;

  layer.removeChildren();

  if (mode==0) {
    generateMouse(0,0,c.clientWidth,uh);
  } else {
    generateKeyboard(0,0,c.clientWidth,uh);
  }
  generateFooter(0,uh,c.clientWidth,bh);

  // Redraw stage
  st.batchDraw();
}

function toggleMode() {
  if (mode==0) {
    mode=1;
  } else {
    mode=0;
  }
  updateObjects();
}

function fullscreen() {
    if(c.webkitRequestFullScreen) { c.webkitRequestFullScreen(); }
    else { c.mozRequestFullScreen(); }
}

// Resize handler
function resizeCanvas() {
    // Get container size
    var containerSize = {
        width: c.clientWidth,
        height: c.clientHeight
    };

    // Odd size can cause blurry picture due to subpixel rendering
    if(containerSize.width % 2 !== 0) containerSize.width--;
    if(containerSize.height % 2 !== 0) containerSize.height--;

    // Resize stage
    st.size(containerSize);

    //update objects and redraw
    updateObjects();
}

function getShapeOwningTouch(shapes, touch) {
  var shape = null;
  for (var j=0; j<shapes.length; j++) {
    var curr = shapes[j];
    if (!isdef(curr.touches))
      curr.touches = {};
    if (curr.touches[touch.identifier]===true) {
      shape = curr;
      break;
    }
  }
  if (shape==null) {
    for (var j=0; j<shapes.length; j++) {
      var a = shapes[j].getAttrs();
      var x = touch.clientX;
      var y = touch.clientY;
      if (x>=a.x && x<=(a.x+a.width) && y>=a.y && y<=(a.y+a.height)) {
        shape = shapes[j];
        break;
      }
    }
  }
  return shape;
}

//own touch event dispatcher
function handleTouch(name, evt) {
  evt.preventDefault();
  var ts = evt.changedTouches;

  for (var i=0; i<ts.length; i++) {
    var t = ts[i];
    var child = getShapeOwningTouch(layer.children, t);

    if (name == 'touchstart') {
      if (!isdef(child.touches))
        child.touches = {};
      child.touches[t.identifier] = true;

      child.touchstart();

    } else if (name == 'touchmove') {
        child.touchmove();
    } else if (name == 'touchend') {
        child.touchend();
        child.touches[t.identifier] = false;
    }

  }
}

function initialize() {
  document.body.addEventListener('touchmove', //prevent scrolling
      function(event) { event.preventDefault(); }, false);

  // Register an event listener to call the resizeCanvas() function
  // each time the window is resized.
  window.addEventListener('resize', resizeCanvas, false);

  window.addEventListener('touchstart',function(evt){handleTouch('touchstart',evt)},false);
  window.addEventListener('touchmove',function(evt){handleTouch('touchmove',evt)},false);
  window.addEventListener('touchend',function(evt){handleTouch('touchend',evt)},false);

  resizeCanvas(); //adjust canvas size to window size the first time
}

