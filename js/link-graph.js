// Link Graph Interface

// The following use "g" to pass around the state of the graphics object.
// It's defined as an object with these properties:
// "graph" - a Raphael graph object
// "width", "height" - the dimensions of the drawing surface
// "focus" - the focal graph node (the central node created by the graphNode
//           function)
// The "p" parameter is a point---an object with "x" and "y" properties.

// Create a classic ellipse-and-label graph node.
//
// "label" is a string to display on the node.
function graphNode(g, label, p) {
  var l = g.graph.text(p.x, p.y, label).attr({'font-size': '14pt', 'fill': '#00F'});
  var dims = l.getBBox();
  var e = g.graph.ellipse(p.x, p.y, dims.width, dims.height*1.8).attr({'fill': '#FFF'});
  // We have to place the text first so that we can calculate the dimensions of
  // the ellipse. But we want the ellipse behind the label since it needs to be
  // non-transparent to be clickable.
  e.insertBefore(l);
  return {'label': l, 'ellipse': e};
}

// In order to place nodes in a visually-pleasing and space-optimal way, we can
// use an ellipse. This will give us coordinates along an imaginary ellipse
// beginning at angle "start" and continuing "spread" angles, divided evenly in
// "steps" parts.
//
// Actually, the "spread" angle is divided into "steps"+1 sections and the
// points are placed on the midpoint of each section. This allows the points to
// naturally group around the midpoint of the arc.
//
// "width" and "height" are the dimensions of an ellipse in canonical form.
// (a*2 and b*2 using conventional definitions.
//
// All angles should be specified in radians.
//
// This returns an array of points (objects with "x" and "y" properties).
function ellipticalArc(width, height, spread, start, steps) {
  var a = width/2;
  var b = height/2;
  var points = [];
  for (var i = 1; i <= steps; i++) {
    var t = start + (i * spread / (steps + 1));
    points.push({'x': Math.round(a * Math.cos(t)),
                 'y': Math.round(b * Math.sin(t))});
  }
  return points;
}

// This maps a point in cartesian coordinates ((0,0) at the center of the
// graph) to a point on the drawing surface ((0,0) at the top left of the
// graph).
function normalize(g, p) {
  return {'x': g.width/2 + p.x, 'y': g.height/2 + p.y};
}

// Given a list of node labels ("ss"), this creates nodes along the right arc
// (default) or the left arc (when reverse is set to true).
function arcNodes(g, ss, reverse) {
  return map(function(p,s) {
               var point = normalize(g, p);
               if (reverse)
                 point.x = g.width/2 - (point.x - g.width/2);
               var n = graphNode(g, s, point, partial(log, 'hello'));
               var l = g.graph.path({'stroke': '#000'}).moveTo(g.width/2, g.height/2).lineTo(point.x, point.y).toBack();
               return {'node': n, 'line': l}; },
             ellipticalArc(g.width*0.8, g.height*0.9, Math.PI, 3/2*Math.PI, ss.length),
             ss);
}

// This sets up the link interface given a focal lexeme as well as arrays of
// origin lexemes and destination lexemes from links the focal lexeme
// participates in.
//
// For now the arguments are strings.
function draw(focus, origs, dests) {
  var graph = $('graph');
  var vdims = getViewportDimensions();
  vdims.h = Math.max(vdims.h - 100, 400);
  var gdims = getElementDimensions(graph);
  var graph = Raphael('graph', gdims.w, vdims.h);
  var g = {'graph': graph, 'width': gdims.w, 'height': vdims.h};
  g.focus = graphNode(g, focus, {'x': g.width/2, 'y': g.height/2});
  var destsNodes = zip(dests, arcNodes(g, map(function(x) {return x.lexeme;}, dests)));
  var origsNodes = zip(origs, arcNodes(g, map(function(x) {return x.lexeme;}, origs), true));
  forEach(chain(destsNodes, origsNodes), function(ns) {
    var link = ns[0];
    var node = ns[1];
    var mouseIn  = function() {
      node.node.ellipse.animate({'fill': '#CCC'}, 250);
      g.focus.ellipse.animate({'fill': '#CCC'}, 250);
      node.line.animate({'stroke-width': 4}, 250);
      document.body.style.cursor = 'pointer';
    };
    var mouseOut = function() {
      node.node.ellipse.animate({'fill': '#FFF'}, 250);
      g.focus.ellipse.animate({'fill': '#FFF'}, 250);
      node.line.animate({'stroke-width': 1}, 250);
      document.body.style.cursor = 'auto';
    };
    var action = function() {
      if (link.url !== undefined)
        document.location = link.url;
      else if (link.number !== undefined)
        document.location = "/link/" + link.number;
    };
    node.node.ellipse.mouseover(mouseIn);
    node.node.ellipse.mouseout(mouseOut);
    node.node.ellipse.click(action);
    node.node.label.mouseover(mouseIn);
    node.node.label.mouseout(mouseOut);
    node.node.label.click(action);
  });
}
