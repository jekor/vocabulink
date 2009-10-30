// Copyright 2008, 2009 Chris Forno
//
// This file is part of Vocabulink.
//
// Vocabulink is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

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
function graphNode(g, label, p, labelColor, outlineColor, hoverColor, style) {
  var t = g.graph.text(p.x, p.y, label).attr({'font-size': '14pt','fill': labelColor});
  var dims = t.getBBox();
  var height = dims.height*1.7;
  var e = g.graph.ellipse(p.x, p.y, Math.max(dims.width, height), height).attr({'fill': '#FFF', 'stroke': outlineColor, 'stroke-width': 3});
  if (style == 'dotted') {
    e.attr({'stroke-dasharray': '.'});
  }
  // We have to place the text first so that we can calculate the dimensions of
  // the ellipse. But we want the ellipse behind the label since it needs to be
  // non-transparent to be clickable.
  e.insertBefore(t);
  // We add event handling functions that allow us to easy add the functions to
  // both the ellipse and the label (the interface is difficult to use without
  // that).
  return {'label': t, 'ellipse': e,
          'labelcolor': labelColor,
          'outline': outlineColor,
          'hovercolor': hoverColor,
          'mouseover': function (f) { t.mouseover(f); e.mouseover(f); },
          'mouseout': function (f) { t.mouseout(f); e.mouseout(f); },
          'click': function (f) { t.click(f); e.click(f); }};
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
  return map(function (x) {
               var p = x[0];
               var s = x[1];
               var point = normalize(g, p);
               if (reverse) {
                 point.x = g.width/2 - (point.x - g.width/2);
               }
               var n = graphNode(g, reverse ? s.orig : s.dest, point, '#00F', s.color, s.bgcolor, s.style);
               var l = g.graph.path({'stroke': s.color, 'stroke-width': 3});
               if (s.style == 'dotted') {
                 l.attr({'stroke-dasharray': '.'});
               }
               l.moveTo(g.width/2, g.height/2).lineTo(point.x, point.y).toBack();
               return {'node': n, 'line': l}; },
             zip(ellipticalArc(g.width*0.667, g.height*0.9, Math.PI, 3/2*Math.PI, ss.length), ss));
}

function drawLinkHelper(g, ns) {
  var link = ns[0];
  var node = ns[1];
  var mouseIn  = function () {
    node.node.ellipse.animate({'fill': node.node.hovercolor}, 250);
    g.focus.ellipse.animate({'fill': g.focus.hovercolor}, 250);
    node.line.animate({'stroke-width': 4}, 250);
    document.body.style.cursor = 'pointer';
  };
  var mouseOut = function () {
    node.node.ellipse.animate({'fill': '#FFF'}, 250);
    g.focus.ellipse.animate({'fill': '#FFF'}, 250);
    node.line.animate({'stroke-width': 3}, 250);
    document.body.style.cursor = 'auto';
  };
  var action = function () {
    if (link.url !== undefined) {
      document.location = link.url;
    }
    else if (link.number !== undefined) {
      document.location = "/link/" + link.number;
    }
  };
  node.node.mouseover(mouseIn);
  node.node.mouseout(mouseOut);
  node.node.click(action);  
}

// This sets up the link interface given a focal lexeme as well as arrays of
// origin lexemes and destination lexemes from links the focal lexeme
// participates in.
//
// For now the arguments are strings.
function drawLinks(focus, origs, dests) {
  var graph = $('#graph');
  var vdims = {'w': $(window).width(),
               'h': $(window).height()};
  vdims.h = Math.max(vdims.h - 275, 400);
  var gdims = {'w': graph.width(),
               'h': graph.height()};
  var g = {'graph': new Raphael('graph', gdims.w, vdims.h),
           'width': gdims.w, 'height': vdims.h};
  g.focus = graphNode(g, focus, {'x': g.width/2, 'y': g.height/2}, '#000', '#000', '#DFDFDF');
  var destsNodes = zip(dests, arcNodes(g, dests));
  var origsNodes = zip(origs, arcNodes(g, origs, true));
  map(drawLinkHelper.curry(g), $.merge(destsNodes, origsNodes));
}

function createGraph() {
  var graph = $('#graph');
  graph.empty();
  var gdims = {'w': graph.width(),
               'h': graph.height()};
  var g = {'graph': new Raphael('graph', gdims.w, gdims.h),
           'width': gdims.w, 'height': gdims.h};
  return g;
}

function drawLink(g, link) {
  var l = g.graph.path({'stroke': link.color, 'stroke-width': 3}).moveTo(g.width*0.3, g.height/2).lineTo(g.width*0.7, g.height/2);
  var origNode = graphNode(g, link.orig, {'x': g.width*0.3, 'y': g.height/2}, '#00F', '#000', '#DFDFDF');
  var destNode = graphNode(g, link.dest, {'x': g.width*0.7, 'y': g.height/2}, '#00F', link.color, link.bgcolor);
  if (link.label !== "") {
    var ldims = origNode.ellipse.getBBox();
    var rdims = destNode.ellipse.getBBox();
    var midpoint = (rdims.x + (ldims.x + ldims.width)) / 2;
    var linkLabel = g.graph.text(midpoint, g.height/2 - 18, link.label).attr({'font-size': '14pt', 'fill': '#000'});
  }
  var mouseIn  = function (node) {
    node.ellipse.animate({'fill': node.hovercolor}, 250);
    document.body.style.cursor = 'pointer';
  };
  var mouseOut = function (node) {
    node.ellipse.animate({'fill': '#FFF'}, 250);
    document.body.style.cursor = 'auto';
  };
  var action = function (s) {document.location = "/links?contains=" + s;};
  origNode.mouseover(mouseIn.curry(origNode));
  origNode.mouseout(mouseOut.curry(origNode));
  origNode.click(action.curry(link.orig));
  destNode.mouseover(mouseIn.curry(destNode));
  destNode.mouseout(mouseOut.curry(destNode));
  destNode.click(action.curry(link.dest));
}

// This draws a link with the destination obscured (for reviews).
function drawLinkReview(link, callback) {
  // We want to give away as little as possible about the link.
  // The idea is that eventually we'll support hinting. When a member asks for
  // a hint, we might show the color of the link. The next hint might be the
  // link word (if one exists), etc.
  // At first, I thought I could repurpose drawLink() or make it more general.
  // But after some review it has less in common with the needs of this
  // function than I thought.
  var g = createGraph();
  var l = g.graph.path({'stroke': '#000', 'stroke-width': 3}).moveTo(g.width*0.3, g.height/2).lineTo(g.width*0.7, g.height/2);
  var origNode = graphNode(g, link.orig, {'x': g.width*0.3, 'y': g.height/2}, '#000', '#000', '#DFDFDF');
  var destNode = graphNode(g, '?', {'x': g.width*0.7, 'y': g.height/2}, '#000', '#000', '#DFDFDF');
  var mouseIn  = function (node) {
    node.ellipse.animate({'fill': node.hovercolor}, 250);
    document.body.style.cursor = 'pointer';
  };
  var mouseOut = function (node) {
    node.ellipse.animate({'fill': '#FFF'}, 250);
    document.body.style.cursor = 'auto';
  };
  origNode.mouseover(mouseIn.curry(origNode));
  origNode.mouseout(mouseOut.curry(origNode));
  destNode.mouseover(mouseIn.curry(destNode));
  destNode.mouseout(mouseOut.curry(destNode));
  return {'graph': g.graph, 'node': destNode};
  // var reveal = function () {g.graph.remove(); drawLink(g, link);};
  // destNode.click(callback);
  // return reveal;
}

function createGraph_(block) {
  // TODO: What about just replacing the contents and maybe changing the style of the h1?
  var graph = $('<div class="graph" style="height: 100px"></div>');
  $(block).replaceWith(graph);
  var gdims = {'w': graph.width(),
               'h': graph.height()};
  var g = {'graph': new Raphael(graph[0], gdims.w, gdims.h),
           'width': gdims.w, 'height': gdims.h};
  return g;
}

function drawLink_() {
  // TODO: Switch drawLink to take the target block as a parameter.
  var orig = $(this).find('.orig').text();
  var dest = $(this).find('.dest').text();
  var label = '';
  if ($(this).hasClass('linkword')) {
    label = $(this).find('.linkword').text();
  }
  var g = createGraph_(this);
  drawLink(g, {'orig': orig, 'dest': dest, 'label': label,
               'color': '#0000FF', 'bgcolor': '#DFDFFF'});
}

$(document).ready(function () {
  $('h1.link').each(drawLink_);
});
