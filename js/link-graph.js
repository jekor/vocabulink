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

// linkColors are pairs of foreground/background colors keyed by link type
// names.
var linkColors = {
  'association': ['#000000', '#DFDFDF'],
  'cognate':     ['#00AA00', '#DFF4DF'],
  'linkword':    ['#0000FF', '#DFDFFF']
};

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

function createGraph_(block) {
  // TODO: What about just replacing the contents and maybe changing the style of the h1?
  var graph = $('<div class="graph" style="height: 100px"></div>');
  $(block).replaceWith(graph);
  var gdims = {'w': graph.width(),
               'h': graph.height()};
  var g = {'graph': new Raphael(graph[0], gdims.w, gdims.h),
           'node': graph[0],
           'width': gdims.w, 'height': gdims.h};
  return g;
}

// Convert an <h1> representing a link into a more visually-expressive version
// using SVG.
function drawH1Link() {
  var orig = $(this).find('.orig').text();
  var dest = $(this).find('.dest').text();
  var label = '';
  var colors = linkColors['association'];
  if ($(this).hasClass('cognate')) {
    colors = linkColors['cognate'];
  } else if ($(this).hasClass('linkword')) {
    label = $(this).find('.linkword').text();
    colors = linkColors['linkword'];
  }
  $(this).empty().css('height', '100px');
  var gdims = {'w': $(this).width(),
               'h': 100};
  var g = {'graph': new Raphael(this, gdims.w, gdims.h),
           'node': this,
           'width': gdims.w, 'height': gdims.h};
  drawLink(g, {'orig': orig, 'dest': dest, 'label': label,
               'color': colors[0], 'bgcolor': colors[1]});
}
