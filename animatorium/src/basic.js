import { SVG } from '@svgdotjs/svg.js'

document.addEventListener('DOMContentLoaded', (event) => {
  var draw = SVG().addTo('body').size(1920, 1080)
  var rect = draw.rect(400, 100).attr({ fill: '#975df9' })
});
