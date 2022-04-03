// The textQueue here is only an example, it probably should only contain records like
// {
//   event: "follow",
//   twitchUserName: "chiroptical",
// }

import { SVG } from '@svgdotjs/svg.js'

// Build and return a transparent text object to animate onto the screen
const thankYou = (evt, twitchUserName, draw) => {
  return draw
    .plain('Thanks for the ' + evt + ' ' + twitchUserName + '!')
    .fill('#975df9')
    .opacity(0.0)
    .font({family: 'Helvetica', size: 48})
    .cx(960)
    .cy(270)
}

// Fade a transparent text object to 100% and back to 0%. Then remove the object
// All animations should be responsible for removing their resources
const textAnimate = text => {
  text
    .animate({duration: 1000})
    .opacity(1.0)

  text
    .animate({duration: 1000})
    .opacity(0.0)

  setTimeout(() => text.remove(), 2000)
}

// A generic wait function used within async loops
async function wait(seconds) {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve();
    }, seconds * 1000);
  });
}

document.addEventListener('DOMContentLoaded', (event) => {
  var draw = SVG().addTo('body').size(1920, 1080)

  // Build a text queue that we can drop text onto
  var textQueue = []

  // Initialize some objects on the queue
  textQueue.push(thankYou('follow', 'chiroptical', draw))
  textQueue.push(thankYou('bits', 'chiroptical', draw))

  // Every 2 seconds, pull something from the text queue, otherwise do nothing
  var textAnimations = async () => {
    while (true) {
      if (textQueue.length) {
        var text = textQueue.shift()
        textAnimate(text)
      }
      await wait(2)
    }
  }
  textAnimations()

  // Add some more stuff to the text queue to ensure things run in order
  textQueue.push(thankYou('raid', 'chiroptical', draw))
  textQueue.push(thankYou('follow', 'chiroptical', draw))
  textQueue.push(thankYou('bits', 'chiroptical', draw))
});
