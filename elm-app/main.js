import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

var sse = new EventSource("http://localhost:3000/sse", {withCredentials: true});

// TODO: What happens when we leave /#/overlay? Are the deactivated? Or do they
// continue running?
sse.onmessage = function (event) {
    app.ports.sseMessageReceiver.send(event.data);
};

sse.onopen = function () {
    app.ports.sseOpenReceiver.send("");
};

sse.onerror = function (event) {
    app.ports.sseErrorReceiver.send(event);
};
