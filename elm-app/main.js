import "./style.css";
import { Elm } from "./src/Main.elm";

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

var sse = new EventSource("http://localhost:3000/sse", {withCredentials: true});

sse.onmessage = function (event) {
    console.log("onmessage")
    app.ports.sseMessageReceiver.send(event.data);
};

sse.onopen = function () {
    console.log("onopen")
    app.ports.sseOpenReceiver.send();
};

sse.onerror = function (event) {
    console.log("onerror")
    app.ports.sseErrorReceiver.send(event);
};
