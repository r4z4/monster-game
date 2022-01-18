import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import imgPath from './test.png';

let counter = 1;

//Need to pass image path as a flag for Html.programWithFlags
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { src: imgPath }
});

app.ports.sendStuff.subscribe(data => {
  console.log(JSON.stringify(data));
});
//VDom vs actual DOM....god damnit
document.getElementById( 'endmonster' ) ? console.log('ththththth') : '';

setInterval(() => {
  counter += 1;
  console.log(JSON.stringify(counter));
  console.log(app.ports);
  app.ports.receiveStuff.send({ value: counter });
}, 5000);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
