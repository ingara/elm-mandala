import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const width = window.innerWidth;
const height = window.innerHeight;

const app = Elm.Main.init({
    node: document.getElementById('root'),
    flags: {
        width,
        height,
    },
});

app.ports.toJs.subscribe(({ msg, payload }) => {
    console.log({ msg, payload });
    const canvas = document.getElementById('canvas');
    switch (msg) {
        case 'SAVE':
            const data = canvas.toDataURL();
            console.log({
                canvas,
                data,
            });
            app.ports.fromJs.send({
                msg: 'SAVED',
                payload: data,
            });
            break;
        case 'LOAD':
            const img = new Image();
            img.onload = () => {
                const ctx = canvas.getContext('2d');
                ctx.drawImage(img, 0, 0);
            };
            img.src = payload;

            break;
    }
});

registerServiceWorker();
