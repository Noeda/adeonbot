
import React from 'react';
import { render } from 'react-dom';
import { createStore } from 'redux';
import { connect, Provider } from 'react-redux';

const CONNECTION_OPEN = 'CONNECTION_OPEN';
const CONNECTION_CLOSE = 'CONNECTION_CLOSE';
const WORLD_UPDATE = 'WORLD_UPDATE';

/* Handle communication with robot */

const initialState = {
    connectionOpen: false,
    latestWorld: {}
};

function diagnosticsApp(state = initialState, action)
{
    if ( action.type === CONNECTION_OPEN ) {
        return { ...state, connectionOpen: true };
    } else if ( action.type === CONNECTION_CLOSE ) {
        return { ...state, connectionOpen: false };
    } else if ( action.type === WORLD_UPDATE ) {
        return { ...state, latestWorld: action.world };
    }
    return state;
}

let store = createStore(diagnosticsApp);

function newWebSocketConnection()
{
    const loc = window.location;
    let new_uri;
    if ( loc.protocol == 'https:' ) {
        new_uri = 'wss:';
    } else {
        new_uri = 'ws:';
    }

    new_uri += '//' + loc.host;
    new_uri += loc.pathname + 'api/v1/websocket/botfeed';

    console.log(new_uri);

    const ws = new WebSocket(new_uri);

    ws.addEventListener('open', (event) => {
        store.dispatch({ type: CONNECTION_OPEN });
    });
    ws.addEventListener('close', (event) => {
        store.dispatch({ type: CONNECTION_CLOSE });
    });
    ws.addEventListener('message', (event) => {
        store.dispatch({ type: WORLD_UPDATE, world: JSON.parse(event.data) });
    });
}

newWebSocketConnection();

const AppComponent = ({connectionOpen, world}) => {
    if (connectionOpen) {
        return <p>{JSON.stringify(world)}</p>
    } else {
        return <p>Nay</p>
    }
}

const mapStateToPropsApp = state => {
    return {
        connectionOpen: state.connectionOpen,
        world: state.latestWorld
    }
}
const mapDispatchToPropsApp = dispatch => { return {} }

const App = connect(mapStateToPropsApp, mapDispatchToPropsApp)(AppComponent);

render(<Provider store={store}>
         <App />
       </Provider>
       ,document.getElementById('content'));

