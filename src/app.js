var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

// store all the clients who are connected
// format: mapping from socketIds to (gameId, master, socket, ready)
var clients = {}

// store all the games that exist; mapping from game ID to the socket of its master and list of connected players
// games are added when a master joins a lobby
// games are removed when the master DCs or joins a different lobby
var games = {}

app.get('/', function(req, res){
    res.sendfile('Client.html');
});

app.get('/Domingo.js', function(req,res){
    res.sendfile('Domingo.js');
});

// log the state every second
setInterval(function() {
    console.log("-----------------------");
    console.log("Clients:");
    for (var sid in clients) {
        var res = clients[sid];
        console.log("SocketID: " + sid + "; GameID: " + res.gameId + "; master? " + res.master +
                    "; ready? " + res.ready);
    }

    console.log("Games:");
    for (var gid in games) {
        var res = games[gid];
        var playersStr = "";

        for (var player in res.connectedIds) {
            console.log("WE HAVE A PLAYER");
            playersStr = playersStr + ", " + player + " ";
        }
        console.log("GameID: " + gid + "; Players: " + playersStr);
    }
    console.log("------------------------");
}, 5000);

// TODO: maybe send acknowledgements?
// TODO: make sure the (primitive) verification I have is enough

// TODO: on a timer, try emitting the server state (ie game list)
// see if we are leaking

io.on('connection', function(socket){
    //socket.emit('game', "hi");

    //add this socket to list of clients, for later use
    //connection will not be ready yet, until
    //they tell us their game ID and whether they are spectating
    clients[socket.id] = {gameId : null, master : false, socket : socket, ready : false}

    socket.on('response', function(msg){
        console.log('got message: ' + msg);

        // parse the submitted game state to get the type and ID
        var parsedMsg = {};
        try {parsedMsg = JSON.parse(msg)}
        catch (err) {
            console.log("parse failure...");
            console.log("Result is " + parsedMsg);
        }

        // handle player registration
        if (parsedMsg.msgType === "join") {
            // are they master? if so we are creating a lobby
            if(parsedMsg.master) {
                // first, make sure that this game does not already exist
                if(! games[parsedMsg.gameId]) {
                    console.log("creating game with ID " + parsedMsg.gameId);
                    // check to see if this socket already was in a game. if so, they have quit and game is over
                    var oldGameId = clients[socket.id].gameId;
                    if (oldGameId) {
                        // TODO: what else do we need to do to clean up this game?
                        // Send a "this game is over?" message to client?
                        delete games[oldGameId];
                    }

                    // initialize with our socket id
                    games[parsedMsg.gameId] = {masterSocket : socket, connectedIds : []};
                    clients[socket.id].gameId = parsedMsg.gameId;
                }
            }
            else {
                // if they were a master/client in any other game, that game is over
                // TODO this is a little brittle
                var oldGameId = clients[socket.id].gameId;
                if (oldGameId) {
                    delete games[oldGameId];
                }
                
                var game = games[parsedMsg.gameId];

                // TODO: somehow rule out duplicate player names. this should be done on master side.
                games[parsedMsg.gameId].connectedIds =
                    games[parsedMsg.gameId].connectedIds.push(socket.id);

                clients[socket.id].gameId = parsedMsg.gameId;

                // we are not master so we need to send a player join message to master

                var outMsg = JSON.stringify({msgType : "playerJoined",
                                             gameId : parsedMsg.gameId,
                                             players : parsedMsg.players});

                console.log("sending player join message. specifically we are sending " + parsedMsg.players);
                var masterSocket = game.masterSocket;
                masterSocket.emit ('game', outMsg);
            }
        }
        // TODO this command seems like it might be superfluous
//        else if (parsedMsg.msgType == "start") {
//        }
        else if (parsedMsg.msgType == "makeMove") {
            // master will check to make sure the move is appropriate. we just forward it
            // what game do we send it to? it is the one they joined
            var clientGameId = clients[socket.id].gameId;
            
            // figure out the master's socket, and forward it
            var masterSocket = games[clientGameId].masterSocket;

            // send "made move" to master
            // I guess we are ignoring the game ID the client requests?
            // perhaps this could be a problem. :/
            masterSocket.emit ('game',
                               JSON.stringify({msgType : "madeMove",
                                               gameId : clientGameId,
                                               move : parsedMsg.move}));
        }
        // updated game state, to be forwarded to spectators
        else if (parsedMsg.msgType === "masterPush") {
            var gameState = parsedMsg.gameState;
            // forward the game state to all interested clients
            for (socketId in clients) {
                // TODO get rid of ready
                if (!clients[socketId].master && /*clients[socketId].ready &&*/
                    clients[socketId].gameId === parsedMsg.gameId)
                {
                    console.log("FORDWARDING TO INTERESTED CLIENTS");
                    socket = clients[socketId].socket;
                    socket.emit ('game',
                                 JSON.stringify({msgType : "updateGameState",
                                                 gameId : gameState.gameId,
                                                 gameState : gameState,
                                                 message : "hello, world"}));
                }
            }
        }
    });

    socket.on('disconnect', function(){
	console.log('user disconnected');
        // remove this socket from the list
        delete clients[socket.id]
    });
});

http.listen(3000, function(){
    console.log('listening on *:3000');
});
