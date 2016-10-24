var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

// TODO fix all the ugly truthiness checks,
// and add some real error handling (exceptions) while you're at it

// store all the clients who are connected
// format: mapping from socketIds to (gameId, master, socket, masterSocket, started)
// where socket is the socket whose id is the key, and masterSocket is master's socket (if there is a master)
var clients = {}

// as an optimization, store game -> sockets mappings
// do this later as I didn't get it right
// var games = {}

// TODO we need log message handling

// helper for cleanup when a client disconnects/ends game
// TODO socket ID might not be 100% necessary
function endGame(gameId, mySocket) {
    // TODO we also need to update clients[mySocketId] to reset state
    console.log("Ending game " + gameId + " on socket " + mySocket.id);
    for (var sId in clients) {
        // if this is the game we are ending
        if (clients[sId].gameId === gameId) {
            var theSocket = clients[sId].socket;

            // something went wrong if socket isn't defined here
            if (theSocket) {
                // don't send quit message to ourselves
                if (sId !== mySocket.id) {
                    theSocket.emit('game', JSON.stringify({msgType : "playerQuit"}));
                }
            }
            else {
                console.log("uh-oh, couldn't get socket!");
            }

            clients[sId] = {gameId : null, master : false, socket : theSocket, masterSocket : null, started : false};
        }
    }
}

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
    for (var sId in clients) {
        var res = clients[sId];
        console.log("SocketID: " + sId + "; GameID: " + res.gameId + "; master? " + res.master);
    }

    /*
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
    */
    console.log("------------------------");
}, 5000);

// check if a game exists, by ID. if it does, return its master socket
// TODO we should actually return the master socket
function getMasterSocket(gameId) {
    for (var sId in clients) {
        if (clients[sId].gameId === gameId) {
            return clients[sId].masterSocket
        }
    }
    return null;
}

// TODO: maybe send acknowledgements?
// TODO: make sure the (primitive) verification I have is enough

io.on('connection', function(socket){
    //add this socket to list of clients, for later use
    //connection will not be ready yet, until
    //they tell us their game ID
    clients[socket.id] = {gameId : null, master : false, socket : socket, masterSocket : null, started : false};

    socket.on('response', function(msg){
        console.log('got message: ' + msg);

        // parse the submitted game state to get the type and ID
        var parsedMsg = {};

        try {parsedMsg = JSON.parse(msg)}
        catch (err) {
            console.log("parse failure...");
            console.log("Result is " + parsedMsg);
        }

        // TODO put a big "try catch" around all this?
        // handle player registration
        if (parsedMsg.msgType === "join") {

            // first, if this socket was already in a game, that game is now over.
            if (clients[socket.id].gameId) {
                endGame(clients[socket.id].gameId, socket.id);
            }

            clients[socket.id].gameId = parsedMsg.gameId;
            
            var masterSocket = getMasterSocket(parsedMsg.gameId);
            
            // does the game we are joining exist yet? if so we are joining
            if (masterSocket) {
                // see if the game has started (if so we can't join)
                if (!clients[masterSocket.id].started) {
                    
                    // set our master socket
                    clients[socket.id].master = false;
                    clients[socket.id].masterSocket = masterSocket;
                    
                    // construct and send join message
                    var joinMsg = JSON.stringify({msgType : "playerJoined",
                                                  gameId : parsedMsg.gameId,
                                                  players : parsedMsg.players});

                    console.log("sending player join message. specifically we are sending " + parsedMsg.players);

                    // TODO this was crashing before sometimes
                    masterSocket.emit('game', joinMsg);

                    // also, let the player know that they got in (and need to wait for client msg)
                    var successMsg = JSON.stringify({msgType : "lobbyResponse",
                                                     response : "lobbyClient"});

                    clients[socket.id].socket.emit('game', successMsg);
                }
                // let the client know they couldn't get in
                else {
                    var failMsg = JSON.stringify({msgType : "lobbyResponse",
                                                  response : "lobbyFail"});
                    
                    clients[socket.id].socket.emit('game', failMsg);
                }
            }
            // if not we are creating a lobby
            else {
                console.log("creating game: " + parsedMsg.gameId);
                
                // set us as master, which means we are our own master socket
                clients[socket.id].master = true;
                clients[socket.id].masterSocket = socket;

                // let the client know that they are master
                var successMsg = JSON.stringify({msgType : "lobbyResponse",
                                                 response : "lobbyMaster"});

                clients[socket.id].socket.emit('game', successMsg);
            }
        }
        else if (parsedMsg.msgType === "makeMove") {
            // master will check to make sure the move is appropriate. we just forward it
            // what game do we send it to? it is the one they joined
            var clientGameId = clients[socket.id].gameId;
            
            // figure out the master's socket, and forward it
            var masterSocket = clients[socket.id].masterSocket;

            // send "made move" to master
            if (masterSocket) {
                masterSocket.emit ('game',
                                   JSON.stringify({msgType : "madeMove",
                                                   gameId : clientGameId,
                                                   move : parsedMsg.move}));
            }
            else {
                console.log("got a 'make move' message at an unexpected time...");
            }
        }
        // updated game state, to be forwarded to spectators
        // TODO it looks like this is coming in the wrong socket (?!)
        else if (parsedMsg.msgType === "masterPush") {
            // make sure we actually are master
            console.log ("About to push. We are socket " + socket.id);
            console.log ("Are we master?: " + clients[socket.id].master);
            if (clients[socket.id].master) {
                var gameState = parsedMsg.gameState;
                // forward the game state to all interested clients
                for (socketId in clients) {
//                    console.log ("CHECK master: " + clients[socketId].master);
//                    console.log ("Looking for game " + parsedMsg.gameId + " - found " + clients[socketId].gameId);
                    if (!clients[socketId].master &&
                        clients[socketId].gameId === parsedMsg.gameId)
                    {
//                        console.log("FORDWARDING TO INTERESTED CLIENTS");
                        var socket2 = clients[socketId].socket;
                        socket2.emit ('game',
                                      JSON.stringify({msgType : "updateGameState",
                                                      gameId : gameState.gameId,
                                                      gameState : gameState,
                                                      message : "hello, world"}));
                    }
                }
            }
        }
        // update the state of the game lobby
        else if (parsedMsg.msgType === "masterLobbyPush") {
            // make sure they are actually master
            if (clients[socket.id].master) {
                var theGameId = clients[socket.id].gameId;

                for (socketId in clients) {
                    if (clients[socketId].gameId === theGameId) {
                        // update the lobby
                        clients[socketId].socket.emit('game',
                                                      JSON.stringify({msgType : "updateLobbyState",
                                                                      players : parsedMsg.players}));
                    }
                }
            }
        }
        // log the moves people do
        // TODO use the new log system
        else if (parsedMsg.msgType === "gameLog") {
            var theGameId = clients[socket.id].gameId;
            // TODO make sure they are actually master if they are issuing a move
            if (parsedMsg.logEntry) {
                for (socketId in clients) {
                    if (clients[socketId].gameId === theGameId) {
                        // send the log
                        clients[socketId].socket.emit('game',
                                                      JSON.stringify({msgType : "gameLog",
                                                                      gameId : theGameId,
                                                                      logEntry : parsedMsg.logEntry}));
                    }
                }
            }
        }
        // clean up the game
        else if (parsedMsg.msgType === "quit") {
            var gameIdToEnd = clients[socket.id].gameId;
            if (gameIdToEnd) {
                if (getMasterSocket(gameIdToEnd)) {
                    endGame(gameIdToEnd, socket);
                }
            }
        }
    });

    socket.on('disconnect', function(){
	console.log('user disconnected');

        var client = clients[socket.id];
        if (client) {
            var gameIdToEnd = clients[socket.id].gameId;

            if(gameIdToEnd) {
                endGame(gameIdToEnd, socket);
            }
            
            // remove this socket from the list
            delete clients[socket.id];
        }
    });
});

http.listen(3000, function(){
    console.log('listening on *:3000');
});
