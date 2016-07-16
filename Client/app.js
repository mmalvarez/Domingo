var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

// store all the clients who are connected
// format: mapping from socketIds to (gameId, master, socket, ready)
var clients = {}

app.get('/', function(req, res){
    res.sendfile('Client.html');
});

app.get('/Domingo.js', function(req,res){
    res.sendfile('Domingo.js');
});

io.on('connection', function(socket){
    socket.emit('game', "hi");

    //add this socket to list of clients, for later use
    //connection will not be ready yet, until
    //they tell us their game ID and whether they are spectating
    clients[socket.id] = {gameId = null; master = false; socket = socket; ready = false}

    socket.on('response', function(msg){
        console.log('message: ' + msg);

        // parse the submitted game state to get the type and ID
        var parsedMsg = {};
        try {gameState = JSON.parse(parsedMsg)}
        catch (err) {
            console.log("parse failure...");
        }

        // handle spectator registration
        if (parsedMsg.type === "spectate") {
            gId = parsedMsg.gameId
            clients[socket.id] =  {gId, false, socket, true}
        }
        // handle player registration
        else if (parsedMsg.type === "start") {
            gId = parsedMsg.gameId
            clients[socket.id] = {gId, true, socket, true}
        }
        // updated game state, to be forwarded to spectators
        else if (parsedMsg.type === "update") {
            var gameState = parsedMsg.gameState
            // forward the game state to all interested clients
            for (socketId in clients) {
                if (!clients[socketId].master && clients[socketId].ready &&
                    clients[socketId].gameId === parsedMsg.gameId)
                {
                    socket = clients[socketId].socket
                    socket.send (JSON.stringify({msgType = "updateGameState", gameState = gameState}))
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
