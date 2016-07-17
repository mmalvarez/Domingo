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

        // handle spectator registration
        if (parsedMsg.msgType === "spectate") {
            gId = parsedMsg.gameId;
            console.log("registering spectator on game " + gId);
            clients[socket.id] =  {gameId : gId, master : false, socket : socket, ready : true};
        }
        // handle player registration
        // todo, START messages should actually be a thing at some point
/*        else if (parsedMsg.msgType === "start") {
            console.log("STARTING A GAME");
            gId = parsedMsg.gameId;
            clients[socket.id] = {gameId : gId, master : true, socket : socket, ready : true};
        } */
        // updated game state, to be forwarded to spectators
        else if (parsedMsg.msgType === "update") {
            var gameState = parsedMsg.gameState;
            // forward the game state to all interested clients
            for (socketId in clients) {
                if (!clients[socketId].master && clients[socketId].ready &&
                    clients[socketId].gameId === parsedMsg.gameId)
                {
                    console.log("FORDWARDING TO INTERESTED CLIENTS");
                    socket = clients[socketId].socket;
                    socket.emit ('game', JSON.stringify({msgType : "updateGameState", gameState : gameState, message : "hello, world"}));
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
