var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

app.get('/', function(req, res){
    res.sendfile('Client.html');
});

app.get('/Domingo.js', function(req,res){
    res.sendfile('Domingo.js');
});

io.on('connection', function(socket){
    socket.emit('game', "hi");

    socket.on('response', function(msg){
	console.log('message: ' + msg);
    });

    socket.on('disconnect', function(){
	console.log('user disconnected');
    });
});

http.listen(3000, function(){
    console.log('listening on *:3000');
});
