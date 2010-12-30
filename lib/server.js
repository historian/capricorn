var http  = require('http')
,   url   = require('url')
,   spawn = require('child_process').spawn
,   path  = require('path')
,   fs    = require('fs')
;

var locks = {};

var exec
;

exec = function(location, out, clb){
  var toolname
  ;

  location = url.parse(location, true);
  toolname = path.join(__dirname, 'tools', location.pathname);

  fs.realpath(toolname, function(err, toolname){
    if (err) {
      clb({ type:'error', code: 404, message: '404: Unknown tool: '+location.pathname+'\n' });
      return;
    }

    var tool
    ,   acquired_locks = []
    ,   env = {}
    ;

    Object.keys(process.env).forEach(function(key){
      env[key] = process.env[key];
    });

    Object.keys(location.query || {}).forEach(function(key){
      var env_key
      ;

      env_key = ('CAPR_'+key).replace(/[^a-z0-9]+/ig, '_').toUpperCase();

      env[env_key] = location.query[key];
    });

    env['PATH'] = path.join(__dirname, 'bin') + ':' + env['PATH'];

    tool = spawn(toolname, [],
                { env: env
                , cmd: path.join(__dirname, 'tools')
                });

    console.log('Executing: '+location.pathname);

    clb({ type:'started', code: 200 });
    out.write('');
    out.write('');

    tool.stdout.on('data', function(data){
      var msg  = data.toString()
      ,   msgs = msg.trim().split('\n')
      ;

      msgs.forEach(function(msg){
        // console.log([msg]);

        if (msg.substr(0,3) == '\033\033\033') {
          var extra = msg.substr(4).trim()
          ;

          switch(msg.substr(3,1)){
          case 'l': // lock
            if (locks[extra]) {
              locks[extra].push(function(){
                acquired_locks.push(extra);
                try { tool.stdin.write('\033\033\033l\006\n'); } catch(e){}
              });
            } else {
              locks[extra] = [];
              acquired_locks.push(extra);
              try { tool.stdin.write('\033\033\033l\006\n'); } catch(e){}
            }
            return;

          case 'L': // unlock
            delete acquired_locks[acquired_locks.indexOf(extra)];
            if (locks[extra] && locks[extra].length > 0) {
              locks[extra].shift()();
              try { tool.stdin.write('\033\033\033L\006\n'); } catch(e){}
            } else {
              delete locks[extra];
              try { tool.stdin.write('\033\033\033L\006\n'); } catch(e){}
            }
            return;

          case 'e': // exec
            exec(extra, out, function(status){
              switch(status.type) {
              case 'error':
                try { tool.stdin.write('\033\033\033e\025\n'); } catch(e){}
                return;

              case 'started':
                try { tool.stdin.write('\033\033\033e\006\n'); } catch(e){}
                return;

              case 'finished':
                try { tool.stdin.write('\033\033\033e\004\n'); } catch(e){}
                return;
              }
            });
            return;
          }
        }

        out.write(msg+'\n');
      });
    });

    tool.stderr.on('data', function(data){
      out.write(data);
    });

    tool.on('exit', function(code){
      acquired_locks.forEach(function(lock){
        if (locks[lock] && locks[lock].length > 0) {
          locks[lock].shift()();
        } else {
          delete locks[lock];
        }
      });
      clb({ type: 'finished', code: code });
    });

  });
};

http.createServer(function (request, response) {
  var location
  ,   toolname
  ;

  exec(request.url, response, function(status){
    switch(status.type) {
    case 'error':
      response.writeHead(status.code, {'Content-Type': 'text/plain'});
      response.end(status.message);
      return;

    case 'started':
      response.writeHead(status.code,
        { 'Content-Type': 'text/plain'
        , 'Transfer-Encoding': 'chunked'
        });
      return;

    case 'finished':
      response.end();
      return;
    }
  });

}).listen(8124);

console.log('Server running at http://127.0.0.1:8124/');