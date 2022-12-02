
let build_static_routes debug =
  if debug
  then Dream.scope "/static" [] [
    Dream.get "/js/**" @@ Dream.static "_build/default/js";
    Dream.get "/styles/**" @@ Dream.static "_build/default/styles";
  ]
  else Dream.scope "/static" [] [
    Dream.get "/js/app.bc.js" @@ (fun _req ->
      Dream.respond
        ~headers:["Content-Type", "text/javascript"]
        [%blob "./js/app.bc.js"]
    );
    Dream.get "/styles/style.css" @@ (fun _req ->
      Dream.respond
        ~headers:["Content-Type", "text/css"]
        [%blob "./styles/style.css"]
    );
  ]
