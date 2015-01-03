-module(dalmatiner_frontend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    TelnetListeners = case application:get_env(dalmatiner_frontend, telnet_listeners) of
                          {ok, TL} ->
                              TL;
                          _ ->
                              2
                      end,
    HTTPListeners = case application:get_env(dalmatiner_frontend, http_listeners) of
                          {ok, HTTPL} ->
                              HTTPL;
                          _ ->
                              2
                      end,
    case application:get_env(dalmatiner_frontend, telnet_port) of
        {ok, TelnetPort} ->
            {ok, _} = ranch:start_listener(dalmatiner_telnet, TelnetListeners,
                                           ranch_tcp, [{port, TelnetPort}],
                                           dalmatiner_telnet, []);

        _ ->
            ok
    end,
    Dispatch = cowboy_router:compile(
                 [
                  %% {URIHost, list({URIPath, Handler, Opts})}
                  {'_', [{"/", dalmatiner_idx_handler, []},
                         {"/buckets/", dalmatiner_bucket_handler, []},
                         {"/buckets/[:bucket]", dalmatiner_metric_handler, []},
                         {"/js/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/js",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/fonts/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/fonts",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/css/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/css",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/img/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/img",
                           [{mimetypes, cow_mimetypes, web}]}},
                         {"/static/[...]", cowboy_static,
                          {priv_dir, dalmatiner_frontend, "static/",
                           [{mimetypes, cow_mimetypes, web}]}}]}
                 ]),
    case application:get_env(dalmatiner_frontend, http_port) of
        {ok, HTTPPort} ->
            %% Name, NbAcceptors, TransOpts, ProtoOpts
            cowboy:start_http(dalmatiner_http_listener, HTTPListeners,
                              [{port, HTTPPort}],
                              [{env, [{dispatch, Dispatch}]}]
                             );
        _ ->
            ok
    end,
    dalmatiner_frontend_sup:start_link().

stop(_State) ->
    ok.
