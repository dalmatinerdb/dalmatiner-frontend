-module(dalmatiner_frontend_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Listeners} = application:get_env(dalmatiner_frontend, http_listeners),
    {ok, Port} =  application:get_env(dalmatiner_frontend, http_port),

    Dispatch = cowboy_router:compile(
                 [
                  %% {URIHost, list({URIPath, Handler, Opts})}
                  {'_', [{"/", dalmatiner_idx_handler, []},
                         %% Old style API
                         {"/buckets/", dalmatiner_bucket_handler, []},
                         {"/buckets/[...]", dalmatiner_metric_handler, []},

                         %% New style API
                         %% List all collections
                         {"/collections", dalmatiner_collection_h, []},
                         %% List all metrics in a collection
                         {"/collections/:collection/metrics/",
                          dalmatiner_metric_h, []},
                         %% List all namespaces
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/",
                          dalmatiner_namespace_h, []},
                         %% List all tags in a namespace
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/:namespace/tags/",
                          dalmatiner_tag_h, []},
                         %% List all values in a tag
                         {"/collections/:collection/metrics/"
                          ":metric/namespaces/:namespace/tags/"
                          ":tag/values",
                          dalmatiner_value_h, []},

                         %% STatic content.
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
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(dalmatiner_http_listener, Listeners,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    dalmatiner_frontend_sup:start_link().

stop(_State) ->
    ok.
