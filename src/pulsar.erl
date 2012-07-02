-module(pulsar).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

load_dependencies(App) ->
    case application:get_application(App) of
        undefined ->
            application:load(App);
        {ok, App} ->
            ok
    end,

    case application:get_key(App, applications) of
        {ok, Applications} ->
            [ensure_started(A) || A <- Applications],
            ok;
        undefined ->
            {not_started, App}
    end.

start() ->
    load_dependencies(pulsar),
    application:start(pulsar).

stop() ->
    application:stop(pulsar).
