-module(cli_test).

-export([gen_docs_test/0]).

-include_lib("hut/include/hut.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(BINARY, "_build/default/bin/anvl").

%%%===================================================================
%%% Testcases
%%%===================================================================

gen_docs_test() ->
  ?assertMatch(0, exec(?BINARY " @anvl_mkdoc")).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Execute an external executable `Executable' with args `Args'
%% and return the exit status
-spec exec(file:filename()) -> integer().
exec(CMD) ->
  Port = open_port( {spawn, CMD}
                  , [ exit_status
                    , binary
                    , stderr_to_stdout
                    ]
                  ),
  ?log(debug, "port_command ~p: ~s ~p", [Port, CMD]),
  collect_port_output(Port).

-spec collect_port_output(port()) -> integer().
collect_port_output(Port) ->
  %% TODO: outputs of commands running in parallel may get mixed
  %% together, do something about this.
  receive
    {Port, {data, Data}} ->
      ?log(notice, "Port=~p~n~s", [Port, Data]),
      collect_port_output(Port);
    {Port, {exit_status, ExitStatus}} ->
      ExitStatus
  end.
