-module(cli_test).

-export([gen_docs_test/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(BINARY, "_build/default/bin/anvl").

%%%===================================================================
%%% Testcases
%%%===================================================================

gen_docs_test() ->
  ?assertMatch(0, exec("@anvl_mkdoc -o docs")).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Execute an external executable `Executable' with args `Args'
%% and return the exit status

-spec exec(string()) -> integer().
exec(Args) ->
  CMD = ?BINARY ++ [$ |Args],
  Port = open_port( {spawn, CMD}
                  , [ exit_status
                    , binary
                    , stderr_to_stdout
                    ]
                  ),
  ?LOG(debug, "port_command ~p: ~s ~p", [Port, CMD]),
  collect_port_output(Port).

-spec collect_port_output(port()) -> integer().
collect_port_output(Port) ->
  %% TODO: outputs of commands running in parallel may get mixed
  %% together, do something about this.
  receive
    {Port, {data, Data}} ->
      collect_port_output(Port);
    {Port, {exit_status, ExitStatus}} ->
      ExitStatus
  end.
