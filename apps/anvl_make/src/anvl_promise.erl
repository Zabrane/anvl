-module(anvl_promise).

-export([main/0, foo/0, bar/0]).

-type promise(A) :: {reference(), fun((A) -> A)}.

-spec want(promise(A)) -> A.
want({Ref, Id}) ->
  receive
    {Ref, A} -> Id(A)
  end.

-spec foo() -> promise(integer()).
foo() ->
  {make_ref(), fun id/1}.

-spec bar() -> promise(atom()).
bar() ->
  {make_ref(), fun id/1}.

-spec id(A) -> A.
id(A) ->
  A.

main() ->
  atom_to_list(want(bar())).
