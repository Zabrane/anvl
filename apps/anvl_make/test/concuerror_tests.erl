-module(concuerror_tests).

-include_lib("snabbkaffe/include/snabbkaffe.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([dependency_test/0]).

dependency_test() ->
  %% Tasks:
  Foo3 = foo(3, []),
  Foo2 = foo(2, [Foo3]),
  Bar1 = bar(1),
  Foo1 = foo(1, [Foo2, Bar1]),
  Deps = [ {Foo2, Foo3}
         , {Foo1, Foo2}
         , {Foo1, Bar1}
         ],
  ?check_trace(
     #{timeout => 0},
     %% Run stage:
     begin
       anvl_make:start_link(#{ foo => fun mk_foo/1
                             , bar => fun mk_bar/1
                             }),
       anvl_make:want(Foo1),
       anvl_make:want(Foo3),
       ok
     end,
     %% Check stage:
     fun(_Ret, Trace) ->
         anvl_trace_specs:all_tasks_complete(Trace),
         anvl_trace_specs:all_tasks_ran_once(Trace),
         anvl_trace_specs:tasks_started([Foo1, Foo2, Foo3, Bar1], Trace)
     end).

%% Targets:
foo(Id, Deps) ->
  {foo, {Id, Deps}}.

bar(Id) ->
  {bar, Id}.

%% Providers:
mk_foo({Id, Deps}) ->
  [anvl_make:want(I) || I <- Deps],
  ?tp(make_target, #{kind => foo, id => Id, deps => Deps}).

mk_bar(Id) ->
  ?tp(make_target, #{kind => bar, id => Id, deps => []}).
