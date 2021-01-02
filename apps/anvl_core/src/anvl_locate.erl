-module(anvl_locate).

% API:
-export([prepare_src/1]).

% Internal exports:
-export([do_prepare_src/1, do_prepare_all/0]).

-include("anvl.hrl").


%%%===================================================================
%%% Types
%%%===================================================================

-type dependency() :: anvl:app_id()
                    | {anvl:app_id(), fetch_spec()}.

-type fetch_spec() :: hex_fetch_spec()
                    | git_fetch_spec()
                    | hg_fetch_spec().

-type hex_fetch_spec() :: string()
                        | {pkg, atom()}.

-type git_fetch_spec() :: {git, string()}
                        | {git | git_subdir, string(), git_tag()}.

-type git_tag() :: {tag | branch | ref, string()}.

-type hg_fetch_spec() :: {hg, string()}.

-reflect_type([dependency/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec prepare_src(anvl:app_id()) -> #app{}.
prepare_src(App) ->
  anvl_make:want({?MODULE, do_prepare_src, [App]}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_prepare_src(anvl:app_id()) -> #app{}.
do_prepare_src(App) ->
  Projects = anvl_make:want({?MODULE, do_prepare_all, []}),
  error(1).

-spec do_prepare_all() -> [#package{}].
do_prepare_all() ->
  ?log(notice, "Resolving dependencies..."),
  [current_project()].

-spec current_project() -> #package{}.
current_project() ->
  RootDir = ?cfg([root_dir]),
  Apps = app_list(?root_project, RootDir),
  #package{ id       = ?root_project
          , apps     = Apps
          , location = RootDir
          , volatile = true
          }.

%% @private List all applications provided by the package
-spec app_list(anvl:package_id(), file:name()) -> [#app{}].
app_list(PackageId, Location) ->
  AppDirs = lists:flatmap( fun(Pattern) ->
                               filelib:wildcard(Pattern, Location)
                           end
                         , ?proj_cfg(PackageId, [dirs, apps])
                         ),
  lists:flatmap( fun(Dir) ->
                     scan_app_dir(PackageId, filename:join(Location, Dir))
                 end
               , AppDirs
               ).

%% @private Try to find name of the OTP application located in the
%% directory. Ignore directories that don't contain any apps.
-spec scan_app_dir(anvl:package_id(), file:name()) -> [#app{}].
scan_app_dir(PackageId, Location) ->
  SrcDirs = ?proj_cfg(PackageId, [dirs, src]),
  Fun = fun(Src) ->
            filelib:wildcard(filename:join([Location, Src, "*.app.src"]))
        end,
  case lists:flatmap(Fun, SrcDirs) of
    [Path] ->
      [#app{ id         = list_to_atom(filename:basename(Path, ".app.src"))
           , location   = Location
           , package_id = PackageId
           }];
    [] ->
      [];
    _ ->
      anvl:panic( "Bad application. "
                  "~p directory contains multiple .app.src files, but only one is expected."
                , [Location]
                )
  end.

%% -spec app_list(anvl:package_id(), file:name()) -> [#app{}].
%% app_list(PackageId, Location) ->
%%   AppDirs = ?proj_cfg(PackageId, [dirs, apps]),
%%   SrcDirs = ?proj_cfg(PackageId, [dirs, src]),
%%   AppSrcs = lists:append(
%%               [filelib:wildcard(filename:join([Location, App, Src, "*.app.src"]))
%%                || App <- AppDirs, Src <- SrcDirs]),
%%   [list_to_atom(filename:basename(Path, ".app.src")) || Path <- AppSrcs].

%% -spec transfer_package_apps(#package{}) -> ok.
%% transfer_package_apps(#package{id = Id, apps = Apps, location = Loc}) ->
