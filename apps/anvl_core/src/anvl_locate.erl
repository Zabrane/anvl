-module(anvl_locate).

% API:
-export([locate_app/1]).

% Internal exports:
-export([do_locate_app/1, do_locate/0]).

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

-spec locate_app(anvl:app_id()) -> #app_location{}.
locate_app(App) ->
  anvl_make:want({?MODULE, do_locate_app, [App]}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-record(package,
        { id       :: anvl:package_id()
        , apps     :: [anvl:app_id()]
        , location :: file:name()
        }).

-spec do_locate_app(anvl:app_id()) -> #app_location{}.
do_locate_app(App) ->
  Projects = anvl_make:want({?MODULE, do_locate, []}),
  error(1).

-spec do_locate() -> [#package{}].
do_locate() ->
  ?log(notice, "Resolving dependencies..."),
  [current_project()].

-spec current_project() -> #package{}.
current_project() ->
  RootDir = ?cfg([root_dir]),
  Apps = app_list(?root_project, RootDir),
  #package{ id       = ?root_project
          , apps     = Apps
          , location = RootDir
          }.

-spec app_list(anvl:package_id(), file:name()) -> [anvl:app_id()].
app_list(PackageId, Location) ->
  AppDirs = ?proj_cfg(PackageId, [dirs, apps]),
  SrcDirs = ?proj_cfg(PackageId, [dirs, src]),
  AppSrcs = lists:append(
              [filelib:wildcard(filename:join([App, Src, "*.app.src"]))
               || App <- AppDirs, Src <- SrcDirs]),
  Apps = [list_to_atom(filename:basename(Path, ".app.src")) || Path <- AppSrcs],
  ?slog(debug, #{ ?what      => "Located applications in the project"
                , package_id => PackageId
                , apps       => Apps
                , location   => Location
                }),
  Apps.
