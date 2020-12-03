-module(anvl_core_plugin).

-behavior(anvl_plugin).

-export([model/0, project_model/0, root_targets/0]).

-include("anvl_int.hrl").

%%%===================================================================
%%% anvl_plugin callbacks
%%%===================================================================

-spec model() -> lee:module().
model() ->
  #{ cache_dir =>
       {[value, mustache, os_env],
        #{ oneliner => "Directory where global caches are located"
         , type => string()
         , default => filename:basedir(user_cache, "anvl")
         , os_env => "ANVL_CACHE_DIR"
         , doc_remark => "Default value is platform-dependent."
         }}
   %% , parallel_tasks =>
   %%     {[value, cli_param],
   %%      #{ oneliner => "Limit the number of parallel jobs"
   %%       , type => non_neg_integer()
   %%       , default => 0
   %%       , cli_short => "j"
   %%       , doc_remark => "0 denotes unlimited"
   %%       }}
   %% , keep_going =>
   %%     {[value, cli_param],
   %%      #{ oneliner => "Keep scheduling new tasks after failure is detected"
   %%       , type => boolean()
   %%       , default => false
   %%       , cli_operand => "keep-going"
   %%       , cli_short => "K"
   %%       }}
   , always_make =>
       {[value, cli_param],
        #{ oneliner => "Unconditionally make all targets"
         , type => boolean()
         , default => false
         , cli_operand => "always-make"
         , cli_short => "B"
         }}
   %% , show_top =>
   %%     {[value, cli_param],
   %%      #{ oneliner => "Show statistics about execution time of different tasks"
   %%       , type => boolean()
   %%       , default => false
   %%       , cli_operand => "top"
   %%       }}
   %% , show_depgraph =>
   %%     {[value, cli_param],
   %%      #{ oneliner => "Generate dependency graph in dot format"
   %%       , type => boolean()
   %%       , default => false
   %%       , cli_operand => "depgraph"
   %%       }}
   , verbosity =>
       {[value, cli_param],
       #{ onliner => "Verbosity of console output"
        , type => anvl:log_level()
        , default => notice
        , cli_short => "v"
        , cli_operand => "verbosity"
        }}
   , get_version =>
       {[value, cli_param],
        #{ oneliner => "Print version and exit"
         , type => boolean()
         , default => false
         , cli_operand => "version"
         }}
   , get_help =>
       {[value, cli_param],
        #{ oneliner => "Get help about a command and exit"
         , type => atom()
         , default => undefined
         , cli_operand => "help"
         , cli_short => "h"
         }}
   , profile =>
       {[value, cli_param],
        #{ oneliner => "Build profile"
         , type => anvl:profile()
         , default => default
         , cli_operand => "profile"
         , cli_short => "p"
         }}
   }.

-spec project_model() -> lee:module().
project_model() ->
  ProjectCfgModel0 =
    #{ base_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where build artifacts are stored"
           , type     => string()
           , default  => "_abuild/{{cfg: [profile]}}"
           , file_key => base_dir
           }}
     , root_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where project files are located"
           , type     => string()
           , default  => "."
           , file_key => root_dir
           }}
     , checkouts_dir =>
         {[value, mustache],
          #{ oneliner => "Directory where checkouts are located"
           , type => string()
           , default => "_checkouts"
           , file_key => checkouts_dir
           }}
     , app_dirs =>
         {[value, mustache],
          #{ oneliner => "Directories where project applications are located"
           , type => list(string())
           , default => ["apps/*", "lib/*", "."]
           , file_key => project_app_dirs
           }}
     , plugins =>
         {[value],
         #{ oneliner => "List of anvl plugins"
           , type => list(anvl:plugin())
           , default => []
           , file_key => anvl_plugins
           , doc_remark => "Anvl plugins are incompatible with the rebar3 providers"
           }}
     , overrides =>
         {[value],
          #{ oneliner => "TODO: not implemented"
           , type => anvl:overrides()
           , default => []
           , file_key => overrides
           }}
     , default_targets =>
         {[value],
          #{ oneliner => "List of default targets that will be executed when no target is given"
           , type => list({atom(), atom(), list()})
           , default => [ {anvl_compile, compile, []}
                        , {anvl_dialyzer, dialyze, []}
                        ]
           , file_key => default_targets
           }}
     %% , package_id =>
     %%     {[value],
     %%      #{ oneliner => "ID of the project (set automatically)"
     %%       , type => package_id()
     %%       , undocumented => true
     %%       }}
     }.

-spec root_targets() -> [anvl_make:target()].
root_targets() ->
  [].

%%%===================================================================
%%% Internal functions
%%%===================================================================
