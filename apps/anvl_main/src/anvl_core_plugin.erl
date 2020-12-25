-module(anvl_core_plugin).

-behavior(anvl_plugin).

-export([model/0, project_model/0, root_targets/0]).

-include("anvl_int.hrl").

%%%===================================================================
%%% anvl_plugin callbacks
%%%===================================================================

-spec model() -> lee:module().
model() ->
  #{ '$doc_root' =>
       {[doc_root],
        #{ oneliner => "A new build system for Erlang (WIP)"
         , app_name => "ANVL"
         , doc => anvl_description()
         , prog_name => "anvl"
         }}
   , root_dir =>
       {[value, cli_param],
        #{ oneliner => "Root directory of the project"
         , type => string()
         , default => "."
         , cli_operand => "dir"
         , cli_short => $D
         }}
   , build_dir =>
       {[value, mustache, cli_param],
        #{ oneliner => "Directory where build artifacts are stored"
         , type     => string()
         , default  => "{{cfg: [root_dir]}}/_abuild/{{cfg: [profile]}}"
         , cli_operand => "build-dir"
         }}
   , cache_dir =>
       {[value, mustache, os_env],
        #{ oneliner => "Directory where global caches are located"
         , type => string()
         , default => filename:basedir(user_cache, "anvl")
         , os_env => "ANVL_CACHE_DIR"
         , doc_remark => "Default value is platform-dependent."
         }}
   , always_make =>
       {[value, cli_param],
        #{ oneliner => "Unconditionally make all targets"
         , type => boolean()
         , default => false
         , cli_operand => "always-make"
         , cli_short => $B
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
       #{ oneliner => "Verbosity of console output"
        , type => anvl:log_level()
        , default => notice
        , cli_operand => "verbosity"
        , cli_short => $v
        }}
   , profile =>
       {[value, cli_param],
        #{ oneliner => "Build profile"
         , type => anvl:profile()
         , default => default
         , cli_operand => "profile"
         , cli_short => $p
         }}
     %% Actions:
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
         , cli_short => $h
         }}
   , hacking => hacking()
   }.

-spec project_model() -> lee:module().
project_model() ->
  #{ checkouts_dir =>
       {[value, mustache, anvl],
        #{ oneliner => "Directory where checkouts are located"
         , type => string()
         , default => "_checkouts"
         , file_key => checkouts_dir
         }}
   , app_dirs =>
       {[value, mustache, anvl, rebar],
        #{ oneliner => "Directories where project applications are located"
         , type => list(string())
         , default => ["apps/*", "lib/*", "."]
         , file_key => project_app_dirs
         }}
   , plugins =>
       {[value, anvl],
       #{ oneliner => "List of anvl plugins"
         , type => list(anvl:plugin())
         , default => []
         , file_key => anvl_plugins
         , doc_remark => "Anvl plugins are incompatible with the rebar3 providers"
         }}
   , overrides =>
       {[value, anvl, rebar],
        #{ oneliner => "TODO: not implemented"
         , type => anvl:overrides()
         , default => []
         , file_key => overrides
         }}
   , default_targets =>
       {[value, anvl],
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

-spec hacking() -> lee_model:module().
hacking() ->
  #{ model =>
       {[map, cli_action],
        #{ oneliner => "Commands for anvl developers (debug configuration model)"
         , cli_operand => "anvl_model"
         },
        #{ dump =>
             {[value, cli_param],
              #{ oneliner => "Dump configuration model to stdout"
               , cli_operand => "dump"
               , cli_short => $D
               , type => boolean()
               , default => false
               }}
         }}
   , docs =>
       {[map, cli_action],
        #{ oneliner => "Commands for anvl developers (generate documentation)"
         , cli_operand => "anvl_mkdoc"
         },
        #{ out_dir =>
             {[value, cli_param],
              #{ oneliner => "Output directory (TODO: unused)"
               , cli_operand => "out"
               , cli_short => $o
               , type => string()
               , default => "anvl_doc"
               }}
         }}
   , config =>
       {[map, cli_action],
        #{ oneliner => "Commands for anvl developers (debug configuration)"
         , cli_operand => "anvl_cfg"
         },
        #{ dump =>
             {[value, cli_param],
              #{ oneliner => "Dump configuration model to stdout"
               , cli_operand => "dump"
               , cli_short => $D
               , type => boolean()
               , default => false
               }}
         , overlay =>
             {[value, cli_param],
              #{ oneliner => "Configuration overlay"
               , cli_operand => "overlay"
               , cli_short => $o
               , type => anvl:package_id()
               , default => ?root_project
               }}
         , key =>
             {[value, cli_param],
              #{ oneliner => "Configuration key"
               , cli_operand => "key"
               , cli_short => $k
               , type => union(undefined, list())
               , default => undefined
               }}
         }}
   }.

anvl_description() ->
"<para>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce
convallis nibh vel consectetur consequat. Nunc vitae ex eu lorem
vulputate bibendum vel quis nibh. Maecenas vulputate purus a tincidunt
bibendum. Proin scelerisque ligula elementum leo auctor, et accumsan
velit pellentesque. Orci varius natoque penatibus et magnis dis
parturient montes, nascetur ridiculus mus. Proin convallis rutrum
lorem, ut facilisis lectus consectetur sed. Cras in ullamcorper
eros. Praesent quis dui quis arcu condimentum viverra quis dapibus
ex. Integer ultricies laoreet nulla, vitae mollis sem tristique
a. Maecenas in turpis eu odio sodales pellentesque vitae sed sem. Cras
in ullamcorper diam.</para>

<para>Aliquam auctor sed ante eu pellentesque. Suspendisse tincidunt
elit sit amet fringilla sollicitudin. Quisque finibus metus quis ipsum
semper, in consectetur justo tincidunt. Nunc lacinia lorem vitae
condimentum consequat. Nam egestas ut mi vel porttitor. Proin tempus
magna quis nulla feugiat, et blandit massa tristique. Donec ut eros
massa. Cras ac gravida purus, ut rhoncus tellus. Praesent nec eros
pellentesque, varius nisl vitae, auctor magna.</para>

<para>Etiam varius nisi ut aliquam tempus. Sed tempor, magna id
blandit egestas, augue erat sollicitudin sem, ac condimentum lorem
eros ut ante. Ut odio risus, mattis sed vulputate nec, semper ac
nulla. Aliquam mollis tincidunt mi quis volutpat. Lorem ipsum dolor
sit amet, consectetur adipiscing elit. Etiam nisi odio, auctor quis
sem quis, egestas placerat ipsum. Vivamus est orci, tempor cursus
feugiat vel, fermentum et est. Etiam non nunc ac ex viverra aliquet ac
et lectus. Aliquam sodales fermentum odio, in tincidunt elit aliquam
id. Etiam id neque sed felis scelerisque vestibulum. Quisque non magna
justo. Orci varius natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Nulla convallis dapibus imperdiet. Nam
vehicula pharetra ipsum in laoreet.<xref linkend=\"[foo]\"/></para>

<para>Donec erat massa, molestie et auctor vel, commodo vel orci. Nam
sollicitudin sit amet ligula vitae bibendum. Nam sed sem vehicula,
auctor velit ac, euismod metus. Mauris ultrices quam non massa
sagittis, ut rutrum quam interdum. <emphasis>Aenean varius rhoncus
turpis nec convallis.</emphasis> Duis sodales sodales
tempus. Vestibulum id ante sodales justo ornare sagittis. Nunc vitae
ornare diam, nec mattis mi.</para>

<para>Sed nunc justo, dignissim quis aliquam non, suscipit et
orci. Curabitur mollis magna ac nunc mattis, sed condimentum felis
iaculis. Mauris sollicitudin, nisl nec auctor rhoncus, ipsum lacus
finibus leo, efficitur facilisis ex nisi vel ipsum. Sed tempor ornare
efficitur. Aliquam erat volutpat. Nulla sit amet euismod turpis. Nunc
rutrum ex sit amet ligula consequat, a lobortis purus
dignissim. Curabitur dolor ante, mollis at malesuada semper, semper
quis arcu. Curabitur in purus fermentum, suscipit diam eleifend,
dictum augue. Pellentesque bibendum nisi sit amet aliquet
accumsan. Donec non justo in ex vehicula posuere vel ut purus. Aenean
vestibulum nulla at ante ultricies bibendum. Aliquam erat
volutpat. Integer pellentesque sit amet felis viverra
imperdiet. Pellentesque dapibus ipsum in magna ultrices
consectetur. Suspendisse non est ex.</para>".
