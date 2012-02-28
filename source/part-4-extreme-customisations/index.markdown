---
layout: page
title: "part 4 - extreme customisations"
date: 2012-02-28 08:32
comments: true
sharing: true
footer: true
---
So far we've looked at the structure of rebar plugins, concentrating on the
common use cases: adding a new command and/or hooking into existing ones. In
this instalment, we will take a view of some of the more extreme kinds of
customisation we can achieve.

We will focuss exclusively on an existing plugin, so there are no specific
sources to download apart from the additional libraries, links to which will
be provided both in the text, and in the final *links* section.

## More Caveats Than Usual

Today's whirlwind tour will take in two plugins that have become all but
quintessential for many of my own projects. It must be noted that these 
plugins are based on *internal* rebar functionality that could change at any
time! As rebar has no official API for plugins to rely on, this is always the
case right now anyway. Nonetheless, if you build plugins using the kind of
*inner guts* API calls that we will use today, you should be aware that future
rebar releases may well cause you some rework headaches.

## Introducing the *Build Lifecycle*

Rebar's usage is predicated on a simple model of user issued *commands*. The
user may offer one or more commands when calling rebar, and they will be
processed in the order they're specified on the command line. This model is
simple to understand and to implement, which has no doubt been at least part
of the reason for the tool's wide adoption and the community's seemingly
voracious appetite for submitting patches and additional features.

If you're familiar with other build tools (Make, Rake, Maven, etc) then you
may be used to one of the two other prevalent models:

1. Dependencies between tasks
2. Build Phases/LifeCycles

In the latter case, Maven defines (via plugins or core components) various
*goals* that can be executed - these are pretty much equivalent to rebar's
concept of commands. Maven ties these *goals* to specific *lifecycle phases*
in such a way that the order in which a project is processed is always well
defined.

Tools like Make and Rake allow you to make certain tasks/commands dependant on
one another, so we can do things like:

```make
all: clean test
clean:
    @(rebar clean)
compile:
    @(rebar compile)
test: compile
    @(rebar eunit qc ct)
```

What we are going to do today, is put both these ideas together into a rebar
plugin, so that we can define build *stages*, attach rebar commands to them
__and__ create dependencies between one or more *stages* as well. We will aim
to do this with the following

- a plugin that works with a stock rebar (master branch of basho/rebar)
- some (local) configuration in the project that sets up the 'lifecycle'

Understanding how this works will be our task for today.

-------

The implementation code for today's article is already written and you can
access it from github [here][rebar_phase_plugin], using git or via the
downloads page.

-------

## From Configuration to Commands

Our 'lifecycle' concept is going to require adding new lifecycle phases as
custom rebar commands. What we don't want to do however, is dictate what those
phases should be called or which (existing) rebar commands are bound to them,
nor should we dictate the order in which the lifecycle phases are executed
(viz. the dependencies between them).

How then, can we go from user defined configuration settings to new commands?
The answer to this is rather subversive: we will generate new functions on the
fly and make them available to rebar!

For those who've joined the series late, let's go over how rebar maps a
command to an implementation module/function at runtime. For any given
command, rebar will understand the command if (and only if)
*one of the modules it knows about* exports a function with a signature that:

- has the same name as the command and is of arity 2
- has the same name as the command but is prefixed with pre_ and is of arity 2
- has the same name as the command but is prefixed with post_ and is of arity 2

There are essentially two ways that rebar knows about modules:

  - From the rebar.app configuration file
  - Via the plugins section of the build configuration

So for each 'phase' that the user defines in their configuration, we will need
a module that exports a function with the *same name as the phase*. Clearly
as our code is not 'psychic' we are going to have to generate these at
runtime.

## Introducing *rebar_plugin_manager*

Writing code to generate a function at runtime is surprisingly simple in
Erlang, however we don't have to do this because it has already been done for
us. There are a variety of tools (both in Erlang/OTP and provided by the
open source community) that do this, but we can do one better.

The [rebar_plugin_manager][rebar_plugin_manager] project provides a few useful
APIs for working with plugins, one of which is `generate_handler/3`. As the
name suggests, this code generates a command handling function for us, saving
us from doing any of the hard work. Not only is it useful to avoid reinventing
the wheel here, but the implementation provided also deals with various edge
cases such as when we're being run from within an escript and/or being called
by code that might not be.

The API for `generate_handler/3` looks like this:

```erlang
-type command_name() :: atom().
-type command_data() :: any().
-type instruction_set() :: list(command_data()).
-type command() :: {'command',
                    command_name(),
                    command_data(),
                    instruction_set()}.

%% and later on....

-spec generate_handler(BaseName::string(), 
                       Cmds::list(rebar_plugin_manager:command()),
                       Origin::module()) -> module().
```

The function takes a *base name* (which reflects the directory base name in
which the command is to be run), a list of 'command tuples' and the
originating module. Although it isn't abundantly clear from the type spec, the
return value of this function is the module to which the new command handling
functions have been added.

Now there is a requirement that the originating module (i.e., the caller)
should export a function with the following signature:

```erlang
-spec execute_command(Command::atom(),
                      BaseName::string(),
                      Config::rebar_config:config(), 
                      AppFile::string()) -> 'ok' | {error, any()}.
```

This works a little bit like an OTP behaviour callback. When one of the newly
exported commands is given to rebar, it will call the implementation that has
been generated for us by the plugin manager, which could reside in a number
of different places - this is implementation specific and may change at any
time! The generated function will in turn call our *originator* back, passing
the command name and base name it was originally given, the current rebar
config record and the application resource file currently in scope.

The function is expected to return 'ok' for success, or `{error, Reason}` in
the event that something goes wrong.

## Utilising the `preprocess/2` hook

If we want our command(s) to get generated 'in time' for rebar to recognise
them, then we need to start using the `preprocess/2` hook. If exported, this
function is called in much the same way as *command-functions*, but has
slightly different semantics.

In the current incarnation of rebar, unlike standard command handling
functions, the return value of `preprocess/2` is expected to be either
`{ok, Predirs::list(string())}` or an error tuple. In the success case, the
second element is used to indicate to rebar that there are directories which require processing
*before* this one - this is the mechanism that rebar's `sub_dirs` and `deps`
functionality uses. It is also possible that this approach (to handling both
sub-directory processing and pre-processing of all commands) may change in
the future. Whilst writing custom commands and/or command hooks is fairly well
supported now (despite the lack of an official API), using the `preprocess/2` hook
is probably a bit more volatile and should be undertaken with this in mind.

Now all the whining caveats are out of the way, let's look at how our own
implementation of `preprocess/2` will look inside of the 
[rebar_phase_plugin][rebar_phase_plugin]:

```erlang
-spec preprocess(rebar_config:config(), string()) -> {'ok', list()}.
preprocess(Config, _) ->
    case is_basedir() of
        true ->
            case commands(Config) of
                [] ->
                    ok;
                Cmds ->
                    rebar_plugin_manager:generate_handler(basename(),
                                                            Cmds, ?MODULE)
            end;
        false ->
            ok
    end,
    {ok, []}.
```

As you can see, we're not returning any pre-dirs, but are simply using this
hook for its side effects - this is the reason I suspect the semantics of
`preprocess/2` could possibly change some time in the future - and our code
should look fairly familiar if you've been following the series.

Firstly we exclude any processing unless we're running in the `base_dir`.
Although we've previously looked at an annotations based solution for this
selective processing rule, it won't apply cleanly to `preprocess/2` so we're 
going to leave it out and focus on delivering the functionality first.

The code's working is pretty obvious - we collect a list of 'commands' and if
it is non-empty then we call `rebar_plugin_manager` and ask it to do the work
of generating handlers for us. Collecting the commands is a call to our old
friend `rebar_config`:

```erlang
commands(Config) ->
    [ generate_command(Phase) || Phase <- load_phases(Config) ].

load_phases(Config) ->
    case rebar_config:get_global({phases, Config}, undefined) of
        undefined ->
            case rebar_config:get_local(Config, phases, []) of
                [] ->
                    rebar_config:get(Config, phases, []);
                Defs ->
                    Defs
            end;
        Phases ->
            Phases
    end.

generate_command({PhaseName, PhaseCommands}) ->
    generate_command({PhaseName, [], PhaseCommands});
generate_command({PhaseName, PhaseDepends, PhaseCommands}) ->
    {command, PhaseName, PhaseDepends, PhaseCommands}.
```

As we can see, the real work of glueing the lifecycle together is done by the
user in either their global or local configuration files.

## Working with rebar_plugin_manager's execute_command/4 callback

With all these bits of *infrastructure* in place, we can now look at how the
[phase plugin][rebar_phase_plugin] will implement the actual processing. Here
we will get to see just how badly we're abusing rebar's internals in order to
achieve our goals.

When rebar is first called, the outer module that handles the escript entry 
point - `rebar:main/1` - does some initial setup work and then delegates off
to `rebar_core:process_commands/2`, passing the list of given commands and the
initial rebar config set. This latter function is where the code that drives
rebar really lives, and this is clearly meant for internal consumption.

Nonetheless, we are going to call this function ourselves, passing a list of
command atoms that we wish to be processed during each stage. We will also
implement our 'dependency' between phases here, with a bit of simple
recursion.

```erlang
-spec execute_command(atom(), string(),
                      rebar_config:config(),
                      string()) -> 'ok' | {error, any()}.
execute_command(Command, Root, Config, AppFile) ->
    case [ Err || Err <- lists:map(
        fun({command, _, PhaseDepends, PhaseCommands}) ->
            case PhaseDepends of
                [] ->
                    ok;
                Deps when is_list(Deps) ->
                    [ execute_command(C, Root,
                                    Config, AppFile) || C <- Deps ];
                Dep when is_atom(Dep) ->
                    execute_command(Dep, Root, Config, AppFile)
            end,
            rebar_log:log(info, "Processing phases ~p~n", [PhaseCommands]),
            rebar_core:process_commands(PhaseCommands, Config);
         (C) when is_atom(C) ->
             rebar_log:log(info, "Processing phase ~p~n", [C]),
             rebar_core:process_commands([C], Config)
        end, lookup_phase_config(Command, Config)), Err /= ok ] of
        [] ->
            ok;
        Other ->
            {error, Other}
    end.

%%
%% Internal API
%%

lookup_phase_config(Command, Config) ->
    [ C || {command, Cmd, _, _}=C <- commands(Config), Cmd == Command ].
```

And minus the few little utility functions (like basedir and basename) that
are required during `preprocess/2`, this is all our plugin needs to do! Once
`rebar_core:process_commands/2` kicks in, everything simply works as expected.

The user's configuration will need to look something like this:

```erlang
%% define the build lifecycle
{phases, [
    {build,              [],    ['get-deps', compile]},
    {test,               build, [eunit, quickcheck]},
    {'integration-test', test,  [ct]},
    {package,            test,  [dist]}
]}.
```

There is an example project in the [rebar_phase_plugin's][rebar_phase_plugin]
source tree that demonstrates this. Here's a quick look at a session:

    t4@malachi:integration-test $ rebar integration-test -v
    ==> integration-test (integration-test)
    ==> integration-test (eunit)
    ======================== EUnit ========================
    module 'foo_app'
    module 'foo_mod1'
      module 'foo_mod1_tests'
    module 'foo_sup'
      There were no tests to run.
    ==> integration-test (ct)

    Common Test v1.6 starting (cwd is /Users/t4/work/hyperthunk/rebar_phase_plugin/examples/integration-test)
    Common Test: Running make in test directories...

    CWD set to: "/Users/t4/work/hyperthunk/rebar_phase_plugin/examples/integration-test/logs/ct_run.test@malachi.local.2012-02-28_15.18.17"

    TEST INFO: 1 test(s), 0 case(s) in 0 suite(s)

    Testing examples.integration-test: Starting test, 0 test cases
    Testing examples.integration-test: TEST COMPLETE, 0 ok, 0 failed of 0 test cases

    Updating /Users/t4/work/hyperthunk/rebar_phase_plugin/examples/integration-test/logs/index.html... done
    Updating /Users/t4/work/hyperthunk/rebar_phase_plugin/examples/integration-test/logs/all_runs.html... done

    DONE.
    Testing examples.integration-test: TEST COMPLETE, 0 ok, 0 failed of 0 test cases

    t4@malachi:integration-test $ 
    

There are no actual tests in the example project, but the point is to illustrate
how the [phase plugin][rebar_phase_plugin] works in terms of running multiple
commands and their dependant phases when required. Here's the rebar.config we
use in the example project:

```erlang
{deps, [
    {rebar_phase_plugin, ".*",
        {git, "../../../", "master"}},
    {rebar_skip_deps, ".*",
        {git, "git://github.com/hyperthunk/rebar_skip_deps.git"}}
]}.

{skip_dep_cmds, ['integration-test', eunit, qc, ct]}.

{phases, [
    {test, ['check-deps', compile], [eunit]},
    {'integration-test', test, [ct]}
]}.

{plugins, [rebar_plugin_manager, rebar_phase_plugin, rebar_skip_deps]}.
{plugin_dir, "deps/rebar_plugin_manager/src"}.
```

## Next time...

Next time we will continue looking at the rebar phase plugin, this time with a
focus on testing rebar plugins and shipping common configuration data.

## Links

As promised, all the relevant links for this article are listed below.

1. [rebar phase plugin][rebar_phase_plugin]
2. [rebar plugin manager][rebar_plugin_manager]

[rebar_phase_plugin]: https://github.com/hyperthunk/rebar_phase_plugin
[rebar_plugin_manager]: https://github.com/hyperthunk/rebar_plugin_manager
