---
layout: page
title: "Part 2 - Plugin Anatomy"
date: 2012-01-04 10:12
comments: true
sharing: true
footer: true
---
Today we'll be looking at a sample plugin implementation, covering the various
callbacks we might want to expose and looking at some of the rebar internals in
the process.

The accompanying source code can be found in the
[plugin-anatomy-local](https://github.com/hyperthunk/rebar-plugin-tutorial/tree/part2) branch of the
[main git repository](https://github.com/hyperthunk/rebar-plugin-tutorial).

## Introducing the `foo-bar` plugin

In this post, we'll develop an illustrative plugin that exports two new
commands to rebar, `foo` and `bar`. To keep things simple, the commands won't
do much apart from printing to the console and maybe writing to the file
system. We'll also keep the packaging simple, by storing the plugin in the root
directory of the sample project.

For any project to be built by rebar, it needs an `.app.src` file, so we'll
generate one using rebar's `create-app` command:

    t4@malachi:rebar-plugin-tutorial $ rebar create-app appid=plugin_anatomy_local
    ==> rebar-plugin-tutorial (create-app)
    Writing src/plugin_anatomy_local.app.src
    Writing src/plugin_anatomy_local_app.erl
    Writing src/plugin_anatomy_local_sup.erl
    t4@malachi:rebar-plugin-tutorial $ rm src/plugin_anatomy_local_*
    t4@malachi:rebar-plugin-tutorial $

Once the initial project structure is in place - note that we removed any
extraneous modules from the source directory - we can test out our plugin:

    t4@malachi:rebar-plugin-tutorial $ echo "-module(foobar_plugin)." >> foobar_plugin.erl
    t4@malachi:rebar-plugin-tutorial $ echo "-compile(export_all)." >> foobar_plugin.erl
    t4@malachi:rebar-plugin-tutorial $ echo 'foo(_, _) -> io:format("foo!~n").' >> foobar_plugin.erl
    t4@malachi:rebar-plugin-tutorial $ echo "{plugins, [foobar_plugin]}." >> rebar.config
    t4@malachi:rebar-plugin-tutorial $ rebar foo -v
    DEBUG: Rebar location: "/Users/t4/bin/rebar"
    DEBUG: Load global config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/rebar.config"
    DEBUG: Entering /Users/t4/work/hyperthunk/rebar-plugin-tutorial
    DEBUG: Available deps: []
    DEBUG: Missing deps  : []
    INFO:  Loading plugin foobar_plugin from /Users/t4/work/hyperthunk/rebar-plugin-tutorial/foobar_plugin.erl
    DEBUG: Predirs: []
    ==> rebar-plugin-tutorial (foo)
    foo!
    DEBUG: Postdirs: []
    t4@malachi:rebar-plugin-tutorial $

By running the `foo` command with `-v`, we're treated to a slew of (often quite
useful) debugging information.
It's clear from this that our plugin was indeed loaded from the file system,
as we expected. The astute reader may have also noticed that the *foo!* text's
appearance in the output is lacking the logging prefixes (such as `DEBUG:`,
`INFO: ` and so on) present in many of the other log lines. If you want
generate log messages such as these, rather than just writing to stdout, you'll
need to use the `rebar_log` module instead. Let's alter our code to do just
that.

```erlang
-module(foobar_plugin).
-compile(export_all).

foo(_Config, _AppFile) ->
    rebar_log:log(info, "foo!~n", []).

```

Now let's try it out on the command line:

    t4@malachi:rebar-plugin-tutorial $ rebar foo
    ==> rebar-plugin-tutorial (foo)
    t4@malachi:rebar-plugin-tutorial $

And there's nothing there. Because rebar's default log level is set to `error`
in the `rebar.app` configuration file, nothing below that level is printed out
unless the `-v` switch is given:

    t4@malachi:rebar-plugin-tutorial $ rebar foo -v
    DEBUG: Rebar location: "/Users/t4/bin/rebar"
    DEBUG: Load global config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/rebar.config"
    DEBUG: Entering /Users/t4/work/hyperthunk/rebar-plugin-tutorial
    DEBUG: Available deps: []
    DEBUG: Missing deps  : []
    INFO:  Loading plugin foobar_plugin from /Users/t4/work/hyperthunk/rebar-plugin-tutorial/foobar_plugin.erl
    DEBUG: Predirs: []
    ==> rebar-plugin-tutorial (foo)
    WARN:  foo!
    DEBUG: Postdirs: []
    t4@malachi:rebar-plugin-tutorial $

Our call to `rebar_log` takes the desired log level as an argument; We can
pass one of debug, info, warn or error. Log messages at `error` level always
appear. If you want to change the default log level, you'll need to rebuild
your rebar binary with a slight modification to the `rebar.app` config file.
We'll be talking about managing custom rebar forks later on in the series.

## Handling Errors

Now that we've implemented the `foo` command, let's take a look at implementing
`bar`. This command will simply keep track of how many times it has been run.
To do this, we'll keep the count in a file stored in a subdirectory of the
project: `/build/foo.count`. Assuming the file exists, we read the count out
of the file and increment it by one, overwriting the file to store the result.

```erlang
bar(Config, _AppFile) ->
    FileName = filename:join("build", "bar.count"),
    {ok, Bin} = file:read_file(FileName),
    Count = list_to_integer(binary_to_list(Bin)),
    NewCount = Count + 1,
    io:format("Bar command has been run ~p times~n", [NewCount]),
    file:write_file(FileName,
                    list_to_binary(integer_to_list(NewCount)), [write]).

```

When we run this, it goes bang very nicely...

    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    ERROR: bar failed while processing /Users/t4/work/hyperthunk/rebar-plugin-tutorial: {'EXIT',{{badmatch,{error,enoent}},
             [{foobar_plugin,bar,2},
              {rebar_core,run_modules,4},
              {rebar_core,execute,4},
              {rebar_core,process_dir0,6},
              {rebar_core,process_commands,2},
              {rebar,main,1},
              {escript,run,2},
              {escript,start,1}]}}
    t4@malachi:rebar-plugin-tutorial $

Our deliberate oversight illustrates an important point: rebar does *not*
handle exceptions apart from wrapping them up into a formatted error message.
If your code crashes while it's executing, the error will be printed to the
console and the vm will subsequently exit (with a well behaved non-zero exit 
status).

We don't see a line number in the exception (using R14) but `{error, enoent}`
is a common return value in the `file` module, indicating that something (in
this case, a file we're trying to open) is not present. It was fairly obvious
the we need to check whether the file exists before attempting to open it, so 
let's try again.

```erlang
bar(Config, _AppFile) ->
    FileName = filename:join("build", "bar.count"),
    Count = case filelib:is_regular(FileName) of
        false ->
            0;
        true ->
            {ok, Bin} = file:read_file(FileName),
            list_to_integer(binary_to_list(Bin))
    end,
    NewCount = Count + 1,
    io:format("Bar command has been run ~p times~n", [NewCount]),
    rebar_utils:ensure_dir(FileName),
    file:write_file(FileName,
                    list_to_binary(integer_to_list(NewCount)), [write]).
```

And now we get the desired result:

    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 1 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 2 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 3 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 4 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 5 times
    t4@malachi:rebar-plugin-tutorial $

Notice the call to `rebar_utils:ensure_dir/1`, which ensures the directory
exists prior to writing the file. Another thing to take note of is that plugin
commands need to return the atom `ok` if they have succeeded, otherwise rebar
will stop executing the current command and halt the vm with an error report. 
The relevant code (in `rebar_core`) looks something like this:

```erlang
%% taken from rebar_core:run_modules/4
    case Module:Command(Config, File) of
        ok ->
            run_modules(Rest, Command, Config, File);
        {error, _} = Error ->
            {Module, Error}
    end
```

The call to `file:write_file/3` returns `ok` on success and `{error, Reason}`
on error, so we don't need to handle the return value explicitly at all. In 
general though, you need to keep this in mind when implementing new commands.

## Everybody wants to go to the party. Nobody wants to stay and clean up.

Let's implement a mechanism to reset the count for our command. We could create
a new `reset-bar` command, but hooking into the existing `clean` command is
probably more intuitive. All we need do is delete the `bar.count` file:

```erlang
-module(foobar_plugin).
-compile(export_all).

-define(BAR_COUNT, filename:join("build", "bar.count")).

clean(_Config, _AppFile) ->
    file:delete(?BAR_COUNT).

%% other functions ommitted

```

This works fine at first, but fails if we try to run `clean` twice:

    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 2 times
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 1 times
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    ERROR: clean failed while processing /Users/t4/work/hyperthunk/rebar-plugin-tutorial in module foobar_plugin: {error,enoent}
    t4@malachi:rebar-plugin-tutorial $

Once again, the requirement for command return values to be `ok` has bitten us.
Our call to `file:delete/1` is returning an error tuple because the file isn't
there after running `clean`. The file won't be there the first time `clean` is
run either, unless `bar` has been run first. We'll tidy this up by delegating
to a rebar utility module, which handles the whole thing.

```erlang
%% foobar_plugin.erl

clean(_Config, _AppFile) ->
    rebar_file_utils:rm_rf("build").

```

    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $

Much better.

## Running in `base_dir`

Let's see what happens when we throw in some dependencies to our
`rebar.config`.

```erlang
%% rebar.config
{plugins, [foobar_plugin]}.

{deps, [{hamcrest, ".*",
    {git, "git://github.com/hyperthunk/hamcrest-erlang.git"}}]}.

```

    t4@malachi:rebar-plugin-tutorial $ rebar get-deps
    ==> rebar-plugin-tutorial (get-deps)
    Pulling hamcrest from {git,"git@github.com:hyperthunk/hamcrest-erlang.git"}
    Initialized empty Git repository in /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/hamcrest/.git/
    Failed to add the RSA host key for IP address '207.97.227.239' to the list of known hosts (/Users/t4/.ssh/known_hosts).
    ==> hamcrest (get-deps)
    Pulling proper from {git,"http://github.com/manopapad/proper.git","master"}
    Initialized empty Git repository in /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/proper/.git/
    ==> proper (get-deps)
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> proper (bar)
    Bar command has been run 1 times
    ==> hamcrest (bar)
    Bar command has been run 1 times
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 1 times
    t4@malachi:rebar-plugin-tutorial $

Our `bar` command will be run for each of the dependencies *as well as* the
root project. Whilst this isn't particularly problematic for this trivial
command, more complex plugins may break (or do bad things to the dependencies)
when executed in this manner. The
[nodewatch](https://github.com/hyperthunk/nodewatch) project for example, has a
folder named `build` as part of it's existing directory structure, so the
removal of this directory during `clean` will effectively break the build for
that dependency.

You can avoid running commands in dependency dirs by passing `skip_deps=true`
on the command line, but this can make trivial build commands far more complex
for the user, forcing them to build your code in various steps, some with
`skip_deps=true` and some without. The ultimate evidence of this problem is the
presence of a *Makefile* wrapper around your use of rebar.

It is generally better to make sure that plugins only run when and where
they're supposed to and we can achieve this for our plugin using rebar's
`base_dir` global configuration property, which points to the *top level*
directory - i.e., the first directory in which the program was executed.

```erlang
%% foobar_plugin.erl
-define(BAR_COUNT, filename:join("build", "bar.count")).

clean(_Config, _AppFile) ->
    case is_base_dir() of
        true -> rebar_file_utils:rm_rf("build");
        false -> ok
    end.

bar(_Config, _AppFile) ->
    case is_base_dir() of
        true ->
            Count = case filelib:is_regular(?BAR_COUNT) of
                false ->
                    0;
                true ->
                    {ok, Bin} = file:read_file(?BAR_COUNT),
                    list_to_integer(binary_to_list(Bin))
            end,
            NewCount = Count + 1,
            io:format("Bar command has been run ~p times~n", [NewCount]),
            rebar_utils:ensure_dir(?BAR_COUNT),
            file:write_file(?BAR_COUNT,
                        list_to_binary(integer_to_list(NewCount)), [write]);
        false ->
            ok
    end.

is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

```

After making this change, we should see rebar still entering the dependency
directories and executing the command. What we're doing here is simply skipping
the processing (in our code), so the user doesn't have to think about
`skip_deps` and the like.

    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> proper (bar)
    ==> hamcrest (bar)
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 1 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> proper (bar)
    ==> hamcrest (bar)
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 2 times
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> proper (bar)
    ==> hamcrest (bar)
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 3 times
    t4@malachi:rebar-plugin-tutorial $ rebar clean
    ==> proper (clean)
    ==> hamcrest (clean)
    ==> rebar-plugin-tutorial (clean)
    t4@malachi:rebar-plugin-tutorial $ rebar bar
    ==> proper (bar)
    ==> hamcrest (bar)
    ==> rebar-plugin-tutorial (bar)
    Bar command has been run 1 times
    t4@malachi:rebar-plugin-tutorial $

## Next time…

We'll continue next time by looking at plugin pre and post command hooks as
well as how to package and reuse external plugins.
