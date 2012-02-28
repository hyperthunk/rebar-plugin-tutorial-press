---
layout: page
title: "Part 3 - Packaging Plugins"
date: 2012-01-04 11:25
comments: true
sharing: true
footer: true
---
In parts [one][part-1] and [two][part-2] we introduced rebar plugins and looked at the basic anatomy of a
plugin module. In this article we'll be looking at how you can package plugins
as *external dependencies* along with additional dependencies that your plugin
requires. We'll also be looking at the pre and post command hooks that plugins
can utilise in order to run before (or after) a given command.

The accompanying source code can be found in the
[plugin-packaging][plugin-branch] branch of the
[main git repository][git-repo]. Some additional libraries are also used and
links to the sources for these are provided both in the text, and in the final
*links* section.

## Packaging Requirements

If we wish to re-use a plugin across multiple projects, it makes sense to
package the plugin as an independent component rather than duplicate the source
code in many places. Once the plugin is packaged up and available for use
across projects, users can take advantage of it by either

1. installing the plugin *globally* (e.g., into `ERL_LIBS`)
2. installing the plugin as part of a standard `rebar get-deps compile` build process

We won't be looking at the approach for (1) until later in the series, so our
attention will be turned to packaging plugins in a manner that allows them to
be included in a projects as one of it's dependencies.

To demonstrate how this works we will develop two small projects, one which
implements a simple build plugin, and another which uses the plugin as part of
its build process. Along the way, we'll meet a few more utility functions that
can help with authoring plugins.

## Introducing the *bad_deps_plugin*

We're going to implement a plugin that allows us to fetch an Erlang library
from github so we can use it as a dependency. Of course, rebar already allows
us to do this, but there are some limitations that it places on potential
dependencies - they __must__ be packaged as an OTP application or library. Now
the common approach to dealing with projects which are not packaged *properly*
(from rebar's point of view) in their source repository, is to fork the
offending repo and fix the project structure so that rebar can handle it. This
creates a bit of a maintenance headache for the person maintaining the fork, so
we're going to handle the *fixing* with a simple plugin.

-------

### CAVEAT!

This plugin is being developed for illustrative purposes only - we've chosen
this particular area because it represents an interesting problem to solve
using plugins. There are numerous and better ways to deal with non-standard
dependencies, which we'll discuss at the end of the article.

-------

The *badly behaving* project we're going to *fix* is
[Kyle Kingsbury's Implementation of a Skewed Binary Heap][skewbinheap].
The git repository has all the Erlang sources (both the implementation and the
test code) in the top level directory, so we'll be fetching the sources and
then rebuilding the required file and directory structure on the fly. Because
rebar is *opinionated* about project structure - and this is something to be
commended - we cannot use the `get-deps` command to fetch the repository,
because rebar will fail after fetching the dependency because there is no
.app or .app.src file present.

We could bemoan the fact that the fetching and installation of dependencies
is coupled in this way, but instead we'll stick to problem solving and use
rebar's `rebar_utils:sh` function to run the `git clone` command *by hand*.

We're not using rebar's standard *deps* mechanism to get hold of our
dependency, yet we'll have to store the git repository information somewhere in
our build configuration. It's time to introduce the rebar configuration API.

## Handling Custom Configuration

In order to make sure that the dependency is still *required* by rebar, even
though it isn't being fetched in the usual manner, we will include an entry in
the `deps` configuration section but exclude the *source* element of the config
tuple. Doing this makes rebar aware that the stated dependency is required, but
excludes the possibility of fetching it remotely. We will also introduce a new
configuration section, named `bad_deps` in which to store the git url we need
in order to fetch the repository ourselves.

Let's take a look at the client's [rebar config file][client-config].

```erlang
%% rebar.config
{deps, [
    {skewbinheap, ".*"}
]}.

{bad_deps, [
    {skewbinheap, "https://github.com/aphyr/skewbinheap.git"}
]}.

```

Configuration data is made available to plugins in the first argument to the
command(s) they are exporting. The configuration tuples are held in an opaque
data structure, so all access should be done using the `rebar_config` module.
Here's our first stab at getting the configuration data out:

```erlang
-module(bad_deps_plugin).
-compile(export_all).

'check-config'(Config, _AppFile) ->
    rebar_log:log(info, "config = ~p~n",
        [rebar_config:get_local(Config, bad_deps, [])]).

```

Actually running this without trying to use it in a sample project is a bit of
a pain, not least because we need rebar to create the initial config for us.
I often skip trying to unit test plugins, not because it is impossible - there
are certainly ways to do it using eunit and/or common_test - but because in
general I find it easier to write a sample project to demonstrate the plugin's
use, and then use the [Rebar ReTest Plugin][retest] to do CLI-centric
integration testing. There will be a full set of articles on testing plugins
at a later date.

For now, we'll just put a *demo* config file locally in the plugin project so
that we can experiment:

```erlang
%% in file demo
{plugin_dir, "src"}.
{plugins, [bad_deps_plugin]}.
{bad_deps, [
    {skewbinheap, "https://github.com/aphyr/skewbinheap.git"}
]}.
```

And now on the command line, we can take a look....

    t4@malachi:rebar-plugin-tutorial $ rebar -C demo check-config -v
    DEBUG: Rebar location: "/Users/t4/bin/rebar"
    DEBUG: Load global config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/.rebar/config"
    DEBUG: Consult config file "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/demo"
    DEBUG: Entering /Users/t4/work/hyperthunk/rebar-plugin-tutorial
    DEBUG: Available deps: []
    DEBUG: Missing deps  : []
    INFO:  Loading plugin bad_deps_plugin from src/bad_deps_plugin.erl
    WARN:  Missing plugins: [rebar_skip_deps]
    DEBUG: Predirs: []
    ==> rebar-plugin-tutorial (check-config)
    INFO:  config = [{skewbinheap,"https://github.com/aphyr/skewbinheap.git"}]
    DEBUG: Postdirs: []
    t4@malachi:rebar-plugin-tutorial $

It probably didn't go unnoticed that the config handling function we called in
our plugin's `check-config` command is called get___local__ - it is well worth
understanding this early on. There are two kinds of configuration that rebar
works with

1. Global configuration - stored in the rebar application environment settings
2. Local configuration - read from a `.config` file and passed about explicitly

Of the two, local configuration data is itself handled in two disparate ways.
As we mentioned in [Part 1][part-1], each time a new directory is processed, a
new configuration set is created - either by reading the local `rebar.config`
file or creating an empty one. It is this *local to the directory*
configuration which we're reading when we call `rebar_config:get_local/3`. The
other configuration reading functions `get/3` and `get_list/3` do the same
thing: they search the entire config set (i.e., including any ancestors) and
return the first entry with the given key.

We must always bare in mind that it is up to the consumer of the configuration
API to decide how they want to deal with the given config. They may choose one
of the following functions to look up a given key

* `get_local` - reads the config for the current directory, or a default value
* `get`/`get_list` - returns the first config section matching the key
* `get_all` - gets all (explicitly stated) values for the whole config set

Just to make sure this point is clear, we'll look at an example. We will assume
that we have both parent and child directories with the following (respective)
configurations:

```erlang
%% parent rebar.config
{foo, "bar"}.
{squish, "thingumy"}.
{sub_dirs, ["child"]}.

%% child rebar.config
{foo, "baz"}.

```

Imagine that our `check-config` command was globally available to rebar and
takes the required key as `key=<name>` on the command line:

    t4@malachi:parent $ rebar check-config key=foo
    ==> child (check-config)
    config key 'foo' found (get_local): "baz"
    config key 'foo' found (get): "baz"
    config key 'foo' found (get_all): ["baz","bar"]
    ==> parent (check-config)
    config key 'foo' found (get_local): "bar"
    config key 'foo' found (get): "bar"
    config key 'foo' found (get_all): ["bar"]
    t4@malachi:parent $ rebar check-config key=squish
    ==> child (check-config)
    config key 'squish' not found (get_local)
    config key 'squish' found (get): "thingumy"
    config key 'squish' found (get_all): ["thingumy"]
    ==> parent (check-config)
    config key 'squish' found (get_local): "thingumy"
    config key 'squish' found (get): "thingumy"
    config key 'squish' found (get_all): ["thingumy"]
    t4@malachi:parent $

So as we can see, the consumer has a lot of control over the scope at which
configuration sections should be read from. For the key `foo`, defined in both
the parent and child directories, we see that both `get_local` and `get` will
stop searching once the key is found and therefore terminate with the config
section defined by the child - in this manner, the `rebar.config` in the child
sub-directory is overriding the value of `foo` defined in the parent config.
For the `squish` key however, we see that `get_local` fails to locate any
value for the key, whilst get continues it's search into the parent config and
locates the right value.

## Using Command Hooks

We want our plugin to do its work *before* rebar's internal dependency handling
mechanism starts working. As a result, we need to hook into `get-deps`, and for
this we will need a *pre-hook*. These are nothing more than exported functions
which have the prefix `pre_` in front of the command name - in our case making
for the rather odd looking export `pre_get-deps`.

The fetching code is fairly simple, calculating the correct target directory
and delegating to the shell to do the work.

```erlang
-module(bad_deps_plugin).
-export(['pre_get-deps'/2]).

'pre_get-deps'(Config, _AppFile) ->
    [ pre_load(Dep) || Dep <- rebar_config:get_local(Config, bad_deps, []) ],
    ok.

pre_load({Dep, Url}) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    TargetDir = filename:join(DepsDir, atom_to_list(Dep)),
    case filelib:is_dir(TargetDir) of
        true ->
            %% we've already fetched this one
            ok;
        false ->
            rebar_utils:sh("git clone " ++ Url, [{cd, DepsDir}])
    end.

```

Now in the sample project that is using this plugin, we need to fetch it from
github. Because *one* of our dependencies - that is, the skewbinheap library -
isn't compatible with rebar, if we put both the plugin and the *broken* dep
into our config and run `rebar get-deps compile`, we *do* see the plugin get
fetched and built, *but* we end up with a non-zero exit code. Personally I see
that as an indication that the build has failed, rather than a *normal* step
of the build process. I think this is especially important in this day and age
where tools like CI servers will report a broken build and of course you can't
pipe failing shell commands or join them with `&&` and the like.

Instead, we'll configure the plugin as a dependency by itself in a separate
config file, and fetch and build it by itself first. After the plugin has been
successfully built, we can use it to bootstrap `skewbinheap` properly for us.
By convention, I like to name rebar config files that are used for bootstrap or
initialisation by their task.

```erlang
%% init.config
{deps, [
    {bad_deps_plugin, ".*",
        {git, "git://github.com/hyperthunk/rebar-plugin-tutorial.git",
            "part3-plugin"}}
]}.

```

    t4@malachi:rebar-plugin-tutorial $ rebar -C init.config get-deps compile
    ==> rebar-plugin-tutorial (get-deps)
    Pulling bad_deps_plugin from {git,"git://github.com/hyperthunk/rebar-plugin-tutorial.git",
                                      "part3-plugin"}
    Initialized empty Git repository in /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/bad_deps_plugin/.git/
    Branch part3-plugin set up to track remote branch part3-plugin from origin.
    ==> bad_deps_plugin (get-deps)
    ==> bad_deps_plugin (compile)
    Compiled src/bad_deps_plugin.erl
    ==> rebar-plugin-tutorial (compile)
    t4@malachi:rebar-plugin-tutorial $

Now that the dependency is installed, we can do a test run to see if our git
command is working as expected. First the main `rebar.config` needs updating,
because *it* needs to know about the plugin dependency in order for the plugin
to get added to the code path before the `get-deps` command is executed.

```erlang
%% tell rebar that we're using this plugin
{plugins, [bad_deps_plugin]}.

{deps, [
    %% make sure that rebar puts bad_deps_plugin on the code path for each cmd
    {bad_deps_plugin, ".*"},
    {skewbinheap, ".*"}
]}.

{bad_deps, [
    {skewbinheap, "https://github.com/okeuday/skewbinheap.git"}
]}.

```

Once we run `get-deps` we can see clearly that our `git clone` command has
indeed worked, because the directory is present. What's interesting to note
here is that `get-deps` will not return a non-zero exit status even when the
`skewbinheap` dependency isn't structured properly as an OTP application. The
`compile` command however, will fail in this manner.

    t4@malachi:rebar-plugin-tutorial $ rebar get-deps
    ==> bad_deps_plugin (pre_get-deps)
    ==> bad_deps_plugin (get-deps)
    ==> rebar-plugin-tutorial (pre_get-deps)
    Initialized empty Git repository in /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/skewbinheap/.git/
    ==> rebar-plugin-tutorial (get-deps)
    t4@malachi:rebar-plugin-tutorial $ ls deps/
    bad_deps_plugin skewbinheap
    t4@malachi:rebar-plugin-tutorial $ echo $?
    0
    t4@malachi:rebar-plugin-tutorial $ rebar compile
    ==> bad_deps_plugin (compile)
    ==> rebar-plugin-tutorial (compile)
    Dependency not available: skewbinheap-.* (undefined)
    t4@malachi:rebar-plugin-tutorial $ echo $?
    1
    t4@malachi:rebar-plugin-tutorial $

Now the other thing to note is that the `pre_get-deps` command is running for
each of the dependencies as well as the top level project. We've come across
this aspect of rebars build lifecycle before (in [Part 2][part-2]) and we can
either make the user `skip_deps` or we can handle the edge case in the plugin.
There is an argument *for* letting a dependency handling command/plugin
continue to process directories recursively, but in practise we'll see that
this doesn't work without significant coding on the part of the plugin author.

For now, we're just going to exclude anything outside of `base_dir` as we did
in [Part 2][part-2]. To avoid writing all the tedious `case is_base_dir()`
boilerplate everywhere, we're going to utilise an [annotation][annotations]
that I've written in to a 
[little library](https://github.com/hyperthunk/rebar_annotations/blob/master/src/base_dir.erl),
which will do the work for us. This also gives us an opportunity to look at 
how plugins with their own dependencies need to be handled when they're
included in a project.

Back in the [part3-plugin branch][plugin-branch], we'll switch over to use the
[annotation][annotations]. First the rebar build config needs tweaking so that
we get hold of the two libraries we're going to be using (there are links to
both of them at the bottom of the page).

```erlang
%% rebar.config
{lib_dirs, ["deps"]}.
{deps, [
    {rebar_annotations, ".*",
        {git, "https://github.com/hyperthunk/rebar_annotations.git"}}
]}.

{plugins, [rebar_annotations_plugin]}.

{annotations, [
    {registered, [base_dir]}
]}.

```

And now we can simply use the `-base_dir` annotation anywhere in our code.
We're going to apply it to the `pre_get-deps/2` function to ensure that it does
not run for sub_dirs or dependencies.

```erlang
-module(bad_deps_plugin).
-export(['pre_get-deps'/2]).
-include_lib("annotations/include/annotations.hrl").

-base_dir(only).
'pre_get-deps'(Config, _AppFile) ->
    [ pre_load(Dep) || Dep <- rebar_config:get_local(Config, bad_deps, []) ],
    ok.

pre_load({Dep, Url}) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    TargetDir = filename:join(DepsDir, atom_to_list(Dep)),
    case filelib:is_dir(TargetDir) of
        true ->
            %% we've already fetched this one
            ok;
        false ->
            rebar_utils:sh("git clone " ++ Url, [{cd, DepsDir}])
    end.

```

In order to compile the plugin with [annotations][annotations] support enabled,
we need to update the `init.config` to meet the following requirements:

1. The libraries used during the *parse transform* phase need to be on the code path
2. The rebar [annotations][annotations] plugin needs to be somewhere rebar can find it

In order to locate the rebar [annotations][annotations] plugin at runtime, we
bootstrap the `plugin_dir` to point to `deps/annotations/src`, which works
nicely when you've got a single plugin build.

```
%% init.config
{lib_dirs, ["deps"]}.

{plugin_dir, "deps/annotations/src"}.
{plugins, [rebar_annotations_plugin]}.

{deps, [
    {bad_deps_plugin, ".*",
        {git, "git://github.com/hyperthunk/rebar-plugin-tutorial.git",
            "part3-plugin"}}
]}.

```

Notice that we're configuring `lib_dirs` to point to the `deps` directory here.
This is required so that the OTP `compile` module can decode the `include_lib`
references. Now that we've dealt with getting the `lib_dirs` on the code path,
we are in a position where the plugin will kick in and do its work in
`base_dir` only. Our build process is now two-phase, but in a later article,
we will look at how we can combine these phases into a single command.

    t4@malachi:rebar-plugin-tutorial $ rebar -C init.config get-deps compile
    t4@malachi:rebar-plugin-tutorial $ rebar get-deps compile

## Fixing the directory structure

Now it's time to move the files around. We're going to provide some additional
configuration in the `bad_deps` section, to specify which files are considered
*production* code and which are test. There is another useful method in
`rebar_utils` which finds files recursively based on a regular expression and
we'll delegate to this for moving stuff around. Let's take a look at the new
`bad_deps` configuration section then.

We won't worry about handling `.yrl` or `.mib` files (or any other custom file
formats such as erlydtl templates) as our target library doesn't have any.

```erlang
{bad_deps, [
    {skewbinheap, "https://github.com/okeuday/skewbinheap.git", [
        {src_main, "^.*(?<!_eqc|_prop|_proper|_test|_tests)\\.erl\$"},
        {src_test, "^.*(_eqc|_prop(er)?|_test(s)?)\\.erl\$"}
    ]}
]}.
```

The code to handle these regular expressions is fairly noddy, simply delegating
to `rebar_utils` to do most of the real work. We also need to generate an 
`.app.src` file in the right place, so that rebar recognises the project as an
OTP library.

The full plugin code is listed below. As well as the `find_files` function,
we're also using another `rebar_file_utils` function to shuffle the sources
into the right place.

```erlang
-module(bad_deps_plugin).
-export(['pre_get-deps'/2]).
-include_lib("annotations/include/annotations.hrl").

-base_dir(only).
'pre_get-deps'(Config, _AppFile) ->
    rebar_log:log(debug, "pre_get-deps running in ~s~n",
                    [rebar_utils:get_cwd()]),
    [ pre_load(Dep) || Dep <- rebar_config:get_local(Config, bad_deps, []) ],
    ok.

pre_load({Dep, Url, Config}) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    Project = atom_to_list(Dep),
    TargetDir = filename:join(DepsDir, Project),
    case filelib:is_dir(TargetDir) of
        true ->
            %% we've already fetched this one
            ok;
        false ->
            rebar_utils:sh("git clone " ++ Url, [{cd, DepsDir}]),
            ProjectDir = filename:join([rebar_utils:get_cwd(),
                                        DepsDir, Project]),
            SrcDir = filename:join(ProjectDir, "src"),
            TestDir = filename:join([rebar_utils:get_cwd(),
                                     DepsDir, Project, "test"]),
            rebar_utils:ensure_dir(filename:join(SrcDir, "foo.txt")),
            rebar_utils:ensure_dir(filename:join(TestDir, "foo.txt")),

            generate_app_file(Project, ProjectDir),
            SrcPattern = proplists:get_value(src_main, Config, "^.*\\.erl\$"),
            TestPattern = proplists:get_value(src_test,
                                              Config, "^.*_tests\\.erl\$"),
            [ mv(Src, SrcDir) || Src <- find(ProjectDir, SrcPattern)],
            [ mv(Src, TestDir) || Src <- find(ProjectDir, TestPattern)]
    end.

generate_app_file(Project, ProjectDir) ->
    Target = filename:join([ProjectDir, "src", Project ++ ".app.src"]),
    file:write_file(Target, app(Project), [write]).

app(Project) ->
    App = {application, list_to_atom(Project),
           [{description, ""},
            {vsn, "1"},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

find(ProjectDir, SrcPattern) ->
    rebar_utils:find_files(ProjectDir, SrcPattern).

mv(Src, SrcDir) ->
    case rebar_file_utils:mv(Src, filename:join(Src, SrcDir)) of
        ok -> ok;
        {error, Reason} -> rebar_utils:abort(Reason, [])
    end.
```

Most of the (verbose) output has been snipped from the shell session listed
below, but the relevant bits are there and the plugin is clearly doing its job
properly.

    t4@malachi:rebar-plugin-tutorial $ rebar get-deps -v
    # <<<<<<<<<<<<<< LOTS OF OUTPUT SNIPPED! >>>>>>>>>>>>>>
    ==> rebar-plugin-tutorial (get-deps)
    DEBUG: is_app_available, looking for App annotations with Path "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/annotations"
    DEBUG: vcs_vsn: Unknown VCS atom in vsn field: "0.0.1"
    INFO:  Looking for annotations-.* ; found annotations-0.0.1 at /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/annotations
    DEBUG: is_app_available, looking for App rebar_annotations with Path "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/rebar_annotations"
    DEBUG: vcs_vsn: Unknown VCS atom in vsn field: "1"
    INFO:  Looking for rebar_annotations-.* ; found rebar_annotations-1 at /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/rebar_annotations
    DEBUG: is_app_available, looking for App bad_deps_plugin with Path "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/bad_deps_plugin"
    DEBUG: vcs_vsn: Unknown VCS atom in vsn field: "1"
    INFO:  Looking for bad_deps_plugin-.* ; found bad_deps_plugin-1 at /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/bad_deps_plugin
    DEBUG: is_app_available, looking for App skewbinheap with Path "/Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/skewbinheap"
    DEBUG: vcs_vsn: Unknown VCS atom in vsn field: "1"
    INFO:  Looking for skewbinheap-.* ; found skewbinheap-1 at /Users/t4/work/hyperthunk/rebar-plugin-tutorial/deps/skewbinheap
    DEBUG: Postdirs: []
    t4@malachi:rebar-plugin-tutorial $ ls -la deps/skewbinheap/
    total 24
    drwxr-xr-x   7 t4  staff   238  6 Jan 11:42 .
    drwxr-xr-x   8 t4  staff   272  6 Jan 11:42 ..
    drwxr-xr-x  13 t4  staff   442  6 Jan 11:42 .git
    -rw-r--r--   1 t4  staff    58  6 Jan 11:42 .gitignore
    -rw-r--r--   1 t4  staff   737  6 Jan 11:42 README.markdown
    drwxr-xr-x   4 t4  staff   136  6 Jan 11:42 src
    -rw-r--r--   1 t4  staff  1602  6 Jan 11:42 test
    t4@malachi:rebar-plugin-tutorial $ ls -la deps/skewbinheap/src/
    total 16
    drwxr-xr-x  4 t4  staff   136  6 Jan 11:42 .
    drwxr-xr-x  7 t4  staff   238  6 Jan 11:42 ..
    -rw-r--r--  1 t4  staff   101  6 Jan 11:42 skewbinheap.app.src
    -rw-r--r--  1 t4  staff  3781  6 Jan 11:42 skewbinheap.erl
    t4@malachi:rebar-plugin-tutorial $ 

## Conclusion

We've developed a useful little plugin here. In general, forking repositories
is made very easy by modern dvcs tools, so the fork/clone-and-maintain approach
is probably a better way to deal with aberrant project structures like this
one. 

A more general problem occurs when building foreign dependencies, some of
which may not be compatible with rebar, may use another build system (such as
make) or may even not contain Erlang sources at all! When dealing with these
situations, I would recommend looking at the 
[rebar_alien_plugin][rebar_alien_plugin], which attempts to provide a framework
for handling them. We will introduce that plugin in detail later on in the
series, as it is effectively a configuration driven plugin compiler.

## Next time...

We'll continue next time by looking at the `preprocess` and `postprocess` hooks
and will introduce a new (and useful) working plugin. We will also introduce
the Rebar Plugin Manager project, which provides hooks to compensate for many
of the complexities we've encountered with getting the code path set up 
correctly so that plugins are available.

## Links

As promised, all the relevant links for this article are listed below.

1. [main source code repository][git-repo]
2. [branch containing the sample plugin code][plugin-branch]
3. [branch containing the sample project][consumer-branch]
3. [annotations library][annotations]
4. [rebar annotations plugin support library][rebar-annotations]
5. [Erlang Solutions' parse_trans library][parse_trans]
6. [rebar_alien_plugin][rebar_alien_plugin]

[part-1]: http://hyperthunk.github.com/rebar-plugin-tutorial/part-1-introducing-rebar-plugins/index.html
[part-2]: http://hyperthunk.github.com/rebar-plugin-tutorial/part-2-plugin-anatomy/index.html
[plugin-branch]: https://github.com/hyperthunk/rebar-plugin-tutorial/tree/part3-plugin
[consumer-branch]: https://github.com/hyperthunk/rebar-plugin-tutorial/tree/part3-project
[git-repo]: https://github.com/hyperthunk/rebar-plugin-tutorial
[client-config]: https://github.com/hyperthunk/rebar-plugin-tutorial/blob/part3-project/rebar.config
[skewbinheap]: https://github.com/aphyr/skewbinheap
[annotations]: https://github.com/hyperthunk/annotations
[rebar-annotations]: https://github.com/hyperthunk/rebar_annotations
[retest]: https://github.com/hyperthunk/rebar_retest_plugin
[parse_trans]: https://github.com/esl/parse_trans
[rebar_alien_plugin]: https://github.com/hyperthunk/rebar_alien_plugin
