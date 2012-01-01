---
layout: post
title: "Plugin Anatomy"
date: 2011-12-31 14:18
comments: true
categories: 
---
Today we'll be looking at a sample plugin implementation, covering the various
callbacks we might want to expose and looking at some of the rebar internals in
the process.

The accompanying source code can be found in the 
[plugin-anatomy-local](https://github.com/hyperthunk/rebar-plugin-tutorial/tree/plugin-anatomy-local) branch of the 
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
	t4@malachi:rebar-plugin-tutorial $ rm src/pl
	plugin_anatomy_local.app.src  plugin_anatomy_local_app.erl  plugin_anatomy_local_sup.erl  
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

By running the `foo` command with `-v`, we get to see debugging information,
in which it is clear that the plugin was loaded from the file system. The 
astute reader will have also noticed that the `io:format/1` call has produced
the *foo!* text in the output, but that the logging prefix (e.g., `DEBUG:`) is
missing. If you want to log messages rather than just print them out, you'll 
need to use the `rebar_log` module instead. Let's alter our code to do just 
that.

All that rebar needs in order to recognise the `foo` command is an exported 
function of arity 2, in at least one of the modules (or in our case, plugins).
In the rewritten version of the code, we'll name the variables for the two 
function arguments, although we won't be using them yet.

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
	
And there's nothing there. This is because rebar's default log level is 
`error`, so nothing below that level is printed out unless the `-v` switch is
given:

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

Our call to `rebar_log` take the desired log level as an argument and we can 
pass one of `debug, info, warn, error`.

### Running in `base_dir`

Now that we've implemented the `foo` command, let's take a look at implementing 
`bar`. 
