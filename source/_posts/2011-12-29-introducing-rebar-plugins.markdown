---
layout: post
title: "Introducing Rebar Plugins"
date: 2011-12-29 21:49
comments: true
categories: 
---
This is the first post in a series on customising rebar using plugins. 
Initially, we'll focus on *how* plugins work, then move on to see what kind 
of extensibility they can provide to developers.

### Caveats

Currently there's no official API for plugins, so it's best to limit which 
internals or internal functions you rely on (even if they're exported). What
this means empirically is that you'll need to check your plugins for 
compatibility whenever you upgrade to a new version of rebar. There appears 
to be some work in progress to deal with this in a more structured way, but 
for external plugins (which are hosted in their own repository) this is worth
keeping in mind.

### First things first

Currently rebar supports two kinds of extensibility mechanism: Plugin 
Extensions and Hooks. Hooks are a lightly documented feature, with the only 
real explanation being the [sample rebar config file](https://github.com/basho/rebar/blob/master/rebar.config.sample#L142). 
We’re not going to cover hooks in much detail, as they are simple enough to 
understand and are only really applicable to simple scripting tasks that don’t 
require, for example, cross platform support or complex logic. Plugin 
Extensions on the other hand, are documented (to some extend anyway), and 
provide a much greater degree of extensibility to developers.

Before we can talk sensibly about plugins, we need to take a look at some of 
the fundamentals behind rebar, especially its handling of build configuration 
files and command processing logic. For any given command, say `foo`, rebar 
understands the command if (and only if) *one of the modules it knows about* 
exports a function with a signature that matches:

- the name of the command and arity 2
- the name of the command prefixed with pre_, and arity 2
- the name of the command prefixed with post_, and arity 2

We’ll be covering how rebar *knows about* modules later on, but for now we’ll 
just assume it’s magic. For the command `foo` to have any meaning then, we’d
need at least one module with at least one of the following signatures 
exported:

```erlang
-module(foo).
-compile(export_all).

pre_foo(_, _) ->
	io:format("we're running just before foo!~n").

foo(_, _) ->
	io:format("we're in foo!~n").

post_foo(_, _) ->
	io:format("we're running just after foo!~n").

```
  
Another essential is the four ways in which rebar handles build configuration. 
When rebar first runs, before handling the current directory, it loads the 
global config file from $HOME/.rebar/config if it actually exists, otherwise 
creating an empty config set. The config for *any* subsequent directory is
handled by either (a) examining the terms in the local file `rebar.config` if 
it exists, or (b) creating an empty config set. 
When executing in the first, top level directory (referred to in the code as 
the `base_dir`), rebar will check for a special global variable (passed as 
`config=` or `-C <config-name>` on the command line)  which overrides 
the name of the config file it should search in. This technique is only applied 
to configuration files in the `base_dir`.

The fourth approach to configuration handling is not just for initialising new 
configurations. As rebar executes user commands (e.g., `clean`, `compile`, 
`eunit`, etc) in a given directory, it uses two special commands to obtain a 
list of directories that need to be processed before the current one, and 
providing the current directory is processed without error, afterwards as well. 
These commands, `preprocess` and `postprocess`, can be exported by any module.
As rebar executes, it builds up a list of modules that understand the current 
command. For each of these modules it tries to call `pre` and `postprocess`, 
then traverses any pre- dirs before handling the current command in the current 
directory. Once all the pre-processing is done, each module that exports one of 
the three function signatures compatible with the current command is called 
(for one or more of the `pre_<command>/2`, `<command>/2` and 
`post_<command>/2` exports) to handle the actual command. The directories 
returned by any postprocess calls are handled last of all.

What is vital to understand about all of this, is that as rebar traverses the 
file system, recursively handling any pre- directories, in each new dir it 
executes with a brand new rebar config set. This config set inherits the parent 
configuration (i.e., the config set for the parent dir) but can override 
certain configuration variables by providing it’s own rebar.config file. This 
is how dependencies and applications stored in sub-directories are handled. The 
salient points about this mechanism are that

  - the only configuration file rebar notices in sub-directories is the one named rebar.config
  - any configuration override (passed with -C for example) is ignored in sub-directories
  - just because a local rebar.config overrides a variable/setting, this might not be applied

Point #3 is a bit scary if you’re new to rebar, but essentially it is the 
result of rebar’s config handling module exporting multiple config handling 
functions, some of which get the local (i.e., the most locally scoped) value, 
some a list of possible values and others the combined list of all values. 
Depending on which of these functions a particular command/module uses when 
reading the configuration, you can potentially see a number of things happen:

  - you might see the local value (from rebar.config) get applied
  - you might see the value from the parent config get applied (e.g., if there is no local config)
  - you might see the local value get ignored

I strongly recommend spending some time looking at rebar’s config module if 
you’re planning on writing plugins (or using complex plugins written by 
others), as it’ll save you a lot of head scratching time if you understand this 
up front.

## What are plugins?

As far as rebar is concerned, plugins are simply Erlang modules that it knows 
something about. There are essentially two ways that rebar knows about modules:

  - From the rebar.app configuration file
  - Via the plugins section of the build configuration

Modules registered in the rebar.app configuration file are basically part of 
rebar itself. Plugins on the other hand, are modules which any given build 
configuration (somewhere in the tree) registers via the `plugins` configuration 
element. This configuration is built up to include every level, including the 
global config, so if you’ve got no local `plugins` configuration, this does
not mean `plugins` won’t get run in your subdirectories. In practise, this 
means that plugins registered up top (e.g., globally or in the `base_dir` 
configuration) will get run in all your sub-directories, including of course 
dependencies. Bare this in mind when using plugins, and take advantage of 
`skip_deps`, `apps=` and `skip_apps=` where necessary to avoid unexpected 
things happening in your sub-dirs and deps folders.

When we look at authoring plugins later in the series, we'll examine the 
various ways in which you can code your plugins to be aware of the scope in 
which they're executing.

## Plugin Classification

To (hopefully) make the differences between plugin extensions and built-in 
modules a bit clearer, we’re going to classify plugin extensions into three 
groups, and will hereafter refer to them simply as plugins:

  - Internal/Built-in
  - External/Pre-packaged
  - Local

Let’s look at what these classifications mean in practise, and hopefully get an 
understanding of the terminology I’ve chosen. Internal (or built-in) modules 
come bundled as part of rebar itself, and as per the documentation, these are 
registered in the rebar application config file. The functionality exposed by 
these modules is available to every rebar user, so they work *Out Of The Box*. 
These plugins are the least likely to be used for extending rebar however, 
because in practise they require you to either (a) maintain a custom fork of 
rebar or (b) submit a pull request in order for your extension(s) to be 
accepted as part of the main source tree. It is the other two types of plugin 
we will be looking at in this post.

### External Plugins

Pre-packaged plugins are bundled as separate Erlang/OTP libraries to be 
installed globally, or included in a project using rebar’s dependency handling 
mechanism. The latter technique is more useful, as it ensures that someone who 
fetches your source code to build/install it, will be able to obtain the right 
plugins without going outside of the project source tree.

The key thing to understand here is that the plugin must be installed *somehow* 
in order for rebar to pick it up. We’ve mentioned that rebar knows about 
plugins because they’re in the `{plugins, ListOfPlugins}` 
configuration element,  but in practise things aren’t quite that simple. In 
order for a plugin to actually get executed (in response to a specific command, 
it’s pre/post hooks or indeed the special preprocess and postprocess commands), 
it needs to be on the code path! This is fine if the plugin is installed 
globally into the user’s erl environment (for example by putting it’s 
application root directory somewhere on the ERL_LIBS environment variable), but 
not so fine if you’re fetching it into the project’s dependencies. If the 
dependency is a direct one, then the `preprocess` handler in rebar_deps will 
nicely update the code path for all commands, so as long as you’re not trying 
to make the plugin run before rebar’s built-in modules (which is, in fact, 
impossible) then it’ll be on the path. This once again doesn’t always work in 
practise, because the function that builds up the code path makes no 
attempt to deal with transitive dependencies. I keep meaning to do a pull 
request for this, but I’m waiting for others to get through the queue first.

### Local Plugins

You probably recall that I mentioned plugins need to be on the code path in 
order to be executed by rebar? Well thanks to a nifty pull request from yours 
truly, there is in fact another way. If rebar cannot find a module on the code 
path matching the name given to the plugins configuration element, it will 
attempt to locate a source file for the module in either the base_dir or the 
directory indicated by the plugin_dir config element. If it finds a source file 
with a matching name, it attempts to compile it on the fly and load the beam 
code into the running emulator, thereby making the plugin available 
dynamically.

The aim of local plugins is to provide a mechanism for scripting complex 
tasks/hooks that apply only to your specific project. This is in contrast with 
the idea of external/pre-packaged plugins, which provide add-on re-usable 
features to rebar that can be used across projects.

## Next time…

Next time we’ll be looking at the structure of the plugin callback functions 
and how to use them in practise. We’ll also be taking a whirlwind tour of some 
of the commonly (re)used rebar modules such as rebar_config, rebar_utils and 
rebar_log, as well as discussing some of the pros and cons of using plugins and 
what the current workarounds look like. We’ll finish with a working example of 
an external plugin that adds new functionality to rebar with all the source 
code available on github.
