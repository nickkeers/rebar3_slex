rebar3_slex
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_slex, ".*", {git, "git@host:user/rebar3_slex.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_slex
    ===> Fetching rebar3_slex
    ===> Compiling rebar3_slex
    <Plugin Output>
