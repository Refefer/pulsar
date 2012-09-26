Pulsar
======

Real-time high volume stats aggregation server.

Required Software
=================

 * Latest version of Erlang, at least R15 or higher.  
 * Rebar

Installing Dependencies
=======================

Erlang
------

1. Download latest source at: [www.erlang.org](http://www.erlang.org/download.html).  
2. Follow install instructions [here](http://wiki.basho.com/Installing-Erlang.html#Installing-on-GNU-Linux).

Rebar
-----
Note: Make sure you have Erlang installed before building Rebar!

1. Clone repo `git@github.com:basho/rebar.git` into new folder.

2. cd into that folder and type:

    `./bootstrap`

3. It will create a new file, `rebar`, in that folder which is what you want.  Either add it to your local executable path or copy it into your cloned Pulsar repo.

Installing Pulsar
=================

1. Git clone `git@github.com:Refefer/pulsar.git` into a directory.

2. Type the following (this assumes that rebar has been copied into the `pulsar` directory):
    
    Fetches the dependencies from github.
    `./rebar get-deps`

    Compiles all the erlang dependencies.
    `./rebar compile`

    The json driver has C bindings which need to be compiled.
    `cd deps/json`

    `make`

3. Assuming that no errors, start the server:

    `./run.sh`

    In another terminal, type:

    `curl -v "http://localhost:8080/site/list"`

    which should return `["www.example.com"]` if successful.

    In the original terminal, type `q().` or `Ctrl-C` and then `a`

Configuration
=============

There are currently a couple of configuration options which can be changed:

For testing, the configuration files are in `./config`

 * `hosts` - The set of distinct hosts to gather stats on.

 * `port` - Which port Pulsar will use.

 * `crosstab_fields` - Set of fields to allow drilling down on.  This will be described in detail a little later.

 * `max_connections` - Maximum number of clients allowed to connect to Pulsar concurrently.  For long polling, this number will need to be increased (as well as a number of kernel specific settings).  For short polling, this should be more than sufficient.

 * 'static_dir' - Path to your static files directory.  Should be set to an absolute path when deployed.

Using Pulsar
============

Input
-----
Pulsar is a real-time analytics aggregator.  It collects statistics in real time, packaging them every five seconds and sending relevant data to its listeners.

There are currently two inputs for collecting statistics:

1. Short-poll, or `tick`: A quick http request which passes statistics and immediately disconnects.  This is a good choice for high volume, quickly changing data such as from tailing logs.  After a set of statistics is published, the counters for each statistic starts again at zero.

2. Long-poll: Useful for tracking "state" statistics.  For example, long-poll would be useful for tracking large numbers of active users on a web site.  Pulsar has been tested up to 90k concurrent connections and can likely go much higher.

Both types of polling are interfaced over http.  Stats are passed in as GET parameters to their respective urls.  Each GET key represents a category and each value represents an item in that cateogry.
  Assuming that Pulsar is running on localhost at port 8080:

    Short-poll example: `http://localhost:8080/tick/[host]?key1=value&key2=value2

    Long-poll example: `http://localhost:8080/poll/[host]?key1=value&key2=value2`

Pulsar currently automatically maps the header field `Referer` to `url`.

Output
------

To view the gathered statistics, the most immediate way is to use the basic default viewer: `grapher`.  It's basic but can provide a lot of useful information.

To access it, goto `http://localhost:8080/static/grapher.html`

Export
------

Exporting statistics is currently disabled but will be added in the near future.

Deploying
---------
To be continued...