lpr: Local Process Registry
===========================

Library to have a local process registry.

Use
---

The API is a subset of the [ostinelli/syn](https://github.com/ostinelli/syn) library.

```erlang
{ok, Ref} = lpr:start_link(),
ok = lpr:stop(Ref)

ok = lpr:register(Ref, Key, Pid)
ok = lpr:register(Ref, Key, Pid, Meta)
{error, pid_already_registered} = lpr:register(Ref, Key, Pid)
{error, taken} = lpr:register(Ref, Key, Pid)

ok = lpr:unregister(Ref, Key)
{error, undefined} = lpr:unregister(Ref, Key)

Pid = lpr:find_by_key(Ref, Key)
{Pid, Meta} = lpr:find_by_key(Ref, Key, with_meta)

Count = lpr:registry_count(Ref)
```

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 test

License
-------

Apache License 2.0

See LICENSE file for details

Author
------

Mariano Guerra
