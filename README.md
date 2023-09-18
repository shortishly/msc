<br>

<p align="center">
    <a href="https://shortishly.github.io/msc/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fmsc%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/msc/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/msc?style=flat-square">
    </a>
</p>

## What is msc?

An Apache licensed MySQL/MariaDB [Erlang/OTP 26+][erlang-org] client
that includes replication.

Start a local mysql:

```bash
./bin/up
```

Build the client with OTP 26+:

```bash
make
```

Run an Erlang/OTP 26+ with the client:

```bash
make shell
```

Connect to the database, run a very simple query:

```erlang
1> URI = <<"mysql://root:secret@localhost:3306/test">>.
2> {ok, Supervisor} = msc_connections_sup:start_child(URI).
3> MM = msc_sup:get_child_pid(Supervisor, mm).
4> msc_mm_sync:query(#{server_ref => MM,
                       query => <<"select 2 + 2">>}).
{[#{decimals => 0,flags => 129,name => <<"2 + 2">>,
    table => <<>>,type => 8,character_set => 63,
    reserved0 => <<0,0>>,
    catalog => <<"def">>,schema => <<>>,org_table => <<>>,
    org_name => <<>>,length_of_fixed_length_fields => 12,
    column_length => 3}],
 [[<<"4">>]]}
```

Replicate data into ETS:

```erlang
1> URI = <<"mysql://root:secret@localhost:3306/test">>.
2> {ok, Supervisor} = msc_connections_sup:start_child(URI).
3> MM = msc_sup:get_child_pid(Supervisor, mm).
4> SQL = <<"SET @master_binlog_checksum = @@global.binlog_checksum, @source_binlog_checksum = @@global.binlog_checksum">>.
5> msc_mm_sync:query(#{server_ref => MM, query => SQL}).
6> {ok, _} = msc_binlog_ets:start().
7> msc_mm_sync:binlog_dump(#{server_ref => MM, call_back => msc_binlog_ets}).
8> ets:i(test_t7).
```


