-- -*- mode: sql; sql-product: mysql; -*-
-- Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


-- use shortishly;

-- create schema if not exists test;

-- drop table if exists test.t8;

-- create table test.t8 (i serial, p date, q0 datetime, q6 datetime(6), r0 timestamp, r6 timestamp(6), s0 time, s6 time(6), t year);

-- insert into test.t8 (p) values ('1000-01-01'), ('9999-12-31');

-- insert into test.t8 (q0) values ('1000-01-01 00:00:00'), ('9999-12-31 23:59:59');
-- insert into test.t8 (q6) values ('1000-01-01 00:00:00.000000'), ('9999-12-31 23:59:59.999999');

-- insert into test.t8 (r0) values ('1970-01-01 00:00:01'), ('2038-01-19 03:14:07');
-- insert into test.t8 (r6) values ('1970-01-01 00:00:01.000000'), ('2038-01-19 03:14:07.499999');

-- insert into test.t8 (s0) values ('-838:59:59'), ('838:59:59');
-- insert into test.t8 (s6) values ('-838:59:59.000000'), ('-838:59:59.000000');

-- insert into test.t8 (t) values (1901), (2155), (0000);
