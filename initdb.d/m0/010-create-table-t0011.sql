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

-- drop table if exists test.t0011;

-- create table test.t0011 (i serial, q0 timestamp, q1 timestamp(1), q2 timestamp(2), q3 timestamp(3), q4 timestamp(4), q5 timestamp(5), q6 timestamp(6));

-- insert into test.t0011 (q0) values ('1970-01-01 00:00:01'),        ('2038-01-19 03:14:07');
-- insert into test.t0011 (q1) values ('1970-01-01 00:00:01.6'),      ('2038-01-19 03:14:07.1');
-- insert into test.t0011 (q2) values ('1970-01-01 00:00:01.65'),     ('2038-01-19 03:14:07.12');
-- insert into test.t0011 (q3) values ('1970-01-01 00:00:01.654'),    ('2038-01-19 03:14:07.123');
-- insert into test.t0011 (q4) values ('1970-01-01 00:00:01.6543'),   ('2038-01-19 03:14:07.1234');
-- insert into test.t0011 (q5) values ('1970-01-01 00:00:01.65432'),  ('2038-01-19 03:14:07.12345');
-- insert into test.t0011 (q6) values ('1970-01-01 00:00:01.654321'), ('2038-01-19 03:14:07.499999');
