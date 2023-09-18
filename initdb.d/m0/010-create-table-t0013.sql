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


use shortishly;

create schema if not exists test;

drop table if exists test.t0013;

create table test.t0013 (i serial, q0 varchar(255) character set latin1 collate latin1_general_cs, q1 varchar(8192) character set latin1 collate latin1_general_cs, q2 varchar(255) character set utf8mb4 collate utf8mb4_general_ci);

insert into test.t0013 (q0) values ('a'), (repeat('abcde', 1)), (repeat('abcde', 51));
insert into test.t0013 (q1) values ('a'), (repeat('abcde', 1)), (repeat('abcde', 52)), (repeat('abcde', 820));
insert into test.t0013 (q2) values ('åäöabc');
