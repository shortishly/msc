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

drop table if exists test.t0012;

create table test.t0012 (i serial, q0 time, q1 time(1), q2 time(2), q3 time(3), q4 time(4), q5 time(5), q6 time(6));

insert into test.t0012 (q0) values ('-838:59:59'), ('838:59:59');
insert into test.t0012 (q1) values ('-838:59:58.6'), ('838:59:58.1');
insert into test.t0012 (q2) values ('-838:59:58.65'), ('838:59:58.12');
insert into test.t0012 (q3) values ('-838:59:58.654'), ('838:59:58.123');
insert into test.t0012 (q4) values ('-838:59:58.6543'), ('838:59:58.1234');
insert into test.t0012 (q5) values ('-838:59:58.65432'), ('838:59:58.12345');
insert into test.t0012 (q6) values ('-838:59:58.654321'), ('838:59:58.123456');
insert into test.t0012 (q6) values ('-838:59:59.000000'), ('838:59:59.000000');
