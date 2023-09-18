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

drop table if exists test.t0010;

create table test.t0010 (i serial, q0 datetime, q1 datetime(1), q2 datetime(2), q3 datetime(3), q4 datetime(4), q5 datetime(5), q6 datetime(6));

insert into test.t0010 (q0) values ('1000-01-01 00:00:00'), ('9999-12-31 23:59:59');
insert into test.t0010 (q1) values ('1000-01-01 00:00:00.6'), ('9999-12-31 23:59:59.6');
insert into test.t0010 (q2) values ('1000-01-01 00:00:00.65'), ('9999-12-31 23:59:59.65');
insert into test.t0010 (q3) values ('1000-01-01 00:00:00.654'), ('9999-12-31 23:59:59.654');
insert into test.t0010 (q4) values ('1000-01-01 00:00:00.6543'), ('9999-12-31 23:59:59.6543');
insert into test.t0010 (q5) values ('1000-01-01 00:00:00.65432'), ('9999-12-31 23:59:59.65432');
insert into test.t0010 (q6) values ('1000-01-01 00:00:00.654321'), ('9999-12-31 23:59:59.654321');
