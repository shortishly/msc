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

set names utf8;

create schema if not exists test;

drop table if exists test.t0018;

create table test.t0018 (i serial, q0 json) charset utf8mb4;
show create table test.t0018;

insert into test.t0018 (q0) values ('["abc", 10, null, true, false]'), ('{"k1": "value", "k2": 10}'), ('["12:18:29.000000", "2015-07-29", "2015-07-29 12:18:29.000000"]');

insert into test.t0018 (q0) values (NULL);
insert into test.t0018 (q0) values ('{"a": 2}');
insert into test.t0018 (q0) values ('[1,2]');
insert into test.t0018 (q0) values ('{"a":"b", "c":"d","ab":"abc", "bc": ["x", "y"]}');
insert into test.t0018 (q0) values ('["here", ["I", "am"], "!!!"]');
insert into test.t0018 (q0) values ('"scalar string"');
insert into test.t0018 (q0) values ('true');
insert into test.t0018 (q0) values ('false');
insert into test.t0018 (q0) values ('null');
insert into test.t0018 (q0) values ('-1');
-- insert into test.t0018 (q0) values (CAST(CAST(1 AS UNSIGNED) AS JSON));
insert into test.t0018 (q0) values ('32767');
insert into test.t0018 (q0) values ('32768');
insert into test.t0018 (q0) values ('-32768');
insert into test.t0018 (q0) values ('-32769');
insert into test.t0018 (q0) values ('2147483647');
insert into test.t0018 (q0) values ('2147483648');
insert into test.t0018 (q0) values ('-2147483648');
insert into test.t0018 (q0) values ('-2147483649');
insert into test.t0018 (q0) values ('18446744073709551615');
insert into test.t0018 (q0) values ('18446744073709551616');
insert into test.t0018 (q0) values ('3.14');
insert into test.t0018 (q0) values ('{}');
insert into test.t0018 (q0) values ('[]');
-- insert into test.t0018 (q0) values (CAST(CAST('2015-01-15 23:24:25' AS DATETIME) AS JSON));
-- insert into test.t0018 (q0) values (CAST(CAST('23:24:25' AS TIME) AS JSON));
-- insert into test.t0018 (q0) values (CAST(CAST('2015-01-15' AS DATE) AS JSON));
-- insert into test.t0018 (q0) values (CAST(TIMESTAMP'2015-01-15 23:24:25' AS JSON));
-- insert into test.t0018 (q0) values (CAST(ST_GeomFromText('POINT(1 1)') AS JSON));

# auto-convert to utf8mb4
insert into test.t0018 (q0) values (CAST('[]' AS CHAR CHARACTER SET 'ascii'));
-- insert into test.t0018 (q0) values (CAST(x'cafe' AS JSON));
-- insert into test.t0018 (q0) values (CAST(x'cafebabe' AS JSON));

# Maximum allowed key length is 64k-1
insert into test.t0018 (q0) values (CONCAT('{"', REPEAT('a', 64 * 1024 - 1), '":123}'));
