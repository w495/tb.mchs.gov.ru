create table d_type (
    name varchar(50) unique
);

insert into d_type (name)
        values  ('news'),
                ('subdir'),
                ('key'),
                ('norm_acts'),
                ('transp_safety'),
                ('root_test'),
                    ('root_test_surface'),
                    ('root_test_air'),
                    ('root_test_water'),
                ('test'),
                    ('test_question'),
                ('root_conference'),
                    ('conference'),
                    ('conf_question'),
                    ('conf_answ'),
                ('term'),
                    ('term_air'),
                    ('term_surface'),
                    ('term_water');
create sequence seq_doc;
create table doc (
    id int primary key default nextval('seq_doc'),
    name varchar(1024),
    datatime timestamp without time zone default now(),
    doc_type_name varchar(50) references d_type(name),
    content text,
    published bool default false,
    deleted bool default false,
    pic_url text,
    pdf_url text,
    msw_url text,
    idx int
);

create table doc2doc (
    parent_doc_id int references doc(id),
    child_doc_id int references doc(id),
    UNIQUE (parent_doc_id, child_doc_id)
);




CREATE INDEX attach2doc_doc_id_idx ON attach2doc(doc_id);

create sequence seq_log_object_type;
create table log_object_type (
    id int primary key default nextval('seq_log_object_type'),
    name varchar(50) unique,
    alias varchar(50)
);

insert into log_object_type (name, alias) values ('directory', 'директория'), ('document', 'документ'), ('test', 'тест'), ('test_answer', 'ответ теста'), ('attach', 'аттач');


create sequence seq_log;
create table log (
    id int primary key default nextval('seq_log'),
    log_object_id int,
    log_object_type_id int references log_object_type(id),
    datetime timestamp without time zone default now(),
    customer_id int references customer(id)
);

create sequence seq_log_action_type;
create table log_action_type (
    id int primary key default nextval('seq_log_object_type'),
    name varchar(50) unique
);

insert into log_action_type (name) values ('create'), ('delete'), ('update');

create table log_action (
    log_id int references log(id),
    log_action_type_id int references log_action_type(id),
    field_name varchar(50),
    old_value text,
    new_value text
);

// ----------------------------------------------------------------------------

create sequence seq_banner_place;
create table banner_place (
    id int primary key default nextval('seq_banner_place'),
    name varchar(30) unique,
    alias varchar(100)
);

insert into banner_place (name, alias) values ('right', 'справа'), ('bottom', 'снизу');

create sequence seq_adv_com;
create table adv_com (
    id int primary key default nextval('seq_adv_com'),
    name varchar(100),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    banner_place_id int references banner_place(id),
    url varchar(200),
    ref varchar(1000)
);

create table web_encoding(
    name varchar(30) unique
);

insert into web_encoding (name) values ('utf8'), ('windows-1251'), ('koi8-r');


create sequence seq_inf_src;
create table inf_src (
    id int primary key default nextval('seq_inf_src'),
    name varchar(100),
    update_datetime timestamp without time zone,
    regexp text,
    value text,
    url varchar(200),
    encoding varchar(30) references web_encoding(name)
);

create sequence seq_game_map;
create table game_map (
    id int primary key default nextval('seq_game_map'),
    name varchar(100),
    data text
);

