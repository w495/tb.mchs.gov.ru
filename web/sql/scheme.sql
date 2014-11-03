create sequence seq_customer_id;
create table customer(
    id int primary key default nextval('seq_customer_id'),
    email varchar(1024) unique,
    login varchar(1024) not null unique,
    pic_url text, /* заглавная картинка для новости */
    city varchar(1024),
    organization varchar(1024),
    position varchar(1024),
    firstname varchar(1024) not null,
    lastname varchar(1024) not null,
    patronimic varchar(1024) not null,
    deleted bool default false,
    birthday date,
    password_hash char(32) not null
);

create sequence seq_customer_group_id;
create table customer_group (
    id int primary key default nextval('seq_customer_group_id'),
    name varchar(1024),
    description varchar(1024),
    deleted bool default false
);

create sequence seq_permission_type;
create table permission_type (
    id int primary key default nextval('seq_permission_type'),
    name varchar(1024) unique
);

create sequence seq_permission;
create table permission (
    id int primary key default nextval('seq_permission'),
    perm_type_id int references permission_type(id),
    entity_id int,
    name varchar(1024),
    description varchar(1024),
    type int
);

create table permission2group (
    perm_id int references permission(id) not null,
    group_id int references customer_group(id) not null
);

create table customer2group (
    customer_id int references customer(id) not null,
    group_id int references customer_group(id) not null
);

create sequence seq_doc_type;
create table doc_type(
    id int primary key default nextval('seq_doc_type'),
    name varchar(1024)
);

create sequence seq_document;
create table document (
    id int primary key default nextval('seq_document'),
    name varchar(1024),
    content text,
    datatime timestamp without time zone default now(),
    published bool,
    updater int,
    pic_url text, /* заглавная картинка для новости */
    pdf_url text,
    msw_url text,
    dir_id int,
    doc_type_id int references doc_type(id)
);


create sequence seq_dir_type;
create table dir_type (
    id int primary key default nextval('seq_dir_type'),
    name varchar(1024)
);

create sequence seq_directory;
create table directory (
    id int primary key default nextval('seq_directory'),
    name varchar(1024),
    parent_dir_id int references directory(id),
    datatime timestamp without time zone default now(),
    updater int,
    doc_description_id int references document(id),
    dir_type_id int references dir_type(id),
    deleted bool
);

alter table document add constraint document_dir_id_fkey
	foreign key (dir_id) references directory(id);

create sequence seq_attach_type;
create table attach_type (
    id int primary key default nextval('seq_attach_type'),
    name varchar(1024)
);

create sequence seq_attach2doc;
create table attach2doc (
    id int primary key default nextval('seq_attach2doc'),
    name varchar(1024),
    alt text,
    doc_id int references document(id),
    attach_type_id int references attach_type(id),
    url text,
    updater int
);


/* ТЕСТЫ */

create sequence seq_test2customer;
create table test2customer (
    id int primary key default nextval('seq_test2customer'),
    customer_id int references customer(id),
    test_id int references directory(id),
    result int,
    datetime timestamp without time zone default now()
);

create table quest_answer (
    answer_id int references document(id),
    correct_flag bool
);

/* КОНФЕРНЕЦИИ */

create table conference (
    conf_id int references directory(id),
    start date,
    stop date
);

create table conf_quest_subscriber (
    conf_quest_id int references directory(id),
    customer_id int references customer(id)
);


/* ЛОГИРОВАНИЕ */

create sequence seq_log_object_type;
create table log_object_type (
    id int primary key default nextval('seq_log_object_type'),
    name varchar(50) unique,
    alias varchar(50)
);


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



create table log_action (
    log_id int references log(id),
    log_action_type_id int references log_action_type(id),
    field_name varchar(50),
    old_value text,
    new_value text
);


/* БАННЕРЫ */

create sequence seq_banner_place;
create table banner_place (
    id int primary key default nextval('seq_banner_place'),
    name varchar(30) unique,
    alias varchar(100)
);


create sequence seq_adv_com;
create table adv_com (
    id int primary key default nextval('seq_adv_com'),
    name varchar(1024),
    datestart timestamp without time zone,
    datestop timestamp without time zone,
    banner_place_id int references banner_place(id),
    url varchar(200),
    ref varchar(1000)
);


create sequence seq_inf_src;
create table inf_src (
    id int primary key default nextval('seq_inf_src'),
    name varchar(1024),
    update_datetime timestamp without time zone,
    regexp text,
    value text,
    url varchar(200)
);

create sequence seq_game_map;
create table game_map (
    id int primary key default nextval('seq_game_map'),
    name varchar(1024),
    data text
);
