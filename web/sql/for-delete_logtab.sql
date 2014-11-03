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
