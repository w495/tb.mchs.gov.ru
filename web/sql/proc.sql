create trusted procedural language 'plpgsql';

/**
 * Создает корневую папку указанного типа.
 * updater = -1 --- создано при создании базы.
**/

create or replace function create_root_dir
(
	_dir_name varchar(100),
	_dir_type varchar(50)
)
returns void as
$body$
declare
	__dir_id integer;
	__doc_id integer;
begin
	/* Создадим директорию */
	insert into directory (name, parent_dir_id, datatime, updater,
		doc_description_id, dir_type_id, deleted)
		values (
			_dir_name,
			lastval(),
			now(),
			-1,
			null,
			(select id from dir_type where name=_dir_type),
			false
		);
	/* dir_id := directory.id */
	__dir_id := lastval();


	/* Создадим описание директории */
	insert into document ( name, content, dir_id, doc_type_id,
		updater, published)
		values (
			_dir_name,
			_dir_name,
			__dir_id,
			(select id from doc_type where name='descr'),
			-1,
			true
		);

	/* doc_id := document.id */
	__doc_id := lastval();

	update directory set doc_description_id = __doc_id where id = __dir_id;
end;
$body$
  language 'plpgsql';


/**
 * Создает жесткую папку указанного типа c указателем родителя.
 * updater = -1 --- создано при создании базы.
**/

create or replace function create_hard_dir_by_id
(
	_dir_name varchar(100),
	_dir_type varchar(50),
	_parent_id integer
)
returns void as
$body$
declare
	__dir_id integer;
	__doc_id integer;
begin
	/* Создадим директорию */
	insert into directory (name, parent_dir_id, datatime, updater,
		doc_description_id, dir_type_id, deleted)
		values (
			_dir_name,
			_parent_id,
			now(),
			-1,
			null,
			(select id from dir_type where name=_dir_type),
			false
		);
	/* dir_id := directory.id */
	__dir_id := lastval();

	/* Создадим описание директории */
	insert into document ( name, content, dir_id, doc_type_id,
		updater, published)
		values (
			_dir_name,
			_dir_name,
			__dir_id,
			(select id from doc_type where name='descr'),
			-1,
			true
		);

	/* doc_id := document.id */
	__doc_id := lastval();

	update directory set doc_description_id = __doc_id where id = __dir_id;
end;
$body$
  language 'plpgsql';

/**
 * Создает жесткую папку указанного типа c указателем родителя.
 * updater = -1 --- создано при создании базы.
**/

create or replace function create_hard_dir_by_name
(
	_dir_name varchar(100),
	_dir_type varchar(50),
	_parent_name varchar(100)
)
returns void as
$body$
declare
	__dir_id integer;
	__doc_id integer;
begin
	/* Создадим директорию */
	insert into directory (name, parent_dir_id, datatime, updater,
		doc_description_id, dir_type_id, deleted)
		values (
			_dir_name,
			(select pd.id from directory as pd where pd.name = _parent_name),
			now(),
			-1,
			null,
			(select id from dir_type where name=_dir_type),
			false
		);
	/* dir_id := directory.id */
	__dir_id := lastval();

	/* Создадим описание директории */
	insert into document ( name, content, dir_id, doc_type_id,
		updater, published)
		values (
			_dir_name,
			_dir_name,
			__dir_id,
			(select id from doc_type where name='descr'),
			-1,
			true
		);

	/* doc_id := document.id */
	__doc_id := lastval();

	update directory set doc_description_id = __doc_id where id = __dir_id;
end;
$body$
  language 'plpgsql';



/**
 * Создает жесткую папку указанного типа c указателем родителя.
 * updater = -1 --- создано при создании базы.
**/

create or replace function create_hard_dir
(
	_dir_name varchar(100),
	_parent_name varchar(100)
)
returns void as
$body$
declare
	__dir_id integer;
	__doc_id integer;
begin
	/* Создадим директорию */
	insert into directory (name, parent_dir_id, datatime, updater,
		doc_description_id, dir_type_id, deleted)
		values (
			_dir_name,
			(select pd.id from directory as pd where pd.name = _parent_name),
			now(),
			-1,
			null,
			(select pd.dir_type_id from directory as pd where pd.name = _parent_name),
			false
		);
	/* dir_id := directory.id */
	__dir_id := lastval();

	/* Создадим описание директории */
	insert into document ( name, content, dir_id, doc_type_id,
		updater, published)
		values (
			_dir_name,
			_dir_name,
			__dir_id,
			(select id from doc_type where name='descr'),
			-1,
			true
		);

	/* doc_id := document.id */
	__doc_id := lastval();

	update directory set doc_description_id = __doc_id where id = __dir_id;
end;
$body$
  language 'plpgsql';
