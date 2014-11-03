/**
 * Сustomers
 *
**/

insert into permission_type (name) values ('static-test');

insert into permission_type (name)
	values
		('static'),
		('expert_conf'),
		('dir');

insert into permission (name, description, perm_type_id)
	values
		('admin', 'полный доступ',
			(select id from permission_type where name='static')),
		('w_conference', 'редактировать конференции',
			(select id from permission_type where name='static')),
 		('w_test', 'редактировать тесты',
			(select id from permission_type where name='static')),
		('w_user', 'редактировать пользователей',
			(select id from permission_type where name='static'));

insert into customer_group (name, description)
	values
		('admin', 	'администраторы'),
		('expert', 	'эксперты'),
		('all', 	'все пользователи');

insert into permission2group (perm_id, group_id)
	values
		( (select id from permission where name='admin'),
			(select id from customer_group where name='admin'));

insert into customer (firstname, lastname, patronimic, login, password_hash)
	values ('---', '---', '---', 'admin', '21232F297A57A5A743894A0E4A801FC3');

insert into customer2group (customer_id, group_id)
	values
		((select id from customer where login='admin'),
			(select id from customer_group where name='admin')),
		((select id from customer where login='admin'),
			(select id from customer_group where name='all'));

/**
 * DOCS
 *
**/

insert into doc_type (name)
	values
        ('descr'),
        ('doc'),
        ('conf_answ');

insert into attach_type (name)
	values
		('file'),
		('video'),
		('image'),
		('link');

insert into dir_type (name)
	values ('news'),
		('subdir'),
		('key'),
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

/**
	Вызываем хранимку для создания корней.
	Базовые папки прошитые жестко.
**/

/* ПАПКИ ПЕРВОГО УРОВНЯ */

select create_root_dir('/', 						    'key');
select create_root_dir('Новости', 						'news');
select create_root_dir('Нормативные акты', 				'subdir');
select create_root_dir('Безопасность на транспорте', 	'subdir');
select create_root_dir('Конференции', 					'root_conference');
select create_root_dir('Тесты', 						'root_test');
select create_root_dir('Термины', 						'term');

/* ПАПКИ ВТОРОГО УРОВНЯ: Термины */

select create_hard_dir('Воздушные термины', 'Термины');
select create_hard_dir('Водные термины', 	'Термины');
select create_hard_dir('Наземные термины',  'Термины');

/* ПАПКИ ВТОРОГО УРОВНЯ: Норматиыные акты */

select create_hard_dir('Головная страница',  					'/');
select create_hard_dir('Воздушный трансаорт',  					'/');
select create_hard_dir('Водный трансаорт',  					'/');
select create_hard_dir('Наземный трансаорт',  					'/');


/* ПАПКИ ВТОРОГО УРОВНЯ: Тесты */

select create_hard_dir_by_name('Воздушные тесты',  	'root_test_air',		'Тесты');
select create_hard_dir_by_name('Водные тесты',  	'root_test_water',		'Тесты');
select create_hard_dir_by_name('Наземные тесты',  	'root_test_surface',	'Тесты');



select create_hard_dir('Нормативно-правовые документы',  					'Нормативные акты');
select create_hard_dir('Указы президентa РФ',  			 					'Нормативные акты');
select create_hard_dir('Постановления правительства РФ', 					'Нормативные акты');
select create_hard_dir('Нормативно-правовые акты министерства',  			'Нормативные акты');
select create_hard_dir('Мониторинг законодательства',  						'Нормативные акты');
select create_hard_dir('Экспертиза проектов документов',  					'Нормативные акты');
select create_hard_dir('Нормативно-правовые акты министерств и ведомств',  	'Нормативные акты');
select create_hard_dir('Федеральные конституционные законы',  				'Нормативные акты');
select create_hard_dir('Докуметы МЧС в системе "Гарант"',  					'Нормативные акты');
select create_hard_dir('Порядок обжалования нормативных правовых актов',  	'Нормативные акты');
select create_hard_dir('Мероприятия проводимые по реализации ФЗ №83',  		'Нормативные акты');


select create_hard_dir('Военные и транспорт',  									'Безопасность на транспорте');
select create_hard_dir('Транспорт и экология',  								'Безопасность на транспорте');
select create_hard_dir('Протекционизм и таможни',  								'Безопасность на транспорте');
select create_hard_dir('Организация движения',  								'Безопасность на транспорте');
select create_hard_dir('Организация транспортного регулирования',  				'Безопасность на транспорте');
select create_hard_dir('Либерализация транспорта',  							'Безопасность на транспорте');
select create_hard_dir('Экономическая теория о транспортном регулировании',  	'Безопасность на транспорте');
select create_hard_dir('Транспортная инфраструктура',  							'Безопасность на транспорте');
select create_hard_dir('Пространство движения',  								'Безопасность на транспорте');
select create_hard_dir('Перспективные транспортные технологии',  				'Безопасность на транспорте');
select create_hard_dir('Человеческий фактор в безопасности транспорта',  		'Безопасность на транспорте');


/* ЛОГИРОВАНИЕ */

insert into log_object_type (name, alias) values ('directory', 'директория'), ('document', 'документ'), ('test', 'тест'), ('test_answer', 'ответ теста'), ('attach', 'аттач');
insert into log_action_type (name) values ('create'), ('delete'), ('update');

/* БАННЕРЫ */

insert into banner_place (name, alias) values ('right', 'справа'), ('bottom', 'снизу');