﻿TRUNCATE answer CASCADE;
TRUNCATE participant CASCADE;
TRUNCATE qgroup CASCADE;
TRUNCATE question CASCADE;
TRUNCATE rating CASCADE;
TRUNCATE section CASCADE;

INSERT INTO section(section_sort, section_title)
	VALUES(1, 'Programmiersprache');

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_questiondisplay, qgroup_freefield, qgroup_section_id)
	VALUES(1, '', 'NominalScale', 'RadioInline', 'NoFreeField', section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(1, section_id_by_sort(1)) FROM (VALUES
	(0, 'C++', true),
    (0, 'C#', true),
    (0, 'F#', true),
    (0, 'Java', true),
    (0, 'Scala', true),
    (0, 'Clojure', true),
    (0, 'OCaml', true),
    (0, 'Haskell', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Which programming language do you prefer?',
	qgroup_id_by_sort(1, section_id_by_sort(1)));
