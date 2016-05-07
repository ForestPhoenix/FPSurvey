TRUNCATE answer CASCADE;
TRUNCATE participant CASCADE;
TRUNCATE qgroup CASCADE;
TRUNCATE qtype CASCADE;
TRUNCATE question CASCADE;
TRUNCATE rating CASCADE;
TRUNCATE section CASCADE;

INSERT INTO qtype(qtype_id, qtype_is_freeform, qtype_display_variant) VALUES
	(1, false, 'dropdown'),
	(2, false, 'radio'), 
	(3, true, 'integer_input'),
	(4, true, 'radio_with_text_input'),
	(5, true, 'text_input'),
	(6, true, 'radio_with_integer_input'),
	(7, false, 'radio_inline');

INSERT INTO section(section_sort, section_title)
	VALUES(1, 'Personelle Fragen');

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(1, '', 'nominal', 1, section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id) 
	SELECT *, qgroup_id_by_sort(1, section_id_by_sort(1)) FROM (VALUES
	(0, 'AG', true),
	(0, 'AMG', true),
	(0, 'AO', true),
	(0, 'BmA', true),
	(0, 'BOS', true),
	(0, 'FA', true),
	(0, 'FM', true),
	(0, 'FO', true),
	(0, 'FOS', true),
	(0, 'FS', true),
	(0, 'FSC', true),
	(0, 'FSZ', true),
	(0, 'GS', true),
	(0, 'ITA', true),
	(0, 'ITGI', true),
	(0, 'MTS', true),
	(0, 'OG', true),
	(0, 'OM', true),
	(0, 'SE', true),
	(0, 'TAM', true),
	(0, 'TPD', true),
	(0, 'TSP', true),
	(0, 'TSPS', true),
	(0, 'W', true),
	(0, 'zv', true),
	(1, 'keine Angabe', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Welchen Bildungsgang belegst du ?', 
	qgroup_id_by_sort(1, section_id_by_sort(1)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(2, '', 'nominal', 7, section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(2, section_id_by_sort(1)) FROM (VALUES
	(0, 'Männlich', true),
	(0, 'Weiblich', true),
	(1, 'keine Angabe', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Welches Geschlecht hast du?', 
	qgroup_id_by_sort(2, section_id_by_sort(1)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(3, '', 'metric', 6, section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(3, section_id_by_sort(1)) FROM (VALUES
	(1, 'keine Angabe', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Wie alt sind Sie?', 
	qgroup_id_by_sort(3, section_id_by_sort(1)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(4, '', 'nominal', 7, section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(4, section_id_by_sort(1)) FROM(VALUES
	(0, 'Auto/ Öffentliche Verkehrsmittel', true),
	(0, 'Fahrrad', true),
	(0, 'zu Fuß', true),
	(1, 'keine Angabe', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Wie kommst du zur Schule ?', 
	qgroup_id_by_sort(4, section_id_by_sort(1)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(5, '', 'nominal', 4, section_id_by_sort(1));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
SELECT *, qgroup_id_by_sort(5, section_id_by_sort(1)) FROM (VALUES
	(0, 'Deutsch', true),
	(0, 'Englisch', true),
	(0, 'Türkisch', true),
	(0, 'Französisch', true),
	(0, 'Spanisch', true),
	(1, 'keine Angabe', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Welche Sprache ist Ihre Muttersprache?', 
	qgroup_id_by_sort(5, section_id_by_sort(1)));

------------- SECTION 2 -----------

INSERT INTO section(section_sort, section_title)
	VALUES(2, 'Einschätzung Schule');

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(1, 'Wie zufieden sind Sie mit', 'ordinal', 2, section_id_by_sort(2));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(1, section_id_by_sort(2)) FROM (VALUES
	(1, 'Unzufrieden', true),
	(2, 'Eher unzufriedem', true),
	(3, 'Eher zufrieden', true),
	(4, 'Zufrieden', true),
	(5, 'Kann ich nicht beurteilen', true)) AS tmp;
	-- TODO: use temporary table (as in ratings)
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'der technischen Ausstattung/ den Arbeitsmitteln der Schule?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(2, 'der Sauberkeit von der Schule allgemein?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(3, 'der Sauberkeit von der Cafeteria und Mensa?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(4, 'der Sauberkeit der Sanitäreinrichtungen?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(5, 'der Sauberkeit der Klassenräume?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(6, 'der Sauberkeit der Arbeitsgeräte?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(7, 'der Sauberkeit der Flure?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(8, 'der Sauberkeit der Sporthalle?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(9, 'der Sauberkeit der Außenbereiche?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(10, 'der Ausstattung der sanitären Einrichtungen in der Schule?', 
	qgroup_id_by_sort(1, section_id_by_sort(2)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(2, 'Wie sicher fühlen Sie sich ', 'ordinal', 2, section_id_by_sort(2));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(2, section_id_by_sort(2)) FROM (VALUES
	(1, 'Unsicher', true),
	(2, 'Eher unsicher', true),
	(3, 'Eher sicher', true),
	(4, 'Sicher', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'auf dem Schulgelände allgemein', 
	qgroup_id_by_sort(2, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(2, 'auf dem Schulweg', 
	qgroup_id_by_sort(2, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(3, 'in der Cafeteria und Mensa', 
	qgroup_id_by_sort(2, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(4, 'im Klassenraum', 
	qgroup_id_by_sort(2, section_id_by_sort(2)));
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(5, 'in den Pausen', 
	qgroup_id_by_sort(2, section_id_by_sort(2)));

------------- SECTION 3 -----------

INSERT INTO section(section_sort, section_title)
	VALUES(3, 'Gestaltung und Durchführung des Unterrichts');

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(1, '', 'ordinal', 1, section_id_by_sort(3));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(1, section_id_by_sort(3)) FROM (VALUES
	(1, 'Unzufrieden', true),
	(2, 'Eher unzufriedem', true),
	(3, 'Eher zufrieden', true),
	(4, 'Zufrieden', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'Wie zufrieden sind Sie im Allgemeinen mit dem Unterricht?', 
	qgroup_id_by_sort(1, section_id_by_sort(3)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(2, '', 'ordinal', 1, section_id_by_sort(3));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(2, section_id_by_sort(3)) FROM (VALUES
	(1, '0', true),
	(2, '1', true),
	(3, '2', true),
	(4, '3', true),
	(5, '>3', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	VALUES(1, 'In durchschnittlich wie vielen Blöcken pro Woche ist kein Lehrer anwesend bzw. fällt der Unterricht aus?', 
	qgroup_id_by_sort(2, section_id_by_sort(3)));

INSERT INTO qgroup(qgroup_sort, qgroup_header, qgroup_scale, qgroup_qtype_id, qgroup_section_id)
	VALUES(3, 'Inwieweit treffen die folgenden Aussagen bezüglich des Unterrichtsausfalls auf Sie zu?',
	'ordnial' , 2, section_id_by_sort(3));
INSERT INTO rating(rating_sort, rating_value, rating_dev_given, rating_qgroup_id)
	SELECT *, qgroup_id_by_sort(3, section_id_by_sort(3)) FROM (VALUES
	(1, 'trifft nicht zu', true),
	(2, 'trifft eher nicht zu', true),
	(3, 'trifft eher zu', true),
	(4, 'trifft zu', true)) AS tmp;
INSERT INTO question(question_sort, question_text, question_qgroup_id)
	SELECT *, qgroup_id_by_sort(3, section_id_by_sort(3)) FROM (VALUES
	(1, 'Ich freue mich über den Unterrichtsausfall'),
	(2, 'Ich habe Stress durch das selbständige Nachholen des Unterrichtsstoffs'),
	(3, 'Ich habe Angst, die Prüfungen nicht zu bestehen'),
	(4, 'Ich bedaure den mangelnden Lernfortschritt'),
	(5, 'Der Unterrichtsausfall ist erholsam für mich'),
	(6, 'Der Unterrichtsausfall hat keinen Einfluss auf mich')) AS tmp;

------------- SECTION 4 -----------

INSERT INTO section(section_sort, section_title)
	VALUES(3, 'Lernerfahrung');

SELECT make_participant();
