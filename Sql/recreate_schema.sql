DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

CREATE TABLE participant
(
	participant_id serial PRIMARY KEY,
	participant_token char(4) UNIQUE
);

CREATE TABLE section
(
  section_id serial PRIMARY KEY,
  section_sort integer NOT NULL DEFAULT -1,
  section_title text NOT NULL
);

CREATE INDEX ON section(section_sort);

CREATE TABLE qgroup
(
	qgroup_id serial PRIMARY KEY,
	qgroup_sort integer NOT NULL DEFAULT -1,
	qgroup_header text NOT NULL,
	qgroup_scale varchar(255) NOT NULL,
	qgroup_questiondisplay varchar(255) NOT NULL,
    qgroup_freefield varchar(255) NOT NULL,
	qgroup_section_id integer NOT NULL,

	FOREIGN KEY (qgroup_section_id)
		REFERENCES section (section_id)
);

CREATE INDEX ON qgroup(qgroup_sort);

CREATE TABLE question
(
	question_id serial PRIMARY KEY,
	question_sort integer NOT NULL DEFAULT -1,
	question_text text NOT NULL,
	question_qgroup_id integer NOT NULL,

	FOREIGN KEY (question_qgroup_id)
		REFERENCES qgroup (qgroup_id)
);

CREATE INDEX ON question(question_sort);

CREATE TABLE rating
(
	rating_id serial PRIMARY KEY,
	rating_sort integer NOT NULL DEFAULT -1,
	rating_value text NOT NULL,
	rating_dev_given boolean NOT NULL,
	rating_qgroup_id integer NOT NULL,

	UNIQUE (rating_qgroup_id, rating_value),
	FOREIGN KEY (rating_qgroup_id)
		REFERENCES qgroup (qgroup_id)
);

CREATE INDEX ON rating(rating_sort);

CREATE TABLE answer
(
	answer_id serial PRIMARY KEY,
	answer_rating_id integer NOT NULL,
	answer_question_id integer NOT NULL,
	answer_participant_id integer NOT NULL,

	FOREIGN KEY (answer_participant_id)
		REFERENCES participant (participant_id),
	FOREIGN KEY (answer_question_id)
		REFERENCES question (question_id),
	FOREIGN KEY (answer_rating_id)
		REFERENCES rating (rating_id)
);

CREATE FUNCTION survey(
    section_qgroup_questionrc refcursor,
    qtype_qgrouprc refcursor,
    qgroup_ratingrc refcursor)
  RETURNS SETOF refcursor AS
$BODY$
BEGIN
	OPEN section_qgroup_questionRC FOR
	SELECT * FROM section
	INNER JOIN qgroup ON qgroup.qgroup_section_id = section.section_id
	INNER JOIN question ON question.question_qgroup_id = qgroup.qgroup_id
	ORDER BY section_sort, qgroup_sort, question_sort;
	RETURN NEXT section_qgroup_questionRC;

	OPEN qtype_qgroupRC FOR
	SELECT * FROM qtype
	INNER JOIN qgroup ON qgroup_qtype_id = qtype_id
	INNER JOIN section ON section_id = qgroup_section_id
	ORDER BY section_sort, qgroup_sort;
	RETURN NEXT qtype_qgroupRC;

	OPEN qgroup_ratingRC FOR
	SELECT * FROM section
	INNER JOIN qgroup ON qgroup_section_id = section_id
	INNER JOIN rating ON rating_qgroup_id = qgroup_id
	ORDER BY section_sort, qgroup_sort, rating_sort;
	RETURN NEXT qgroup_ratingRC;
	RETURN;
END
$BODY$ LANGUAGE plpgsql;

CREATE FUNCTION make_participant() RETURNS SETOF participant AS
$BODY$
DECLARE
    new_token char(4);
    done bool;
BEGIN
    done := false;
    WHILE NOT done LOOP
        new_token := substring((md5(''||now()::text||random()::text)) for 4);
        done := NOT EXISTS(
		SELECT 1 FROM participant
		WHERE participant_token = new_token);
    END LOOP;
    RETURN QUERY
    INSERT INTO participant(participant_token)
	VALUES (new_token) RETURNING *;
END;
$BODY$ LANGUAGE plpgsql;

CREATE FUNCTION section_id_by_sort(sort integer) RETURNS integer AS
$BODY$
	SELECT section_id FROM section WHERE section_sort = sort;
$BODY$ LANGUAGE SQL;

CREATE FUNCTION qgroup_id_by_sort(sort integer, v_section_id integer) RETURNS integer AS
$BODY$
	SELECT qgroup_id FROM section
	INNER JOIN qgroup ON qgroup.qgroup_section_id = section.section_id
	WHERE section_id = v_section_id
	AND qgroup_sort = sort;
$BODY$ LANGUAGE SQL;
