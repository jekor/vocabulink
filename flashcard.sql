CREATE TABLE card (
       card_no SERIAL PRIMARY KEY,
       username varchar(64) NOT NULL,
       card_name varchar(64) NOT NULL,
       question text,
       answer text,
       UNIQUE (username, card_name)
);