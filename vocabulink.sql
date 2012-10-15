-- vocabulink.sql

-- I would use regexp checks and domains at the database level if they worked
-- for unicode characters. Instead, I'll leave it up to the Haskell layer to do
-- verification.

CREATE TABLE member (
       member_no SERIAL PRIMARY KEY,
       username CHARACTER VARYING(24) NOT NULL UNIQUE,
       join_date TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       email TEXT,
       password_hash TEXT
);
CREATE INDEX member_username ON member (username);
CREATE INDEX member_email ON member (email);
COMMENT ON TABLE member IS 'At some point in time we may want to store a member''s timezone and locale so that we can report dates and times accurately to them. For example, "Your next review is scheduled on November 3rd at 10:48." For now we just use relative times ("Your next review is scheduled for 18 hours from now.").';
COMMENT ON COLUMN member.member_no IS 'I debated about using the username as a primary key. In the end, I decided against it. Performance and space concerns won me over. It''s also nice to have a numeric reference to a member in case we need to refer to a member in some context where Unicode characters aren''t valid.';
COMMENT ON COLUMN member.username IS 'I want to allow usernames to include any URL-safe characters, including alphanumeric Unicode characters. The regexp for username is ^[\p{L}\p{Nd}$-_.+!*''(),]{3,}$. Because usernames will be used in URLs, and displayed in various places, I want to keep them to a reasonable size. This was also a motivation for making the difficult decision to disallow spaces. All of the preceding is outdated, we allow any characters in the username.';
COMMENT ON COLUMN member.email IS 'The email address here represents confirmed email addresses. If the member has not been confirmed, their email address lives in the member_confirmation relation.';
COMMENT ON COLUMN member.password_hash IS 'The member''s password is stored as a hash, calculated by the pgcrypto contrib functions. See /usr/share/postgresql/contrib/pgcrypto.sql. The actual function used is crypt(?, gen_salt(''bf'')) (bf is for blowfish). A password is not required so that non-members can interact to some extend with the site (such as posting comments). The idea is that someone can provide an email address and confirm it each time if they just want to comment on blog postings.';

INSERT INTO member (member_no, username, password_hash)
VALUES (0, 'anonymous', '');

CREATE TABLE member_confirmation (
       member_no INTEGER REFERENCES member (member_no) NOT NULL PRIMARY KEY ON DELETE CASCADE,
       hash TEXT NOT NULL,
       email TEXT NOT NULL,
       email_sent TIMESTAMP (0) WITH TIME ZONE
);
COMMENT ON TABLE member_confirmation IS 'This allows us to keep track of in-progress member confirmations. A tuple exists for each unconfirmed member.';
COMMENT ON COLUMN member_confirmation.hash IS 'This is a random hash that we can email to the user (in the form of a link) to ensure that they''ve actually received the confirmation email. It should be random so that it''s not guessable';
COMMENT ON COLUMN member_confirmation.email_sent IS 'email_sent is the time a confirmation email was successfully sent (or at least when our MTA says it was sent).';

CREATE TABLE password_reset_token (
       member_no INTEGER REFERENCES member (member_no) NOT NULL PRIMARY KEY ON DELETE CASCADE,
       hash TEXT NOT NULL,
       expires TIMESTAMP (0) WITH TIME ZONE
);
COMMENT ON COLUMN password_reset_token.hash IS 'This is a random hash that we can email to the user (in the form of a link) to ensure that they''ve actually received the password reset email. It should be random so that it''s not guessable';

CREATE TABLE link (
       link_no SERIAL PRIMARY KEY,
       learn TEXT NOT NULL CHECK (length(foreign_phrase) > 0),
       known TEXT NOT NULL CHECK (length(familiar_phrase) > 0),
       learn_lang CHARACTER VARYING (3) REFERENCES language (abbr) NOT NULL ON UPDATE CASCADE,
       known_lang CHARACTER VARYING (3) REFERENCES language (abbr) NOT NULL ON UPDATE CASCADE,
       author INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       created TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       updated TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       deleted BOOLEAN NOT NULL DEFAULT FALSE
);
COMMENT ON TABLE link IS 'A link is an association between 2 words (or phrases) in a single direction.';
COMMENT ON COLUMN link.deleted IS 'We need a way to delete links, but we don''t want to destroy people''s review decks. This allows us to mark deleted links so that we don''t display them to people who aren''t already reviewing them and we can later sweep ones with no references.';
-- We're going to search by these often.
CREATE INDEX link_foreign_phrase_index ON link (foreign_phrase);
CREATE INDEX link_familiar_phrase_index ON link (familiar_phrase);

CREATE TABLE link_frequency_list (
       list_no SERIAL PRIMARY KEY,
       lang CHARACTER VARYING (3) REFERENCES language (abbr) NOT NULL ON UPDATE CASCADE,
       list_name TEXT NOT NULL,
       description TEXT NOT NULL,
       UNIQUE (lang, list_name)
);

CREATE TABLE link_frequency (
       link_no INTEGER REFERENCES link (link_no) NOT NULL ON DELETE CASCADE,
       list_no INTEGER REFERENCES link_frequency_list (list_no) NOT NULL,
       rank INTEGER NOT NULL,
       frequency REAL NOT NULL,
       PRIMARY KEY (link_no, list_no)
);

CREATE TABLE link_linkword (
       link_no INTEGER REFERENCES link (link_no) NOT NULL PRIMARY KEY ON DELETE CASCADE,
       linkword TEXT NOT NULL
);

CREATE TABLE link_soundalike (
       link_no INTEGER REFERENCES link (link_no) NOT NULL PRIMARY KEY ON DELETE CASCADE
);

CREATE TABLE linkword_story (
       story_no SERIAL PRIMARY KEY,
       link_no INTEGER REFERENCES link_type_linkword (link_no) NOT NULL ON DELETE CASCADE,
       author INTEGER REFERENCES member (member_no) NOT NULL ON DELETE CASCADE,
       created TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       edited  TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       story TEXT NOT NULL
);

CREATE TABLE link_to_review (
       member_no INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       link_no INTEGER REFERENCES link (link_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       target_time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       added_time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       PRIMARY KEY (member_no, link_no)
);
COMMENT ON COLUMN link_to_review.member_no IS 'Anonymous members cannot schedule reviews. That would be chaos. It''s also confusing if you hadn''t realized that you weren''t logged in.';
COMMENT ON COLUMN link_to_review.target_time IS 'Target is the date and time at which this link should come up for review. The link will be reviewed sometime after that. All new links for review are currently scheduled for immediate review.';

CREATE TABLE link_review (
       member_no INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       link_no INTEGER REFERENCES link (link_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       target_time TIMESTAMP (0) WITH TIME ZONE NOT NULL,
       actual_time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       recall_grade REAL NOT NULL,
       recall_time INTEGER NOT NULL,
       PRIMARY KEY (member_no, link_no, actual_time)
);
COMMENT ON COLUMN link_review.recall IS 'Recall is a measure of how easy or complete the memory of a link was. 1.0 is perfect recall. 0.0 means "no clue".';
COMMENT ON COLUMN link_review.recall_time IS 'Recall time is the amount of time (in milliseconds) taken to recall (or not) the destination of a link. It could be measured as the time between when the page is displayed and when the destination lexeme is shown (using JavaScript).';

CREATE TABLE link_sm2 (
       member_no INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       link_no INTEGER REFERENCES link (link_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       n SMALLINT NOT NULL DEFAULT 1,
       EF REAL NOT NULL DEFAULT 2.5,
       PRIMARY KEY (member_no, link_no)
);
COMMENT ON TABLE link_sm2 IS 'link_sm2 is used to track statistics for reviews based on SuperMemo algorithm 2 (SM-2).';
COMMENT ON COLUMN link_sm2.ef IS 'EF stands for "Easiness Factor".';
COMMENT ON COLUMN link_sm2.n IS 'This member is in review interval n. They may have reviewed the item more than n times, but n resets with a response lower than 3 in SM-2.';

-- Articles: Essays, Blog Posts, Disclaimers, etc. --

CREATE TABLE article (
       filename TEXT PRIMARY KEY,
       author INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       publish_time TIMESTAMP (0) WITH TIME ZONE NOT NULL,
       update_time TIMESTAMP (0) WITH TIME ZONE NOT NULL,
       section TEXT,
       title TEXT NOT NULL
);
COMMENT ON COLUMN article.publish_time IS 'A blog post is published at this time. If it''s before this time, the post is only visible to the owner.';

CREATE RULE "replace article" AS
    ON INSERT TO "article"
    WHERE EXISTS (SELECT TRUE FROM article WHERE filename = NEW.filename)
    DO INSTEAD (UPDATE article SET update_time = NEW.update_time,
                                   title = NEW.title,
                                   section = NEW.section
                WHERE filename = NEW.filename);

CREATE TABLE article_comment (
  filename TEXT REFERENCES article (filename) NOT NULL,
  root_comment INTEGER REFERENCES comment (comment_no) NOT NULL ON DELETE CASCADE,
  PRIMARY KEY (filename, root_comment)
);

CREATE FUNCTION create_article_root_comment() RETURNS trigger AS $$
BEGIN
  INSERT INTO article_comment (filename, root_comment)
                       VALUES (NEW.filename, create_virtual_root_comment());
  RETURN NEW;
END; $$ LANGUAGE plpgsql;

CREATE TRIGGER add_root_comment AFTER INSERT ON article FOR EACH ROW
EXECUTE PROCEDURE create_article_root_comment();

-- Comments

CREATE TABLE comment (
       comment_no SERIAL PRIMARY KEY,
       author INTEGER REFERENCES member (member_no) NOT NULL ON UPDATE CASCADE ON DELETE CASCADE,
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       body TEXT,
       parent_no INTEGER REFERENCES comment (comment_no) ON DELETE CASCADE
);

-- Some objects don't have a meaningful root comment. Instead, commenters
-- comment on the object itself. For those, we need a virtual root comment
-- since the comment structure is a tree.
CREATE FUNCTION create_virtual_root_comment() RETURNS BIGINT AS $$
  INSERT INTO comment (author) VALUES (0);
  SELECT currval('comment_comment_no_seq');
$$ LANGUAGE SQL;

CREATE TYPE displayable_comment AS (
       comment_no INTEGER,
       level INTEGER,
       username TEXT,
       email TEXT,
       time TIMESTAMP (0) WITH TIME ZONE,
       comment TEXT
);

CREATE FUNCTION comment_tree(INTEGER) RETURNS SETOF displayable_comment AS $$
  SELECT c.comment_no, t.level, m.username, m.email, c.time, c.body
  FROM comment c, member m,
       connectby('comment', 'comment_no', 'parent_no', $1::TEXT, 0)
       AS t(comment_no int, parent_no int, level int)
  WHERE c.comment_no = t.comment_no
    AND m.member_no = c.author AND c.body IS NOT NULL
$$ LANGUAGE SQL;

CREATE FUNCTION comment_root(INTEGER) RETURNS INTEGER AS $$
DECLARE
  r comment%rowtype;
BEGIN
  SELECT * INTO r FROM comment WHERE comment_no = $1;
  IF r.parent_no IS NOT NULL THEN
    RETURN comment_root(r.parent_no);
  ELSE
    RETURN r.comment_no;
  END IF;
END; $$ LANGUAGE plpgsql;

CREATE TABLE link_comment (
       link_no INTEGER REFERENCES link (link_no) NOT NULL ON DELETE CASCADE,
       root_comment INTEGER REFERENCES comment (comment_no) NOT NULL ON DELETE CASCADE,
       PRIMARY KEY (link_no, root_comment)
);

CREATE FUNCTION create_link_root_comment() RETURNS trigger AS $$
BEGIN
  INSERT INTO link_comment (link_no, root_comment)
                    VALUES (NEW.link_no, create_virtual_root_comment());
  RETURN NEW;
END; $$ LANGUAGE plpgsql;

CREATE TRIGGER add_root_comment AFTER INSERT ON link FOR EACH ROW
EXECUTE PROCEDURE create_link_root_comment();

-- What are the most popular languages on vocabulink? Count the number of times
-- the language appears on either side of a link to find out.
CREATE VIEW language_frequency AS
SELECT lang.abbr, lang.name, SUM(t.freq) AS freq FROM
((SELECT origin_language AS abbr, COUNT(*) AS freq
 FROM link
 WHERE NOT deleted
 GROUP BY origin_language) UNION
(SELECT destination_language AS abbr, COUNT(*) AS freq
 FROM link
 WHERE NOT deleted
 GROUP BY destination_language)) AS t
INNER JOIN language lang USING (abbr)
GROUP BY lang.abbr, lang.name;

CREATE VIEW language_frequency_to_english AS
SELECT lang.abbr, lang.name, COUNT(*) AS freq
FROM link
INNER JOIN language lang ON (lang.abbr = link.origin_language)
WHERE NOT deleted AND destination_language = 'en'
GROUP BY lang.abbr, lang.name;

CREATE TABLE language (
       abbr CHARACTER VARYING (3) PRIMARY KEY,
       name TEXT UNIQUE NOT NULL
);
COMMENT ON COLUMN language.abbr IS 'We use 2-letter language codes (ISO 639-1) when available and 3-letter codes for languages that don''t have a 2-letter code (the notable case of which is Lojban which any self-respecting language-learning site should support.';
INSERT INTO language (abbr, name) VALUES
('aa','Afar'),
('ab','Abkhazian'),
('ae','Avestan'),
('af','Afrikaans'),
('ak','Akan'),
('am','Amharic'),
('an','Aragonese'),
('ar','Arabic'),
('as','Assamese'),
('av','Avaric'),
('ay','Aymara'),
('az','Azerbaijani'),
('ba','Bashkir'),
('be','Belarusian'),
('bg','Bulgarian'),
('bh','Bihari'),
('bi','Bislama'),
('bm','Bambara'),
('bn','Bengali'),
('bo','Tibetan'),
('br','Breton'),
('bs','Bosnian'),
('ca','Catalan'),
('ce','Chechen'),
('ch','Chamorro'),
('co','Corsican'),
('cr','Cree'),
('cs','Czech'),
('cu','Church Slavic'),
('cv','Chuvash'),
('cy','Welsh'),
('da','Danish'),
('de','German'),
('dv','Divehi'),
('dz','Dzongkha'),
('ee','Ewe'),
('el','Greek'),
('en','English'),
('eo','Esperanto'),
('es','Spanish'),
('et','Estonian'),
('eu','Basque'),
('fa','Persian'),
('ff','Fulah'),
('fi','Finnish'),
('fj','Fijian'),
('fo','Faroese'),
('fr','French'),
('fy','Western Frisian'),
('ga','Irish'),
('gd','Scottish Gaelic'),
('gl','Galician'),
('gn','Guaraní'),
('gu','Gujarati'),
('gv','Manx'),
('ha','Hausa'),
('he','Hebrew'),
('hi','Hindi'),
('ho','Hiri Motu'),
('hr','Croatian'),
('ht','Haitian'),
('hu','Hungarian'),
('hy','Armenian'),
('hz','Herero'),
('ia','Interlingua'),
('id','Indonesian'),
('ie','Interlingue'),
('ig','Igbo'),
('ii','Sichuan Yi'),
('ik','Inupiaq'),
('io','Ido'),
('is','Icelandic'),
('it','Italian'),
('iu','Inuktitut'),
('ja','Japanese'),
('jbo','Lojban'),
('jv','Javanese'),
('ka','Georgian'),
('kg','Kongo'),
('ki','Kikuyu'),
('kj','Kwanyama'),
('kk','Kazakh'),
('kl','Kalaallisut'),
('km','Khmer'),
('kn','Kannada'),
('ko','Korean'),
('kr','Kanuri'),
('ks','Kashmiri'),
('ku','Kurdish'),
('kv','Komi'),
('kw','Cornish'),
('ky','Kirghiz'),
('la','Latin'),
('lb','Luxembourgish'),
('lg','Ganda'),
('li','Limburgish'),
('ln','Lingala'),
('lo','Lao'),
('lt','Lithuanian'),
('lu','Luba-Katanga'),
('lv','Latvian'),
('mg','Malagasy'),
('mh','Marshallese'),
('mi','Māori'),
('mk','Macedonian'),
('ml','Malayalam'),
('mn','Mongolian'),
('mr','Marathi'),
('ms','Malay'),
('mt','Maltese'),
('my','Burmese'),
('na','Nauru'),
('nb','Norwegian Bokmål'),
('nd','North Ndebele'),
('ne','Nepali'),
('ng','Ndonga'),
('nl','Dutch'),
('nn','Norwegian Nynorsk'),
('no','Norwegian'),
('nr','South Ndebele'),
('nv','Navajo'),
('ny','Chichewa'),
('oc','Occitan'),
('oj','Ojibwa'),
('om','Oromo'),
('or','Oriya'),
('os','Ossetian'),
('pa','Panjabi'),
('pi','Pāli'),
('pl','Polish'),
('ps','Pashto'),
('pt','Portuguese'),
('qu','Quechua'),
('rm','Raeto-Romance'),
('rn','Kirundi'),
('ro','Romanian'),
('ru','Russian'),
('rw','Kinyarwanda'),
('sa','Sanskrit'),
('sc','Sardinian'),
('sd','Sindhi'),
('se','Northern Sami'),
('sg','Sango'),
('sh','Serbo-Croatian'),
('si','Sinhala'),
('sk','Slovak'),
('sl','Slovenian'),
('sm','Samoan'),
('sn','Shona'),
('so','Somali'),
('sq','Albanian'),
('sr','Serbian'),
('ss','Swati'),
('st','Southern Sotho'),
('su','Sundanese'),
('sv','Swedish'),
('sw','Swahili'),
('ta','Tamil'),
('te','Telugu'),
('tg','Tajik'),
('th','Thai'),
('ti','Tigrinya'),
('tk','Turkmen'),
('tl','Tagalog'),
('tn','Tswana'),
('to','Tonga'),
('tr','Turkish'),
('ts','Tsonga'),
('tt','Tatar'),
('tw','Twi'),
('ty','Tahitian'),
('ug','Uighur'),
('uk','Ukrainian'),
('ur','Urdu'),
('uz','Uzbek'),
('ve','Venda'),
('vi','Vietnamese'),
('vo','Volapük'),
('wa','Walloon'),
('wo','Wolof'),
('xh','Xhosa'),
('yi','Yiddish'),
('yo','Yoruba'),
('za','Zhuang'),
('zh','Chinese'),
('zu','Zulu');
