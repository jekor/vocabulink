-- vocabulink.sql

-- I would use regexp checks and domains at the database level if they worked
-- for unicode characters. Instead, I'll leave it up to the Haskell layer to do
-- verification.

CREATE TABLE log_type (
       name CHARACTER VARYING(32) PRIMARY KEY
);
INSERT INTO log_type (name) VALUES ('exception'), ('IO exception'),
                                   ('SQL error'), ('404'), ('parse error'),
                                   ('config');

CREATE TABLE log (
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       type CHARACTER VARYING(32) REFERENCES log_type (name) ON UPDATE CASCADE,
       message TEXT
);
CREATE INDEX log_time_index ON log (time);
COMMENT ON TABLE log IS 'We''d like to keep track of errors and other events we might be interested in. This is not meant to be a permanent log. To create a primary key for this table, we''d need a way of identifying the host and thread that the message came from. For now, we''ll ignore that';

CREATE TABLE member (
       member_no SERIAL PRIMARY KEY,
       username CHARACTER VARYING(32) NOT NULL UNIQUE,
       join_date TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       email TEXT,
       website TEXT,
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
       member_no INTEGER REFERENCES member (member_no) PRIMARY KEY,
       hash TEXT NOT NULL,
       email TEXT NOT NULL,
       email_sent TIMESTAMP (0) WITH TIME ZONE
);
COMMENT ON TABLE member_confirmation IS 'This allows us to keep track of in-progress member confirmations. A tuple exists for each unconfirmed member.';
COMMENT ON COLUMN member_confirmation.hash IS 'This is a random hash that we can email to the user (in the form of a link) to ensure that they''ve actually received the confirmation email. It should be random so that it''s not guessable';
COMMENT ON COLUMN member_confirmation.email_sent IS 'email_sent is the time a confirmation email was successfully sent (or at least when our MTA says it was sent).';

-- For our purposes, a lexeme is any text or symbol which can be linked. Each lexeme has a lemma, which is the canonical representation of different forms of the lexeme.
-- Lexemes include "日本語", "語", "五", "5", "five", "language", "にほんご" and "に".
-- Lexemes automatically exist. They are not represented by a relation.

CREATE TABLE language (
       abbr CHARACTER VARYING (3) PRIMARY KEY,
       name TEXT
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

CREATE TABLE link_type (
       name TEXT PRIMARY KEY,
       description TEXT NOT NULL,
       relation TEXT
);
COMMENT ON TABLE link_type IS 'There are different types of links between lexemes. From simple associations (just asserting that a link exists) to full-blown stories with pictures that use a native-language link word.';
COMMENT ON COLUMN link_type.relation IS 'For most link types, an individual link carries with it extra information. We use a separate table for each to store the extra information for each link. I considered using PostgreSQL''s inheritance features, but they seem to be problematic and I don''t know how well they perform. More than 1 link type can share the same table.';
INSERT INTO link_type (name, description, relation)
     VALUES ('association', 'A simple association with no attached meaning', NULL),
            ('cognate', 'A sound-alike or borrowed word', NULL),
            ('link word', 'A story derived from a native-language link word', 'link_type_link_word'),
            ('relationship', 'A relationship between 2 native words and a corresponding pair in a foreign language', 'link_type_relationship');

CREATE TABLE link (
       link_no SERIAL PRIMARY KEY,
       origin TEXT NOT NULL CHECK (length(origin) > 0),
       destination TEXT NOT NULL CHECK (length(destination) > 0),
       origin_language CHARACTER VARYING (3) NOT NULL REFERENCES language (abbr) ON UPDATE CASCADE,
       destination_language CHARACTER VARYING (3) NOT NULL REFERENCES language (abbr) ON UPDATE CASCADE,
       link_type TEXT NOT NULL REFERENCES link_type (name) ON UPDATE CASCADE,
       rating REAL,
       author INTEGER NOT NULL REFERENCES member (member_no) ON UPDATE CASCADE,
       created TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       updated TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       deleted BOOLEAN NOT NULL DEFAULT FALSE
);
COMMENT ON TABLE link IS 'A link is an association between 2 ideas in a single direction. (A reverse association would require another link.)';
COMMENT ON COLUMN link.origin IS 'lexeme (lemma)';
COMMENT ON COLUMN link.destination IS 'lexeme (lemma)';
COMMENT ON COLUMN link.deleted IS 'We need a way to delete links, but we don''t want to destroy people''s review decks. This allows us to mark deleted links so that we don''t display them to people who aren''t already reviewing them and we can later sweep ones with no references.';
-- We're going to search by these often.
CREATE INDEX link_origin_index ON link (origin);
CREATE INDEX link_destination_index ON link (destination);

CREATE TABLE link_type_link_word (
       link_no INTEGER NOT NULL REFERENCES link (link_no),
       link_word TEXT NOT NULL,
       story TEXT NOT NULL
);

CREATE TABLE link_type_relationship (
       link_no INTEGER NOT NULL REFERENCES link (link_no),
       left_side TEXT NOT NULL,
       right_side TEXT NOT NULL
);

CREATE TABLE link_to_review (
       member_no INTEGER REFERENCES member (member_no) ON UPDATE CASCADE,
       link_no INTEGER REFERENCES link (link_no) ON UPDATE CASCADE,
       target_time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       PRIMARY KEY (member_no, link_no)
);
COMMENT ON COLUMN link_to_review.member_no IS 'Anonymous members cannot schedule reviews. That would be chaos. It''s also confusing if you hadn''t realized that you weren''t logged in.';
COMMENT ON COLUMN link_to_review.target_time IS 'Target is the date and time at which this link should come up for review. The link will be reviewed sometime after that. All new links for review are currently scheduled for immediate review.';

CREATE TABLE link_review (
       member_no INTEGER REFERENCES member (member_no) ON UPDATE CASCADE,
       link_no INTEGER REFERENCES link (link_no) ON UPDATE CASCADE,
       target_time TIMESTAMP (0) WITH TIME ZONE NOT NULL,
       actual_time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       recall REAL NOT NULL,
       recall_time REAL NOT NULL,
       PRIMARY KEY (member_no, link_no, actual_time)
);
COMMENT ON COLUMN link_review.recall IS 'Recall is a measure of how easy or complete the memory of a link was. 1.0 is perfect recall. 0.0 means "no clue".';
COMMENT ON COLUMN link_review.recall_time IS 'Recall time is the amount of time (in milliseconds) taken to recall (or not) the destination of a link. It could be measured as the time between when the page is displayed and when the destination lexeme is shown (using JavaScript).';

CREATE TABLE link_sm2 (
       member_no INTEGER REFERENCES member (member_no) ON UPDATE CASCADE,
       link_no INTEGER REFERENCES link (link_no) ON UPDATE CASCADE,
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
       author INTEGER REFERENCES member (member_no) ON UPDATE CASCADE,
       publish_time TIMESTAMP (0) WITH TIME ZONE NOT NULL,
       update_time TIMESTAMP (0) WITH TIME ZONE,
       section TEXT,
       title TEXT
);
COMMENT ON COLUMN article.publish_time IS 'A blog post is published at this time. If it''s before this time, the post is only visible to the owner.';

CREATE RULE "replace article" AS
    ON INSERT TO "article"
    WHERE EXISTS (SELECT TRUE FROM article WHERE filename = NEW.filename)
    DO INSTEAD (UPDATE article SET update_time = NEW.update_time,
                                   title = NEW.title,
                                   section = NEW.section
                WHERE filename = NEW.filename);

-- Forums

CREATE TABLE forum_group (
       group_name TEXT PRIMARY KEY,
       position SMALLINT NOT NULL
);
COMMENT ON COLUMN forum_group.position IS 'The lower the position, the higher on the page the forum group is displayed.';

CREATE TABLE forum (
       name TEXT PRIMARY KEY,
       title TEXT NOT NULL,
       group_name TEXT REFERENCES forum_group (group_name) ON UPDATE CASCADE,
       position SMALLINT NOT NULL,
       icon_filename TEXT NOT NULL
);
COMMENT ON COLUMN forum.name IS 'The forum name must be URL-safe.';
COMMENT ON COLUMN forum.position IS 'This is like the forum group position. The forums are listed from left to right, top to bottom. The position is only for this forum group.';
COMMENT ON COLUMN forum.icon_filename IS 'The filename is the relative path to the icon from the configured icon directory.';

CREATE TABLE comment (
       comment_no SERIAL PRIMARY KEY,
       author INTEGER NOT NULL REFERENCES member (member_no) ON UPDATE CASCADE,
       time TIMESTAMP (0) WITH TIME ZONE NOT NULL DEFAULT current_timestamp,
       comment TEXT,
       parent_no INTEGER REFERENCES comment (comment_no)
);

CREATE TABLE forum_topic (
       topic_no SERIAL PRIMARY KEY,
       forum_name TEXT NOT NULL REFERENCES forum (name) ON UPDATE CASCADE,
       title TEXT NOT NULL,
       root_comment INTEGER REFERENCES comment (comment_no) NOT NULL,
       last_comment INTEGER REFERENCES comment (comment_no) NOT NULL,
       num_replies SMALLINT NOT NULL DEFAULT 0
);
COMMENT ON COLUMN forum_topic.last_comment IS 'While a pointer to the last comment isn''t theoretically necessary, it greatly simplifies retrieving information on forum topics in bulk.';
COMMENT ON COLUMN forum_topic.num_replies IS 'Again, this is not strictly necessary, but it does make queries easier.';
