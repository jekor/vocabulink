-- vocabulink.sql

-- I would use regexp checks and domains at the database level if they worked
-- for unicode characters. Instead, I'll leave it up to the Haskell layer to do
-- verification.

CREATE TABLE member (
       member_no SERIAL PRIMARY KEY,
       username character varying(18) NOT NULL UNIQUE,
       join_date timestamp with time zone NOT NULL DEFAULT current_timestamp,
       email text NOT NULL,
       password_hash text NOT NULL
);
COMMENT ON COLUMN member.member_no IS 'I debated about using the username as a primary key. In the end, I decided against it. Performance and space concerns won me over. It''s also nice to have a numeric reference to a member in case we need to refer to a member in some context where Unicode characters aren''t valid.';
COMMENT ON COLUMN member.username IS 'I want to allow usernames to include any URL-safe characters, including alphanumeric Unicode characters. The regexp for username is ^[[:alnum:]$-_.+!*''(),]{3,}$. Because usernames will be used in URLs, and displayed in various places, I want to keep them to a reasonable size.';
COMMENT ON COLUMN member.email IS 'In order to allow any reasonable email address from members, the regexp for email addresses is ^[[:print:]]+@[[:print:]]+$. Again, this should accept Unicode characters';
COMMENT ON COLUMN member.password_hash IS 'The member''s password is stored as a hash, calculated by the pgcrypto contrib functions. See /usr/share/postgresql/contrib/pgcrypto.sql. The actual function used is crypt(?, gen_salt(''bf'')) (bf is for blowfish)';

CREATE TABLE card (
       card_no SERIAL PRIMARY KEY,
       member_no integer REFERENCES member (member_no) ON UPDATE CASCADE,
       question text,
       answer text,
       is_public boolean DEFAULT true,
       UNIQUE (member_no, question)
);
COMMENT ON TABLE card IS 'Question and answer are arbitrary labels for a card. When dealing with vocabulary they could very well be "native" and "foreign". I wanted to avoid "front" and "back", because I don''t want to be hindered by physical analogies.';
COMMENT ON COLUMN card.is_public IS 'For now, I''m using a very simple permissions system. A card is viewable by anyone if is_public is true.';

CREATE TABLE card_set (
       set_no SERIAL PRIMARY KEY,
       member_no integer REFERENCES member (member_no) ON UPDATE CASCADE,
       set_name text,
       is_public boolean,
       UNIQUE (member_no, set_name)
);
COMMENT ON TABLE card_set IS 'A card set is a collection of cards. A card may belong in more than one set. A set is named and has similar permission controls as an individual card.';

CREATE TABLE card_set_member (
       set_no integer REFERENCES card_set (set_no),
       card_no integer REFERENCES card (card_no),
       PRIMARY KEY (set_no, card_no)
);
