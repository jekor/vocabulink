-- vocabulink.sql

-- I would use regexp checks and domains at the database level if they worked
-- for unicode characters. Instead, I'll leave it up to the Haskell layer to do
-- verification.

CREATE TABLE member (
       member_no SERIAL PRIMARY KEY,
       username CHARACTER VARYING(32) NOT NULL UNIQUE,
       join_date TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
       email TEXT,
       password_hash TEXT NOT NULL
);
COMMENT ON COLUMN member.member_no IS 'I debated about using the username as a primary key. In the end, I decided against it. Performance and space concerns won me over. It''s also nice to have a numeric reference to a member in case we need to refer to a member in some context where Unicode characters aren''t valid.';
COMMENT ON COLUMN member.username IS 'I want to allow usernames to include any URL-safe characters, including alphanumeric Unicode characters. The regexp for username is ^[\p{L}\p{Nd}$-_.+!*''(),]{3,}$. Because usernames will be used in URLs, and displayed in various places, I want to keep them to a reasonable size. This was also a motivation for making the difficult decision to disallow spaces.';
COMMENT ON COLUMN member.email IS 'In order to allow any reasonable email address from members, the regexp for email addresses is ^[\p{L}\p{N}\p{P}\p{S}]+@[\p{L}\p{N}\p{P}\p{S}]+$. Again, this should accept Unicode characters';
COMMENT ON COLUMN member.password_hash IS 'The member''s password is stored as a hash, calculated by the pgcrypto contrib functions. See /usr/share/postgresql/contrib/pgcrypto.sql. The actual function used is crypt(?, gen_salt(''bf'')) (bf is for blowfish)';

CREATE TABLE idea (
       idea_no SERIAL PRIMARY KEY,
       representation TEXT NOT NULL,
       created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
COMMENT ON TABLE idea IS 'An idea is a concept, word, image---anything concrete and brief. Examples of ideas are "日本語", "日", "sun", "language", and "にほんご".';

CREATE TABLE link_type (
       type_name TEXT PRIMARY KEY,
       description TEXT NOT NULL,
       color INTEGER NOT NULL
);
INSERT INTO link_type (type_name, description, color)
     VALUES ('association', 'A simple association', 0);
INSERT INTO link_type (type_name, description, color)
     VALUES ('story', 'A vivid story', 16711680);

CREATE TABLE link (
       link_no SERIAL PRIMARY KEY,
       origin_idea INTEGER REFERENCES idea (idea_no) ON UPDATE CASCADE,
       destination_idea INTEGER REFERENCES idea (idea_no) ON UPDATE CASCADE,
       -- type? story, picture, etc.
       type_name TEXT REFERENCES link_type (type_name) ON UPDATE CASCADE,
       representation TEXT,
       rating REAL,
       author INTEGER REFERENCES member (member_no) ON UPDATE CASCADE,
       created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
       updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
COMMENT ON TABLE link IS 'A link is an association between 2 ideas in a single direction. (A reverse association would require another link.)';

CREATE TABLE link_set (
       set_no SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       author INTEGER REFERENCES member (member_no) ON UPDATE CASCADE
);

CREATE TABLE link_set_member (
       set_no INTEGER REFERENCES link_set (set_no) ON UPDATE CASCADE,
       link_no INTEGER REFERENCES link (link_no) ON UPDATE CASCADE,
       PRIMARY KEY (set_no, link_no)
);