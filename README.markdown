# Vocabulink - Learn Languages through Fiction

This is Vocabulink, the software that powers [http://www.vocabulink.com/](http://www.vocabulink.com/).

This source distribution consists mostly of a Haskell SCGI application (located in the hs directory), but also contains the JavaScript, SQL, and CSS used by the site as well. It contains almost everything you need to create a working copy of Vocabulink. It does not include any database records or any other content from the site such as native speaker audio recordings. Such content is copyrighted by its respective owners and has NOT been released to you under the GNU Affero Public license.

## How to build

The software is built with [Nix](http://nixos.org/nix/) to try to ensure reproducible builds. For examples of how it can be built/deployed, see [Jen](https://github.com/jekor/jen/blob/master/jen.nix).

## To create a test user

```
INSERT INTO member (username, password_hash, email)
VALUES ('username', crypt('password', gen_salt('bf')), 'email')
```
