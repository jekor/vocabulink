% Copyright 2009 Chris Forno

% This file is part of Vocabulink.

% Vocabulink is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.

% Vocabulink is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.

% You should have received a copy of the GNU Affero General Public License
% along with Vocabulink. If not, see <http://www.gnu.org/licenses/>.

\documentclass[oneside]{article}
%include polycode.fmt
\usepackage[T1]{fontenc}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{hyperref}
\usepackage[pdftex]{graphicx}
\usepackage[x11names, rgb]{xcolor}
\usepackage{tikz}
\usetikzlibrary{decorations,arrows,shapes}
\usepackage[margin=1.4in]{geometry}

\hypersetup{colorlinks=true}

\title{Vocabulink Web Test}
\author{Chris Forno (jekor)}
\date{May 3rd, 2009}

\begin{document}
\maketitle

\section{Introduction}

Let's do some high-level functional tests of vocabulink.lan---the locally
running version of vocabulink.com---so that we can deploy to the live site with
more confidence.

To compile the tests, you'll need to install the following Haskell packages:

\begin{itemize}
\item selenium
\item base64-string
\end{itemize}

First, we need to be able to communicate with a Selenium RC server. I'm using a
modified version of Test.Selenium.

> import Test.Selenium.Server
> import Test.Selenium.Syntax
> import Test.Selenium.HUnit

> import Codec.Binary.Base64.String as B64
> import Codec.Binary.UTF8.String
> import Control.Monad
> import Data.Maybe
> import Network.URI
> import System.Random

All test sessions currently use Firefox as my test machine only has Free
Software installed. My test machine is also my development machine.

> session :: SeleniumRCSession
> session = mkSeleniumRCSession "localhost" Firefox $ fromJust $ parseURI "http://www.vocabulink.lan/"

> main :: IO ()
> main = do
>   username <- randomUsername
>   password <- randomPassword
>   withSelenium session $ do
>     runSeleniumTestsTT [  ("Front Page", frontPage),
>                           ("Latest Links", latestLinks),
>                           ("Sign Up", signUp username (testEmailAddress username) password) ]
>   return ()

First, we want to make sure that the front page of the site is functioning. We
could almost take this for granted, but we won't.

> frontPage :: Selenium ()
> frontPage = do
>   open "/"
>   assert (TextPresent "Welcome to Vocabulink")

See if the latest links page is displaying links. There should always be 10 for
non-authenticated clients.

> latestLinks :: Selenium ()
> latestLinks = do
>   open "/links"
>   assert' (XPathCount "//ul[@class='links']/li/a") "10"

Sign up for membership. Most other tests depend on this one completing successfully.

> signUp :: String -> String -> String -> Selenium ()
> signUp username email password = do
>   open "/"
>   clickAndWait (Link "Sign Up")
>   typeText (IdOrName "input0") username
>   typeText (IdOrName "input1") email
>   typeText (IdOrName "input2") password
>   typeText (IdOrName "input3") password
>   check (IdOrName "input4")
>   clickAndWait (XPath "//input[@value='Sign Up']")
>   assert (TextPresent "Welcome Back")

We can't guarantee that tests will run with a clean database each time. We
could query the database for an available test username, but I'd like to keep
the functional tests independent of the site's code. Instead, we'll use a
random username. This has a slight chance of collision but also the benefit of
exercising allowable username characters.

> randomUsername :: IO String
> randomUsername = do
>   usernameLength <- randomRIO (3, 32)
>   replicateM usernameLength randomIO

Vocabulink enforces email address uniqueness. We'll need a different email
address for each username, but we need the email to go somewhere. Luckily,
gmail allows us to add arbitrary suffixes after ``+'' at the end of the
address. However, email addresses have a more restricted character set than
Vocabulink usernames, so we'll need to encode them.

Codec.Binary.Base64.String doesn't seem to like unicode characters. We'll
convert the username to 8-bit characters before attempting to base-64 encode
it.

> testEmailAddress :: String -> String
> testEmailAddress username = "chris.forno+" ++ B64.encode (encodeString username) ++ "@gmail.com"

> randomPassword :: IO String
> randomPassword = do
>   passwordLength <- randomRIO (6, 72)
>   replicateM passwordLength randomIO

\end{document}