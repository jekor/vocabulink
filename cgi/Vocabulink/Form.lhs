% Copyright 2010, 2011 Chris Forno

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

\section{Form}

> module Vocabulink.Form (Html5Form, AppForm, runForm, runForm', plug,
>                         check, checkM, ensure, ensures, ensureM, nothingIfNull,
>                         textarea, label, button, fieldset, legend,
>                         name, enctype, readonly, disabled,
>                         formLabel, formLabel',
>                         tabularInput, tabularSubmit,
>                         nonEmptyAndLessThan) where

> import Vocabulink.App
> import Vocabulink.CGI
> import Vocabulink.Html
> import Vocabulink.Utils

> import Control.Applicative.Error (Failing(..))
> import Text.Blaze.Html5 (textarea, label, fieldset, legend)
> import Text.Blaze.Html5.Attributes (enctype, readonly, disabled)
> import Text.Blaze.Html5.Formlets (Html5Form)
> import qualified Text.Blaze.Html5.Formlets as HF (input)
> import Text.Formlets (runFormState, plug, check, checkM, ensure, ensures, ensureM, nothingIfNull)

> import Prelude hiding (div, span, id, writeFile)

\subsection{Formlet Helpers}

Formlets are a great tool for abstracting and building complex forms. But the
library is still a bit rough around the edges. These helpers are by no means
elegant, but they help get the job done.

All of the formlets we build have this type:

> type AppForm a = Html5Form (AppT IO) a

We use |runForm| for most of the heavy lifting. It takes a form and a submit
button label, runs the form, and returns either the form to display (with
errors, if any) or the result of the form.

``Running'' the form involves taking the form inputs from the ``environment''
(the CGI input variables) and ``passing'' them to the form. The form then
attempts to validate against the environment. If it fails, it returns a form
(as Html) to display to the client, but if it succeeds it returns a value of
the type of the form.

|s| is either a label or some custom Html for the submit button (or noHtml if
you don't want a submit button).

> runForm :: Html5Form (AppT IO) a -> Either String Html -> App (Either Html a)
> runForm frm s = do
>   (status, html) <- runForm' frm
>   case status of
>     Failure failures -> do
>       uri   <- requestURI
>       meth  <- requestMethod
>       let submit' = case s of
>                       Left s'  -> input ! type_ "submit" ! value (stringValue s')
>                       Right h  -> h
>       return $ Left $ form ! action (stringValue $ uriPath uri) ! method "post" $ do
>                         when (meth /= "GET") (unordList $ map string failures)
>                         html
>                         submit'
>     Success result   -> return $ Right result

This is a slimmer wrapper around runFormState for when you want to get access
to the errors before they're packed into the returned Html. This is also handy
when implementing ``preview'' functionality for forms.

> runForm' :: Html5Form (AppT IO) a -> App (Failing a, Html)
> runForm' frm = do
>   names <- getInputNames
>   env <- zip names . map fromJust <$> mapM getTextOrFileInput names
>   let (res, markup, _) = runFormState env frm
>   status <- res
>   return (status, markup)

We ofter want to ``wrap'' a label around a form component. Note that this
doesn't currently set a @for@ attribute either.

> formLabel :: Monad m => String -> Html5Form m a -> Html5Form m a
> formLabel text = plug (\html -> do label $ string (text ++ ": "); html)

Here's an alterate version of the above which also adds a paragraph.

> formLabel' :: Monad m => String -> Html5Form m a -> Html5Form m a
> formLabel' text = plug (\html -> p $ do
>                                    label $ string (text ++ ": ")
>                                    html)

> nonEmptyAndLessThan :: Int -> String -> [(String -> Bool, String)]
> nonEmptyAndLessThan i t =
>   [  ((> 0)  . length, t ++ " must not be empty."),
>      ((< i)  . length, t ++ " must be " ++ show i ++ "characters or shorter") ]

\subsection{Form Builders}

For complex forms, we use tables. Tables allow for proper alignment that makes
the form much easier to read. This type of form tends to have a number of
common elements that we can abstract out.

One thing that we're don't currently do is hook the label to the control using
the ``for'' attribute.

> tabularInput :: String -> Html -> Html
> tabularInput l i = tr $ do
>   th $ label $ string (l ++ ":")
>   td i

We want any submit button centered on a row of its own.

> tabularSubmit :: String -> Html
> tabularSubmit l = tr $ td ! colspan "2" $ input ! type_ "submit" ! class_ "light" ! value (stringValue l)
