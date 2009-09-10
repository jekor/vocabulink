% Copyright 2008, 2009 Chris Forno

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

\section{Review Html}

This is separate from the main Review module only to break cyclical imports. It
may end up being more work than it's worth.

> module Vocabulink.Review.Html (reviewBox, reviewIndicator) where

> import Vocabulink.App
> import Vocabulink.DB
> import Vocabulink.Utils

> import Text.XHtml.Strict

We display the number of links that are waiting for review for logged in
members in the standard page header. Reviewing is currently the primary
function of Vocabulink, and we want it prominently displayed.

The idea is that a member will go to the site, and we want them to be instantly
reminded that they have links to review. Or, if a link for review becomes due
while they are browsing another part of the site, we want them to be notified.

> reviewBox :: App Html
> reviewBox = do
>   memberNo' <- asks appMemberNo
>   case memberNo' of
>     Nothing        -> return noHtml
>     Just memberNo  -> do
>       n <- numLinksToReview memberNo
>       return $ case n of
>         Just 0   -> anchor ! [  href "/review/next", theclass "review-box",
>                                 thestyle "color: black" ] <<
>                       "No links to review"
>         Just n'  -> anchor ! [href "/review/next", theclass "review-box"] <<
>                       [  strong << show n',
>                          stringToHtml (n' > 1 ? " links" $ " link"),
>                          stringToHtml " to review" ]
>         Nothing  -> stringToHtml "Error finding links for review."

This retrievs the number of links that a user has for review right now.

> numLinksToReview :: Integer -> App (Maybe Integer)
> numLinksToReview memberNo = do
>   v <- queryValue'  "SELECT COUNT(*) FROM link_to_review \
>                     \WHERE member_no = ? AND current_timestamp > target_time"
>                     [toSql memberNo]
>   return $ fmap fromSql v

When displaying a link, it's useful to show its review status (or a method to
add it to a review set if it's not). This returns an Html element that will do
both based on the link number and the currently logged in member.

> reviewIndicator :: Integer -> App (Html)
> reviewIndicator linkNo = do
>   memberNo <- asks appMemberNo
>   case memberNo of
>     Nothing -> return $ paragraph ! [theclass "review-box login"] <<
>                           anchor ! [href "/member/login"] << "Login to Review" 
>     Just n  -> do
>       r <- reviewing n linkNo
>       case r of
>         Nothing  -> return $ paragraph ! [theclass "review-box"] <<
>                       "Unable to determine review status."
>         Just r'  -> return $ r' ? paragraph ! [theclass "review-box reviewing"] <<
>                       "Reviewing" $
>                         form ! [  action ("/review/" ++ show linkNo ++ "/add"),
>                                   method "POST", theclass "review-box review"] <<
>                           [ submit "review" "Review" ]

|reviewing| determines whether or not a member is already reviewing a link.
This will be true only if the member is currently reviewing the link, not if
they've reviewed it in the past and later removed it from their review set.

> reviewing :: Integer -> Integer -> App (Maybe Bool)
> reviewing memberNo linkNo =
>   (/= []) <$$> queryTuples'  "SELECT link_no FROM link_to_review \
>                              \WHERE member_no = ? AND link_no = ?"
>                              [toSql memberNo, toSql linkNo]
