Formettes Tutorial
==================

Formettes is a library for create type-safe, composable, and validated
HTML forms. It is built around applicative functors and is based on
the same principles as formlets and digestive-functors < 0.2.

The core formettes library is designed to be portable and can be used
with a wide variety of Haskell web frameworks and template solutions
-- though only a few options are supported at the moment.

The most basic method of creating and processing forms with out the
assistence of formettes is to:

 1. create a &lt;form&gt; tag with the desired elements by hand

 2. write code which processed the form data set and tries to extract a value from it

The developer encounters a number of difficulties using this method:

 1. the developer must be careful to use the same 'name' field in the
 HTML and the code.

 2. if a new field is added to the form, the code must be manually
 updated. Failure to do so while result in the new value being
 silently ignored.

 3. form fragments can not be easily combined because the 'name'
 fields might collide.

 4. if the form fails to validate, it is difficult to redisplay the
 form with the error messages and data that was submitted.

Formettes solves these problems by combining the view generation code
and validation code into a single 'Form' element. The 'Form' elements
can be safely combined to create more complex forms.

In theory, Formettes could be applied to other domains, such as
command-line or GUI applications. However, Formettes is based around
the pattern of:

 1. generate the entire form at once
 2. wait until the user has filled out all the fields and submitted it
 3. process the results and generate an answer or redisplay the form with validation errors

For interactive applications, there is no reason to wait until the
entire form has been filled out to perform validation.

Brief History
-------------

Formettes is an extension of the OCaml-based formlets concept
originally developed by Philip Wadler et al., which was later ported
to Haskell as the `formlets` library, and then revamped again as the
`digestive-functors` library.

The `digestive-functors` 0.3 represents a major break from the
traditional `formlets` model. The motivation behind
`digestive-functors` 0.3 was to allow the separation of validators
from the view code. This allows library authors to define validation
for forms, but allow the library users to create the view for the
forms. It also provides a mechanism to support templating systems like
Heist, where the view is defined in an external XML file rather than
Haskell code.

In order to achieve this, `digestive-functors` unlinks the validation
and view code and requires the developers to stitch them back together
using `String` based names. This leads to runtime errors.

The `Formettes` library is a heavily modified fork of
`digestive-functors` 0.2. It builds on the the traditional `formlets`
safety and style and extends it to allow view and validation
separation in a type-safe manner.

Hello Form!
-----------

The `easiest` way to learn `Formettes` is through example. We will
start with a simple form that does not require any special
validation. We will then extend to the form, adding some
validators. And then we will show how to split the validation and view
into separate pieces of code.

This example uses Happstack for the web server and HSP for the templating library.

First we have some pragmas:

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
> {-# OPTIONS_GHC -F -pgmFtrhsx #-}
> module Main where

And then some imports. We import modules from three different `Formettes` packages: the core formettes library, the formettes-happstack package, and the formettes-hsp package:

> import Control.Applicative
> import Happstack.Server
> import HSP.ServerPartT
> import HSP
> import Text.Formettes (IndexedFunctor(..), IndexedApplicative(..), CommonFormError(..), Form, FormError(..), Proof(..), (++>), (<+*+>), prove, transformEither, transform, decimal)
> import Text.Formettes.Happstack
> import Text.Formettes.HSP.String

Next we will create a type alias for our application's server monad:

> type AppT m = XMLGenT (ServerPartT m)

Forms have the type 'Form' which looks like:

] newtype Form m input error view proof a = Form { ... }

As you will note it is heavily parameterized:

<dl>
 <dt>m</dt><dd>a monad which can be used to validate the result</dd>
 <dt>input</dt><dd>the framework specific type containing the fields from the form data set.</dd>
 <dt>error</dt><dd>An application specific type for form validation errors.</dd>
 <dt>view</dt><dd>The type of the view for the form.</dd>
 <dt>proof</dt><dd>A datatype which names something that has been proved about the result</dd>
 <dt>a</dt><dd>The value returned when the form data set is successfully decoded.</dd>
</dl>

In order to keep our type signatures sane, it is convenient to create an application specific type alias to simplify things:

> type SimpleForm = Form IO [Input] AppError [AppT IO (XMLType (ServerPartT IO))] ()

`AppError` is used to report form validation errors. Here we define a single, unified error type for all form errors in our application:

> data AppError
>     = Required
>     | NotANatural String
>     | Common (CommonFormError [Input])
>       deriving Show

> instance (Monad m) => EmbedAsChild (ServerPartT m) AppError where
>     asChild error = <% show error %>

We could instead have a per-form error type -- or even just use `String`. The advantage of using a type is that it make it easier to providing I18N translations, or for users of a library to customize the error message text.

Our error type needs an `FormError` instance:

> instance FormError AppError where
>     type ErrorInputType AppError = [Input]
>     commonFormError = Common

Internally, `Formettes` has an error type `CommonFormError` which is used to report things like missing fields and other errors. The `FormError` class is used to lift those errors into our custom error type.

Now we have the groundwork laid to create a simple form. Let's create
a form that allows users to post a message. First we will want a type to
represent the message -- a simple record will do:

> data Message = Message
>     { name    :: String -- ^ the author's name
>     , title   :: String -- ^ the message title
>     , message :: String -- ^ contents of the message
>     } deriving (Eq, Ord, Read, Show)

Now we can create a very basic form:

> postForm :: SimpleForm Message
> postForm =
>     Message
>      <$> label "name:"            ++> inputText ""       <* br
>      <*> label "title: "          ++> inputText ""       <* br
>      <*> (label "message:" <* br) ++> textarea 80 40 ""  <* br

This form contains all the information need to generate the form elements and to parse the submitted form data set and extract a `Message` value.

The `label` function creates a <code>&lt;label&gt;</code> element using the supplied label.

The `inputText` function creates a <code>&lt;input type="text"&gt;</code> input element using the argument as the initial value.

The `textarea` function creates <code>&lt;textearea&gt;</code>. The arguments are the number of cols, rows, and initial contents.

The `br` functions creates a `Form` element that doesn't do anything except insert a <code>&lt;br&gt;</code> tag.

The `<$>`, `<*>` and `<*` operators come from `Control.Applicative`. If you are not familiar with applicative functors then you will want to read a [tutorial such as this one](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors).

`++>` comes from the Formettes library and has the type:

] (++>) :: (Monad m, Monoid view) =>
]          Form m input error view () ()
]       -> Form m input error view proof a
]       -> Form m input error view proof a

The `++>` operator is similar to the `*>` operator with one important difference. If we were to write:

] label "name: " *> inputText

then the `label` and `inputText` which each have unique `FormId` values. But when we write:

] label "name: " ++> inputText

they have the same `FormId` value. The `FormId` value is typically used to create unique `name` and `id` attributes for the form elements. But, in the case of `label`, we want the `for` attribute to refer to the `id` of the element it is labeling.

There is also a similar operator <++ for when you want the label after the element.

This form is very simple. It accepts any input, not caring if the fields are empty or not. It also does not try to convert the `String` values to another type before adding them to the record.

However, we do still see benefits from `Formettes`. We specified the form once, and from that we automatically extract the code to generate HTML and the code to extract the values from the form data set. This adhears to the DRY (don't repeat yourself) principle. We did not have to explicitly name our fields, keep the names in-sync in two different places, worry if the HTML and processing code contain the same set of fields, or worry if a name/id has already been used.

Form with simple validation
---------------------------

The next step is to perform some validation on the input fields. If the fields validate successfully, then we get a `Message`. But if the input fails to validate, then we want to automatically regenerate the `Form` showing the data the user submitted plus validation errors.

For this example, let's simply make sure they entered something in all the fields. To do that we will create a simple validation function:

> required :: String -> Either AppError String
> required []  = Left Required
> required str = Right str

In this case we are simply checking that the `String` is not null. If it isn't we return an error, otherwise we return the `String` unmodified. Some validators will actually transform the value -- such as converting the `String` to an `Integer`.

To apply this validation function we can use `transformEither`:

] transformEither :: Monad m =>
]                    Form m input error view anyProof a
]                 -> (a -> Either error b)
]                 -> Form m input error view () b

We can update our form to:

> validPostForm :: SimpleForm Message
> validPostForm =
>     Message <$> name <*> title <*> msg
>         where
>           name  = errorList ++> label "name:"            ++> (inputText "" `transformEither` required) <* br
>           title = errorList ++> label "title:"           ++> (inputText "" `transformEither` required) <* br
>           msg   = errorList ++> (label "message:" <* br) ++> textarea 80 40 ""                         <* br

The `errorList` will add a list of error messages to a 'Form'
element. This gives greater control over where error messages appear
in the form. The list of errors is literally a list of errors inside
an:

    &lt;ul class="formettes-error-list"&gt;

You can use .css to control the theming.

For even greater control we could use the `Text.Formettes.Generalized.errors` function:

] errors :: Monad m =>
]           ([error] -> view) -- ^ function to convert the error messages into a view
]        -> Form m input error view () ()

This allows you to provide your own custom code for rendering the errors.

Separating Validation and Views
===============================

One of the primary motivations behind the changes in digestive-functors 0.3 is allowing developers to separate the validation code from the code which generates the view. We can do this using Formettes at well -- in a manner that is both more flexible and which provides greater type safety. The key is the 'proof' parameter which we have so far set to `()` and otherwise ignored.

In Formettes we divide the work into two pieces:

 1. Proofs
 2. a Form that returns a Proved value

This allows the library authors to create Proofs and demand that the `Form` a developer provides satisfies a `Proof`. At the same time, it gives the developer unrestricted control over the layout of the `Form` -- including choice of templating library.

Let's create a new type alias for `Form` that allows us to actually set the 'proof' parameter.

> type ProofForm proof = Form IO [Input] AppError [AppT IO (XMLType (ServerPartT IO))] proof

First we will explore the `Proof` related code that would go into a library.

The `proof` parameter is used to indicate that something has been proven about a forms return value.

Two create a proof we need two things:

 1. a type which names the proof
 2. a function which performs the proof

We wrap those two pieces up into a `Proof`:

] data Proof m error proof a b
]     = Proof { proofName     :: proof                   -- ^ name of the thing to prove
]             , proofFunction :: a -> m (Either error b) -- ^ function which provides the proof
]            }

In `validPostForm`, we checked they input fields were not empty
strings. We could turn that into a proof by creating a type to name
that proof:

> data NotNull = NotNull

and then creating a proof like so:

> notNullProof :: forall m error a. (Monad m) =>
>                 error -- ^ error to return if list is empty
>              -> Proof m error NotNull [a] [a]
> notNullProof errorMsg =
>     Proof { proofName     = NotNull
>           , proofFunction = assertNotNull
>           }
>     where
>       assertNotNull :: [a] -> m (Either error [a])
>       assertNotNull [] = return (Left errorMsg)
>       assertNotNull xs  = return (Right xs)

We can also create proofs that combine existing proofs. For example, a `Message` is only valid if all its fields are not null. So, first thing we want to do is create a proof name valid messages:

> data ValidMessage = ValidMessage

The normal `Message` constructor has the type:

] Message :: String -> String -> String -> Message

What we want is a smart constructor that also enforces the proofs:

> mkMessage :: ProofForm (NotNull -> NotNull -> NotNull -> ValidMessage) (String -> String -> String -> Message)
> mkMessage = ipure (\NotNull NotNull NotNull -> ValidMessage) Message


The library author can then specify that the user supplied form has the type:

] Form ValidMessage Message

To construct a form, we use a pattern very similar to what we did when using 'SimpleForm'.

To apply a `Proof` we use the `prove` function:

] prove :: (Monad m) =>
]          Form m input error view q a
]       -> Proof m error proof a b
]       -> Form m input error view proof b

So, we can make a `ProofForm` for non-empty `Strings` like this:

> inputText' :: String -> ProofForm NotNull String
> inputText' initialValue = inputText initialValue `prove` (notNullProof Required)

> textarea' :: Int -> Int -> String -> ProofForm NotNull String
> textarea' cols rows initialValue = textarea cols rows initialValue `prove` (notNullProof Required)

to create the ValidMessage form we can then combine the pieces like:

> provenPostForm :: ProofForm ValidMessage Message
> provenPostForm =
>     mkMessage <+*+> errorList ++> label "name: "    ++> inputText' ""
>               <+*+> errorList ++> label "title: "   ++> inputText' ""
>               <+*+> errorList ++> label "message: " ++> textarea' 80 40 ""

This code looks quite similar to our `validPostForm` code. The primary
difference is that we use `<+*+>` instead of `<*>`. That brings is to the topic of indexed applicative functors.

Type Indexed / Parameterized Applicative Functors
-------------------------------------------------

Lets look at the type for `Form` again:

    newtype Form m input error view proof a = Form { ... }

In order to make an `Applicative` instance of `Form`, all the proof type variables have to be the same and form a Monoid:

] instance (Functor m, Monad m, Monoid view, Monoid proof) => (Form m input error view proof) where ...

for `SimpleForm` we used this instance:

] instance (Functor m, Monoid view, Monad m) => Applicative (Form m input error view ()) where

Which looks and feels almost exactly like digestive-functors <= 0.2.

But, for the provePostForm, that instance won't work for use. mkMessage has the type:

] mkMessage :: ProofForm (NotNull -> NotNull -> NotNull -> ValidMessage) (String -> String -> String -> Message)

and we want to apply it to `ProofForms` created by:

] inputText' :: String -> ProofForm NotNull String

Here the proof types don't match up. Instead we need a Applicative Functor that allows use to transform the return value *and* the proof value. We need what, I believe, is called an type-indexed applicative functor or a parameterized applicative functor. Most literature on this subject is actually dealing with type-indexed or parameterized monads, but the idea is the same, or the class names are different.

We define two new classes, `IndexedFunctor` and `IndexedApplicative`:

] class IndexedFunctor f where
]     -- | imap is similar to fmap
]     imap :: (x -> y) -- ^ function to apply to first parameter
]          -> (a -> b) -- ^ function to apply to second parameter
]          -> f x a    -- ^ indexed functor
]          -> f y b

] class (IndexedFunctor f) => IndexedApplicative f where
]     -- | similar to 'pure'
]     ipure   :: x -> a -> f x a
]     -- | similar to '<*>'
]    (<+*+>) :: f (x -> y) (a -> b) -> f x a -> f y b

These classes look just like their non-indexed counterparts, except that they transform an extra parameter. Now we can create instances like:

] instance (Monad m)              => IndexedFunctor     (Form m input view error) where
] instance (Monad m, Monoid view) => IndexedApplicative (Form m input error view) where

We use this classes the same way we would use the normal `Functor` and `Applicative` classes. The only difference is that the type-checker can now enforce the proofs.

Using proofs in unproven forms
------------------------------

The `Proof` module provides a handful of useful `Proofs` that perform
transformations, such as converting a String to a Int. We can use this
`Proofs` with our `SimpleForm` by using the `transform` function:

] transform :: (Monad m) =>
]              Form m input error view anyProof a
]           -> Proof m error proof a b
]           -> Form m input error view () b

`transform` is similar to the `prove` function, except it ignores the
proof name and sets the proof to `()`. Technically `()` is still a
proof -- but we consider it to be the proof that proves nothing. We can then use that to create a simple form that parses a positive Integer value.

> inputInt :: SimpleForm Integer
> inputInt = inputText "" `transform` (decimal NotANatural)
