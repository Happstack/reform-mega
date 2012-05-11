Formettes Tutorial
==================

Formettes is a type-safe form generation and validation library. Its
primary use is generating HTML forms and processing the corresponding
form submissions.

The traditional method of creating and processing forms is:

 1. create a &lt;form&gt; tag with the desired elements by hand

 2. write code which processed the form data set and tries to extract a value from it

The developer encounters a number of difficulties using this method.

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

Formettes is an extension of the work originally done by Philip Wadler
et al., which was later ported to Haskell as the formlets library, and
then revamped again as the digestive-functors library.

The digestive-functors 0.3 represents a major break from the
traditional formlets tradition. The motivation behind
digestive-functors 0.3 was to allow the separation of validators from
the view code. This allows library authors to define forms, but allow
the library users to create the view for the forms. It also provides a
mechanism to support templating systems like Heist, where the view is
defined in an external XML file rather than Haskell code.

In order to achieve this, digestive-functors unlinks the validation
and view code and requires the developers to stitch them back together
using String based names. This leads to runtime errors.

The formettes library is forked for digestive-functors 0.2 and retains
the traditional formlets feel. It allows the separation of the view
and validation code, but does so in a way that retains type safety.

Hello Form!
-----------

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances #-}
> {-# OPTIONS_GHC -F -pgmFtrhsx #-}
> module Main where

First we need to import the Formettes library. We need to import three different components, the core library, a server backend, and a template library. In this case we will be using Happstack for the server and HSP for the templating.

> import Control.Applicative
> import Happstack.Server
> import HSP.ServerPartT
> import HSP
> import Text.Formettes
> import Text.Formettes.Happstack
> import Text.Formettes.HSP.String


First we will create a type alias for our applications server monad:

> type AppT m = XMLGenT (ServerPartT m)

Forms have the type 'Form' which looks like:

    newtype Form m input error view proof = Form { ... }

As you will note it is heavily parameterized:

<dl>
 <dt>m</dt><dd>a monad which can be used to validate the result</dd>
 <dt>input</dt><dd>the framework specific type containing the fields from the form data set.</dd>
 <dt>error</dt><dd>An application specific type for form validation errors.</dd>
 <dt>view</dt><dd>The type of the view for the form.</dd>
 <dt>proof</dt><dd>A datatype which names something that has been proved about the result</dd>
 <dt>a</dt><dd>The value returned when the form data set is successfully decoded.</dd>
</dl>

In order to keep our type signatures sane, it is convenient to create a type alias to simplify things:

> type SimpleForm = Form IO [Input] AppError [AppT IO (XMLType (ServerPartT IO))] ()

`AppError` is used to report form validation errors. Here we have a single, unified error type for all form errors in our application:

> data AppError
>     = Required
>     | Common (CommonFormError [Input])
>       deriving Show

> instance (Monad m) => EmbedAsChild (ServerPartT m) AppError where
>     asChild error = <% show error %>

We could instead have a per-form error type -- or even just use String. The advantage of using a type is that it make it easier to providing I18N translations, or for users of a library to customize the error message text.

The error type needs an `FormError` instance:

> instance FormError AppError where
>     type ErrorInputType AppError = [Input]
>     commonFormError = Common

Internally, Formettes has an error type `CommonFormError` which is used to report things like missing fields and other errors. The `FormError` class is used to lift those errors into your custom error type.

Now we have the groundwork laid to create a simple form. Let's create
a form that allows you to post a message. First we will want a type to
represent the message -- a simple record will do:

> data Message = Message
>     { name    :: String
>     , title   :: String
>     , message :: String
>     } deriving (Eq, Ord, Read, Show)

Now we can create a very basic form like so:

> postForm :: SimpleForm Message
> postForm =
>     Message
>      <$> label "name:"            ++> inputText ""       <* br
>      <*> label "title: "          ++> inputText ""       <* br
>      <*> (label "message:" <* br) ++> textarea 80 40 ""  <* br

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

This form is very simple. It accepts any input, not caring if the fields are empty or not. It also does not try to convert the String values to another type. However, we do still benefit from Formettes. We create a single form, and from that we automatically extract the code to generate HTML and the code to extract the values from the form data set.

We did not have to explicit name our fields, keep the names in-sync in two different places, worry if the html and processing code contain the same set of fields, or worry if a name/id has already been used.

Form with simple validation
---------------------------

We can easily modify our form so that all the requires are required. To do that we will create a simple validation function:

> required :: String -> Either AppError String
> required []  = Left Required
> required str = Right str

In this case we are simply checking that the `String` is not null, so we return the `String` unmodified. Other times, we might actually transform the value. For example, we might attempt to convert the `String` to an `Integer`. To apply this validation function we can use `transformEither`:

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

Let's create a new form type alias that allows us to actually set the 'proof' parameter.

> type ProofForm proof = Form IO [Input] AppError [AppT IO (XMLType (ServerPartT IO))] proof

Using the 'proof' parameter is used to indicate that something has been proven about a forms return value. 

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

> data NotNullProof = NotNullProof

and then creating a proof like so:

> notNullProof :: error -> Proof m error NotNullProof [a] [a]
> notNullProof errorMsg =
>     Proof { proofName    = NotNullProof
>           , proofFunction = assertNotNull
>           }
>     where
>       assertNotNull :: [a] -> m (Either error [a])
>       assertNotNull = return errorMsg


> -- mkMessage :: ProofForm 