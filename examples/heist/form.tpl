<html>
 <head>
  <title>Formettes + Heist</title>
 </head>
 <body>
  <h1>A form</h1>
  <form action="/" method="POST" enctype="multipart/form-data">
   <fmErrorList ref="username" />
   <label>Username: </label><fmInputString ref="username" />
   <fmErrorList ref="email" />
   <label>Email: </label><fmInputString ref="email" />
   <input type="submit" />
  </form>
 </body>
</html>
