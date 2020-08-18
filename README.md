
To see a demonstration of the program, without having to install it, please [see the screenshots](https://github.com/elsanussi-s-mneina/phonetics-modeling-elm/wiki/screenshots).





To Intall Elm:
Go to https://guide.elm-lang.org/install/elm.html
To get started.




This project was created using the following command:

`elm init`


## How to run:

`elm reactor`

Open browser to localhost:8000 or to the URL indicated in the command line.

A webpage will appear.

Select "src" under source directories.

A file icon titled "Main.elm" should appear under "File Navigation" on that web page.

Click the icon.

This should open a web page with one button, and one label.
click the button.

It does not do much. It should just add a character to the label.
(More functionality is planned).

## How to run in a Docker container:
Ensure you have Docker installed.

It ran correctly with the following version of Docker: 

`docker --version`

Docker version 19.03.12, build 48a66213fe

If you do not have Docker installed, you can install it by going to
the following URL:
https://www.docker.com/products/docker-desktop

Run the following command in the terminal:

`docker-compose run -p 8000:8000 elm reactor`

Open your web browser to:
  `localhost:8000/src/Main.elm`

