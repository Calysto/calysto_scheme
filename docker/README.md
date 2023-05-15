
To run calysto_scheme from docker:

Build the docker image:

```shell
cd calysto_scheme/docker
docker build . -t calysto-scheme
```

To run, using port 8000 to acces the docker jupyter
running on port 80:

```shell
docker run -it -p 8000:80 -v $(pwd):/app calysto-scheme
```

Then open your browser to:

```shell
http://127.0.0.1:8000/
```
