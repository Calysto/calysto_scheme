FROM nbgallery/jupyter-alpine

RUN mkdir /app
WORKDIR /app

RUN pip3 install --upgrade pip
RUN pip3 install --upgrade jupyter
RUN pip3 install --upgrade notebook

RUN pip3 install --upgrade calysto-scheme
RUN python3 -m calysto_scheme install

CMD jupyter notebook --allow-root --ip=0.0.0.0 --port=80
