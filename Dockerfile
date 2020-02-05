FROM node:13

ENV PURESCRIPT_DOWNLOAD_SHA256 a78635b85a52f646c02b6a4f0c1d09d114ef75c008cc8523c80b5dd1ffc9fd4dc91e65642383ad8c7d0a2c4f3dd2a4ddc5c8c68eb564fd92adc709c9fcda42e1

RUN yarn global add bower pulp@13.0.0

RUN cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.13.6/linux64.tar.gz \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz

ENV PATH /opt/purescript:$PATH

RUN userdel node
RUN useradd -m -s /bin/bash pureuser

WORKDIR /home/pureuser

USER pureuser