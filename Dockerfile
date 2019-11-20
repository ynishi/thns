FROM haskell:8.6.5

COPY . /prj
WORKDIR /prj
RUN set -e \
  && stack install \
  && ln -s /root/.local/bin/thns-exe /thns

WORKDIR /
CMD ["/thns"]
