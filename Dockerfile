FROM haskell:8.6.5

COPY . /prj
WORKDIR /prj
RUN set -e \
  && stack install \
  && ln -s /root/.local/bin/thns-exe /thns \
  && ln -s /root/.local/bin/thns-cli /thns-cli \
  && ln -s /root/.local/bin/thns-axios /thns-axios

WORKDIR /
CMD ["/thns"]
