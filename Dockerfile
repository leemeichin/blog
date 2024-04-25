FROM silex/emacs:29-ci@sha256:43e3a57e945733acbd4c38d80b09736b8b3c117f22f70f1c3b0d045276c764b8 AS emacs

WORKDIR /home/build

COPY . .

RUN ./bin/build

FROM busybox:1.35

RUN adduser -D static
USER static
WORKDIR /home/static

COPY --from=emacs /home/build/publish .

CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]
