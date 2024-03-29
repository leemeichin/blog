FROM silex/emacs:29-ci AS emacs

WORKDIR /home/build

COPY . .

RUN ./bin/build

FROM busybox:1.35

RUN adduser -D static
USER static
WORKDIR /home/static

COPY --from=emacs /home/build/publish .

CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]
