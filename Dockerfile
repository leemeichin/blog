FROM silex/emacs:29.1-alpine AS emacs

WORKDIR /build

COPY . .

RUN ./bin/build

FROM busybox:1.35

RUN adduser -D static
USER static
WORKDIR /home/static

COPY --from=emacs /build .

CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]
