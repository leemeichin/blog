FROM silex/emacs:27.1-alpine AS emacs

RUN apk add --update git

WORKDIR /build

COPY . .

RUN ./bin/build

FROM busybox:1.35

RUN adduser -D static
USER static
WORKDIR /home/static

COPY --from=emacs /build .

CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]
