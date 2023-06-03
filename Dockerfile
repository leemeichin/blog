FROM klakegg/hugo:ext-alpine-onbuild AS hugo

ARG HUGO_ENV_ARG=production

FROM busybox:1.35

RUN adduser -D static
USER static
WORKDIR /home/static

COPY --from=hugo /target .

CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]
