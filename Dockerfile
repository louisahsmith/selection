FROM virtualstaticvoid/heroku-docker-r:shiny
ENV PORT=8080
CMD "/usr/bin/R --no-save -f /app/run.R"
