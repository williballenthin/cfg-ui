FROM ubuntu:xenial

# basic stuff
RUN apt-get update && apt-get install -y \
    build-essential python-virtualenv \
    python3-dev python-pip libffi-dev libssl-dev \
    wget tmux git vim ack-grep tmuxinator rlwrap
RUN mkdir /env && virtualenv -p python3 /env
RUN git clone https://github.com/VundleVim/Vundle.vim.git /root/.vim/bundle/Vundle.vim && \
    wget https://raw.githubusercontent.com/williballenthin/dotfiles/master/vim/.vimrc -O /root/.vimrc && \
    vim +PluginInstall +qall && \
    wget https://raw.githubusercontent.com/williballenthin/dotfiles/master/tmux/.tmux.conf -O /root/.tmux.conf && \
    pip install flake8 ipython

# webapp build tools
RUN apt-get install -y openjdk-8-jre nginx
RUN mkdir /tools && \
    wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -O /tools/lein && \
    chmod +x /tools/lein && \
    wget https://github.com/clojure/clojurescript/releases/download/r1.9.293/cljs.jar -O /tools/cljs.jar

# binary analysis tools
RUN git clone https://github.com/radare/radare2.git /tools/radare2
RUN cd /tools/radare2 && \
    sys/install.sh && \
    make symstall && \
    r2pm init && r2pm update

# 80/http is the web application
EXPOSE 80
# 3449/ws is the figwheel websocket
EXPOSE 3449

VOLUME /data

RUN mkdir /cfg /code
ADD nginx.conf /cfg/nginx.conf
RUN git clone https://github.com/williballenthin/cfg-ui.git /code/cfg-ui
# cache a build
RUN cd /code/cfg-ui && \
    LEIN_ROOT=yes /tools/lein cljsbuild once

RUN sleep 1.0001 && cd /code/cfg-ui && git pull origin master
RUN cd /code/cfg-ui && \
    LEIN_ROOT=yes /tools/lein cljsbuild once

ENV LEIN_ROOT 1
# TODO: add rlwrap
CMD nginx -c /cfg/nginx.conf && (r2 -q -c "http.bind=0.0.0.0; =h 90" /bin/ls &) && cd /code/cfg-ui/ && /tools/lein run -m clojure.main /code/cfg-ui/script/figwheel.clj
