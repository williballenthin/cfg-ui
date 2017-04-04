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

# 80/http is the web application
EXPOSE 80
# 3449/ws is the figwheel websocket
EXPOSE 3449

VOLUME /data

RUN mkdir /cfg /code
ADD nginx.conf /cfg/nginx.conf
RUN git clone https://github.com/williballenthin/cfg-ui.git /code/cfg-ui
RUN cd /code/cfg-ui && \
    LEIN_ROOT=yes /tools/lein cljsbuild once

ENTRYPOINT ["/bin/bash"]
