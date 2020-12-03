FROM ubuntu:18.04
ARG DEBIAN_FRONTEND=noninteractive
WORKDIR /coco_platform
RUN apt-get update -y && \
  apt-get install -y \
  build-essential \
  python-dev \
  python-numpy \
  python-matplotlib \
  python-scipy \
  python-pandas \
  python-six \
  python-setuptools \
  git
ADD install.sh .
RUN ./install.sh
