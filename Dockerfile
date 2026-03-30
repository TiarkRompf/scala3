FROM ubuntu:22.04

WORKDIR /scratch
RUN apt update && apt install -y build-essential curl openjdk-17-jdk-headless time && apt clean
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
RUN chmod +x cs
RUN ./cs setup -y --apps sbt:1.10.7,cs
RUN rm cs
RUN apt install -y python-is-python3 && apt clean
ADD scala3.tar.gz /work
WORKDIR /work/scala3
ENV PATH="/root/.local/share/coursier/bin:$PATH"
RUN cd /work/scala3 && sbt compile
