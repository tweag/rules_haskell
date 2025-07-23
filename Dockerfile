# Use the official Ubuntu base image
FROM ubuntu:latest

# Update package lists and install necessary packages
RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y build-essential libffi-dev libgmp-dev libtinfo6 libtinfo-dev python3 openjdk-11-jdk && \
    apt-get install -y nix-bin && \
    apt-get install -y graphviz xdot

# Install bazelisk to handle a different bazel version
RUN apt install -y npm && \
    npm install -g @bazel/bazelisk

# Set the working directory inside the container
WORKDIR /home/rules_haskell

# Copy application files (if any)
COPY . /home/rules_haskell

# Run bazel using bazelisk to ensure the correct version is been used
RUN bazelisk --version

# Build tutorial
RUN cd tutorial && bazelisk build //...
