FROM centos:7
ENV BAZEL_URL=https://copr.fedorainfracloud.org/coprs/vbatts/bazel/repo/epel-7/vbatts-bazel-epel-7.repo
RUN curl -sSL $BAZEL_URL -o /etc/yum.repos.d/vbatts-bazel-epel-7.repo && \
    yum update -y && \
    yum group install -y "Development Tools" && \
    yum install -y bazel2 python3 gmp-devel ncurses-devel zlib-devel && \
    yum clean all && \
    rm -rf /var/cache/yum

